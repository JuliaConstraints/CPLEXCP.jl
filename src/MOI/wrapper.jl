# Inspired by CPLEX.jl.

@enum(
    VariableType,
    CONTINUOUS,
    BINARY,
    INTEGER,
    INTERVAL,
    SEQUENCEINTERVAL
)

_type_to_variabletype(T::Type{<:Real}) = ((T == Float64) ? CONTINUOUS : INTEGER)
_variabletype_to_type(vt::VariableType) = ((vt == CONTINUOUS) ? Float64 : Int)

# TODO.
@enum(
    CallbackState,
    CB_NONE
    # Others at some point.
)

mutable struct VariableInfo
    index::MOI.VariableIndex
    variable::Variable
    name::String
    type::VariableType

    # Variable bounds: either integers or floats. Needed for is_valid on SingleVariable 
    # and bound constraints.
    lb::Real
    ub::Real
    integer::Union{Nothing, Tuple{MOI.VariableIndex, MOI.ConstraintIndex{MOI.SingleVariable, MOI.Integer}, Constraint}}
    binary::Union{Nothing, Tuple{MOI.VariableIndex, MOI.ConstraintIndex{MOI.SingleVariable, MOI.ZeroOne}, Constraint}}

    # Bound-constraint names.
    lb_name::String
    ub_name::String
    interval_name::String
    equalto_name::String
    integer_name::String
    binary_name::String

    # For second-order cones, the variable must be constrained to be >= 0.
    # However, when removing all cones this variable participates in,
    # the previous lower bound must be restored.
    n_socs::Int
    old_lb::Union{Nothing, Real}
end

function VariableInfo(index::MOI.VariableIndex, variable::Variable) 
    return VariableInfo(
        index, variable, "", CONTINUOUS, 
        -IloInfinity, IloInfinity, nothing, nothing, 
        "", "", "", "", "", "", 
        0, nothing
    )
end

mutable struct ConstraintInfo
    # Only necessary information to access an existing constraint, delete it when needed.
    # No need to store more details, like subexpressions, to delete: the JVM's garbage collector
    # will take care of this.
    index::MOI.ConstraintIndex
    constraint::Constraint
    f::Union{MOI.AbstractScalarFunction, MOI.AbstractVectorFunction}
    set::MOI.AbstractSet
    name::String
end

function ConstraintInfo(index::MOI.ConstraintIndex, constraint::Constraint, 
                        f::Union{MOI.AbstractScalarFunction, MOI.AbstractVectorFunction}, 
                        set::MOI.AbstractSet)
    return ConstraintInfo(index, constraint, f, set, "")
end

mutable struct Optimizer <: MOI.AbstractOptimizer
    # The low-level CPLEX model.
    inner::JavaCPOModel

    # The model name.
    name::String

    # A flag to keep track of MOI.Silent, which overrides the OutputFlag
    # parameter.
    silent::Bool

    # A mapping from the MOI.VariableIndex to the CPLEX variable object.
    # VariableInfo also stores some additional fields like the type of variable.
    variable_info::CleverDicts.CleverDict{MOI.VariableIndex, VariableInfo}

    # A mapping from the MOI.ConstraintIndex to the CPLEX variable object.
    # VariableInfo also stores some additional fields like the type of variable.
    constraint_info::Dict{MOI.ConstraintIndex, ConstraintInfo}

    # Memorise the objective sense and the function separately, as the Concert
    # API forces to give both at the same time.
    objective_sense::MOI.OptimizationSense
    objective_function::Union{Nothing, MOI.AbstractScalarFunction}
    objective_function_cp::Union{Nothing, NumExpr}
    objective_cp::Union{Nothing, IloObjective}

    # Cache parts of a solution.
    cached_solution_state::Union{Nothing, Bool}

    # Handle callbacks. WIP.
    callback_state::CallbackState

    # Mappings from variable and constraint names to their indices. These are
    # lazily built on-demand, so most of the time, they are `nothing`.
    # The solver's functionality is not useful in this case, as it can only
    # handle integer variables. Moreover, bound constraints do not have names
    # for the solver.
    name_to_variable::Union{Nothing, Dict{String, MOI.VariableIndex}}
    name_to_constraint::Union{Nothing, Dict{String, MOI.ConstraintIndex}}

    """
        Optimizer()

    Create a new Optimizer object.
    """
    function Optimizer()
        model = new()
        model.inner = cpo_java_model()

        MOI.set(model, MOI.Silent(), false)

        model.variable_info = CleverDicts.CleverDict{MOI.VariableIndex, VariableInfo}()
        model.constraint_info = Dict{MOI.ConstraintIndex, ConstraintInfo}()

        model.objective_sense = MOI.FEASIBILITY_SENSE
        model.objective_function = nothing
        model.objective_function_cp = nothing
        model.objective_cp = nothing

        model.cached_solution_state = nothing
        model.callback_state = CB_NONE

        MOI.empty!(model)
        return model
    end
end

# TODO: ?
# Base.show(io::IO, model::Optimizer) = show(io, model.inner)

function MOI.empty!(model::Optimizer)
    model.inner = cpo_java_model()
    model.name = ""
    empty!(model.variable_info)
    empty!(model.constraint_info)

    model.objective_sense = MOI.FEASIBILITY_SENSE
    model.objective_function = nothing
    model.objective_function_cp = nothing
    model.objective_cp = nothing

    model.cached_solution_state = nothing
    model.name_to_variable = nothing
    model.name_to_constraint = nothing
    model.callback_state = CB_NONE
    return
end

function MOI.is_empty(model::Optimizer)
    !isempty(model.name) && return false
    !isempty(model.variable_info) && return false
    !isempty(model.constraint_info) && return false
    model.objective_sense != MOI.FEASIBILITY_SENSE && return false
    model.objective_function !== nothing && return false
    model.objective_function_cp !== nothing && return false
    model.objective_cp !== nothing && return false
    model.name_to_variable !== nothing && return false
    model.name_to_constraint !== nothing && return false
    model.cached_solution_state !== nothing && return false
    model.callback_state != CB_NONE && return false
    return true
end

MOI.get(::Optimizer, ::MOI.SolverName) = "CPLEX CP Optimizer"

## Types of objectives and constraints that are supported.
# TODO: everything CP.

function MOI.supports(::Optimizer, ::MOI.ObjectiveFunction{F}) where {F <: Union{
    MOI.SingleVariable,
    MOI.ScalarAffineFunction{Float64},
    MOI.ScalarQuadraticFunction{Float64}
}}
    return true
end

function MOI.supports_constraint(::Optimizer, ::Type{MOI.SingleVariable}, ::Type{F}) where {
        T <: Union{Int, Float64},
        F <: Union{
            MOI.EqualTo{T},
            MOI.LessThan{T},
            MOI.GreaterThan{T},
            MOI.Interval{T},
            MOI.ZeroOne,
            MOI.Integer
        }
    }
    return true
end

function MOI.supports_constraint(::Optimizer, 
                                 ::Union{Type{MOI.ScalarAffineFunction{T}}, Type{MOI.ScalarQuadraticFunction{T}}}, 
                                 ::Type{F}) where {
        T <: Union{Int, Float64},
        F <: Union{
            MOI.EqualTo{T},
            MOI.LessThan{T},
            MOI.GreaterThan{T},
            MOI.Interval{T}
        }
    }
    return true
end

MOI.supports(::Optimizer, ::MOI.VariableName, ::Type{MOI.VariableIndex}) = true
MOI.supports(::Optimizer, ::MOI.ConstraintName, ::Type{<:MOI.ConstraintIndex}) = true

MOI.supports(::Optimizer, ::MOI.Name) = true
MOI.supports(::Optimizer, ::MOI.Silent) = true
MOI.supports(::Optimizer, ::MOI.NumberOfThreads) = true
MOI.supports(::Optimizer, ::MOI.TimeLimitSec) = true
MOI.supports(::Optimizer, ::MOI.ObjectiveSense) = true
MOI.supports(::Optimizer, ::MOI.RawParameter) = true

# It is possible to use the default copy behaviour, including with names.
MOI.Utilities.supports_default_copy_to(::Optimizer, ::Bool) = true

function MOI.copy_to(dest::Optimizer, src::MOI.ModelLike; kwargs...)
    return MOI.Utilities.automatic_copy_to(dest, src; kwargs...)
end

function MOI.get(::Optimizer, ::MOI.ListOfVariableAttributesSet)
    return MOI.AbstractVariableAttribute[MOI.VariableName()]
end

function MOI.get(model::Optimizer, ::MOI.ListOfModelAttributesSet)
    attributes = Any[MOI.ObjectiveSense()]
    typ = MOI.get(model, MOI.ObjectiveFunctionType())
    if typ !== nothing
        push!(attributes, MOI.ObjectiveFunction{typ}())
    end
    if MOI.get(model, MOI.Name()) != ""
        push!(attributes, MOI.Name())
    end
    return attributes
end

function MOI.get(::Optimizer, ::MOI.ListOfConstraintAttributesSet)
    return MOI.AbstractConstraintAttribute[MOI.ConstraintName()]
end

## Optimize methods

function MOI.optimize!(model::Optimizer)
    model.cached_solution_state = cpo_java_solve(model.inner)
    return
end

function _throw_if_optimize_in_progress(model, attr)
    if model.callback_state != CB_NONE
        throw(MOI.OptimizeInProgress(attr))
    end
end

function MOI.get(model::Optimizer, attr::MOI.TerminationStatus)
    _throw_if_optimize_in_progress(model, attr)
    if model.cached_solution_state === nothing
        return MOI.OPTIMIZE_NOT_CALLED
    end

    if model.cached_solution_state
        return MOI.OPTIMAL
    else
        return MOI.INFEASIBLE
    end

    # TODO: exploit the attributes.

    # if stat == 1 # CPX_STAT_OPTIMAL
    #     return MOI.OPTIMAL
    # elseif stat == 3 # CPX_STAT_INFEASIBLE
    #     return MOI.INFEASIBLE
    # elseif stat == 4 # CPX_STAT_INForUNBD
    #     return MOI.INFEASIBLE_OR_UNBOUNDED
    # elseif stat == 2 # CPX_STAT_UNBOUNDED
    #     return MOI.DUAL_INFEASIBLE
    # elseif stat in (12, 21, 22, 36) # CPX_STAT_*ABORT*_OBJ_LIM
    #     return MOI.OBJECTIVE_LIMIT
    # elseif stat in (10, 34) # CPX_STAT_*ABORT_IT_LIM
    #     return MOI.ITERATION_LIMIT
    # elseif stat == 53 # CPX_STAT_CONFLICT_ABORT_NODE_LIM
    #     return MOI.NODE_LIMIT
    # elseif stat in (11, 25, 33, 39) # CPX_STAT_*ABORT*TIME_LIM
    #     return MOI.TIME_LIMIT
    # elseif stat == 5 # CPX_STAT_OPTIMAL_INFEAS
    #     return MOI.NUMERICAL_ERROR
    # # MIP STATUS
    # elseif stat in (101, 102) # CPXMIP_OPTIMAL, CPXMIP_OPTIMAL_TOL
    #     return MOI.OPTIMAL
    # elseif stat == 103 # CPXMIP_INFEASIBLE
    #     return MOI.INFEASIBLE
    # elseif stat == 119 # CPXMIP_INForUNBD
    #     return MOI.INFEASIBLE_OR_UNBOUNDED
    # elseif stat == 118 # CPXMIP_UNBOUNDED
    #     return MOI.DUAL_INFEASIBLE
    # elseif stat in (105, 106) # CPXMIP_NODE_LIM*
    #     return MOI.NODE_LIMIT
    # elseif stat in (107, 108, 131, 132) # CPXMIP_*TIME_LIM*
    #     return MOI.TIME_LIMIT
    # else
    #     return MOI.OTHER_ERROR
    # end
end

function MOI.get(model::Optimizer, attr::MOI.PrimalStatus)
    _throw_if_optimize_in_progress(model, attr)
    if model.cached_solution_state
        return MOI.FEASIBLE_POINT
    else
        return MOI.NO_SOLUTION
    end
end

function MOI.get(model::Optimizer, attr::MOI.VariablePrimal, x::MOI.VariableIndex)
    _throw_if_optimize_in_progress(model, attr)
    variable = _info(model, x).variable
    return cpo_java_getvalue(model.inner, variable)
end

function MOI.get(model::Optimizer, attr::MOI.ConstraintPrimal, c::MOI.ConstraintIndex{MOI.SingleVariable, <:Any})
    _throw_if_optimize_in_progress(model, attr)
    return MOI.get(model, MOI.VariablePrimal(), MOI.VariableIndex(c.value))
end

function MOI.get(model::Optimizer, attr::MOI.ConstraintPrimal, c::MOI.ConstraintIndex)
    _throw_if_optimize_in_progress(model, attr)
    constraint = _info(model, x).constraint # IloConstraint <: IloIntExpr
    return cpo_java_getvalue(model.inner, constraint)
end

# No dual values are available.

function MOI.get(model::Optimizer, attr::MOI.ObjectiveValue)
    _throw_if_optimize_in_progress(model, attr)
    return cpo_java_getobjvalue(model.inner)
end

function MOI.get(model::Optimizer, attr::MOI.ObjectiveBound)
    _throw_if_optimize_in_progress(model, attr)
    return cpo_java_getobjbound(model.inner)
end

function MOI.get(model::Optimizer, attr::MOI.SolveTime)
    _throw_if_optimize_in_progress(model, attr)
    return cpo_java_getdoubleparameter(model.inner, "SolveTime")
end

# No SimplexIterations or BarrierIterations.

function MOI.get(model::Optimizer, attr::MOI.NodeCount)
    _throw_if_optimize_in_progress(model, attr)
    return cpo_java_getintparameter(model.inner, "NumberOfBranches")
end

function MOI.get(model::Optimizer, attr::MOI.RelativeGap)
    _throw_if_optimize_in_progress(model, attr)
    MOI.check_result_index_bounds(model, attr)
    return cpo_java_getobjgap(model.inner)
end

# No DualObjectiveValue.

function MOI.get(model::Optimizer, ::MOI.Silent)
    return model.silent
end

function MOI.get(model::Optimizer, attr::MOI.ResultCount)
    _throw_if_optimize_in_progress(model, attr)
    return cpo_java_getintinfo(model.inner, "NumberOfSolutions")
end

function MOI.set(model::Optimizer, ::MOI.Silent, flag::Bool)
    model.silent = flag
    cpo_java_setintparameter(model.inner, "LogVerbosity", flag ? "Normal" : "Quiet")
    return
end

function MOI.get(model::Optimizer, ::MOI.NumberOfThreads)
    _throw_if_optimize_in_progress(model, attr)
    return cpo_java_getintparam(model.inner, "Workers")
end

function MOI.set(model::Optimizer, ::MOI.NumberOfThreads, x::Int)
    _throw_if_optimize_in_progress(model, attr)
    cpo_java_setintparameter(model.inner, "Workers", x)
    return
end

function MOI.get(model::Optimizer, ::MOI.TimeLimitSec)
    _throw_if_optimize_in_progress(model, attr)
    return cpo_java_getdoubleparam(model.inner, "TimeLimit")
end

function MOI.set(model::Optimizer, ::MOI.TimeLimitSec, x::Union{Number, Nothing})
    _throw_if_optimize_in_progress(model, attr)
    value = (x === nothing) ? IloInfinity : Float64(x)
    cpo_java_setdoubleparameter(model.inner, "TimeLimit", value)
    return
end

function MOI.get(model::Optimizer, param::MOI.RawParameter)
    _throw_if_optimize_in_progress(model, attr)

    if cpo_java_isparamint(model.inner, param.name)
        return cpo_java_getintparameter(model.inner, param.name)
    elseif cpo_java_isparamint(model.inner, param.name)
        return cpo_java_getdoubleparameter(model.inner, param.name)
    else
        error("Unknown parameter: $(param.name)")
    end
end

function MOI.set(model::Optimizer, param::MOI.RawParameter, x::Int)
    _throw_if_optimize_in_progress(model, attr)
    cpo_java_setintparameter(model.inner, param.name, x)
    return
end

function MOI.set(model::Optimizer, param::MOI.RawParameter, x::Float64)
    _throw_if_optimize_in_progress(model, attr)
    cpo_java_setdoubleparameter(model.inner, param.name, x)
    return
end

function MOI.get(model::Optimizer, ::MOI.Name)
    return model.name
end

function MOI.set(model::Optimizer, ::MOI.Name, name::String)
    model.name = name
    cpo_java_setname(model.inner, name)
    return
end

function MOI.get(model::Optimizer, ::MOI.NumberOfVariables)
    return length(model.variable_info)
end

function MOI.get(model::Optimizer, ::MOI.ListOfVariableIndices)
    return sort!(collect(keys(model.variable_info)), by = x -> x.value)
end

function MOI.get(model::Optimizer, ::MOI.RawSolver)
    return model.inner
end

# # TODO: initial values (also for constraints?). Use these values in `optimize!`?
# function MOI.set(model::Optimizer, ::MOI.VariablePrimalStart, x::MOI.VariableIndex, value::Union{Nothing, Float64})
#     info = _info(model, x)
#     info.start = value
#     return
# end
#
# function MOI.get(model::Optimizer, ::MOI.VariablePrimalStart, x::MOI.VariableIndex)
#     return _info(model, x).start
# end
#
# function MOI.supports(::Optimizer, ::MOI.VariablePrimalStart, ::Type{MOI.VariableIndex})
#     return true
# end

_type_enums(::Type{MOI.ZeroOne}) = (BINARY,)
_type_enums(::Type{MOI.Integer}) = (INTEGER,)
_type_enums(::Any) = (nothing,)

_check_bound_compatible(model::Optimizer, idx::MOI.VariableIndex, ::Type{<:MOI.LessThan{T}}) where T = _has_ub(model, idx)
_check_bound_compatible(model::Optimizer, idx::MOI.VariableIndex, ::Type{<:MOI.GreaterThan{T}}) where T = _has_lb(model, idx)
_check_bound_compatible(model::Optimizer, idx::MOI.VariableIndex, ::Type{<:MOI.Interval{T}}) where T = _has_lb(model, idx) && _has_ub(model, idx)
_check_bound_compatible(model::Optimizer, idx::MOI.VariableIndex, ::Type{<:MOI.EqualTo{T}}) where T = _get_lb(model, idx) == _get_ub(model, idx)
_check_bound_compatible(model::Optimizer, idx::MOI.VariableIndex, ::Any) = false

function MOI.get(model::Optimizer, ::MOI.NumberOfConstraints{MOI.SingleVariable, S}) where {S}
    n = 0
    for (key, info) in model.variable_info
        if _check_bound_compatible(model, key, S) || info.type in _type_enums(S)
            n += 1
        end

        if S == MOI.ZeroOne && info.binary !== nothing
            n += 1
        end
        if S == MOI.Integer && info.integer !== nothing
            n += 1
        end
    end
    return n
end

function MOI.get(model::Optimizer, ::MOI.NumberOfConstraints{F, S}) where {F, S}
    n = 0
    for (key, info) in model.constraint_info
        if typeof(info.set) == S
            n += 1
        end
    end
    return n
end

function MOI.get(model::Optimizer, ::MOI.ListOfConstraintIndices{MOI.SingleVariable, S}) where {S}
    indices = MOI.ConstraintIndex{MOI.SingleVariable, S}[]
    for (key, info) in model.variable_info
        if _check_bound_compatible(model, key, S) || info.type in _type_enums(S)
            push!(indices, MOI.ConstraintIndex{MOI.SingleVariable, S}(key.value))
        end

        if S == MOI.ZeroOne && info.binary !== nothing
            push!(indices, MOI.ConstraintIndex{MOI.SingleVariable, S}(key.value))
        end
        if S == MOI.Integer && info.integer !== nothing
            push!(indices, MOI.ConstraintIndex{MOI.SingleVariable, S}(key.value))
        end
    end
    return sort!(indices, by = x -> x.value)
end

function MOI.get(model::Optimizer, ::MOI.ListOfConstraintIndices{F, S}) where {F, S}
    indices = MOI.ConstraintIndex{F, S}[]
    for (key, info) in model.constraint_info
        if typeof(info.set) == S
            push!(indices, MOI.ConstraintIndex{F, S}(key.value))
        end
    end
    return sort!(indices, by = x -> x.value)
end

# No SOS1/SOS2.

function MOI.get(model::Optimizer, ::MOI.ListOfConstraints)
    constraints = Set{Tuple{DataType, DataType}}()

    for info in values(model.variable_info)
        if _get_lb(model, info.index) == _get_ub(model, info.index)
            push!(constraints, (MOI.SingleVariable, MOI.EqualTo{typeof(_get_lb(model, info.index))}))
        elseif _has_lb(model, idx) && _has_ub(model, idx)
            push!(constraints, (MOI.SingleVariable, MOI.Interval{typeof(_get_lb(model, info.index))}))
        elseif _has_ub(model, idx)
            push!(constraints, (MOI.SingleVariable, MOI.LessThan{typeof(_get_lb(model, info.index))}))
        elseif _has_lb(model, idx)
            push!(constraints, (MOI.SingleVariable, MOI.GreaterThan{typeof(_get_lb(model, info.index))}))
        end
        if info.type == CONTINUOUS
        elseif info.type == BINARY
            push!(constraints, (MOI.SingleVariable, MOI.ZeroOne))
        elseif info.type == INTEGER
            push!(constraints, (MOI.SingleVariable, MOI.Integer))
        end
    end

    for (key, info) in model.constraint_info
        push!(constraints, (info.f, info.set))
    end

    return collect(constraints)
end

function MOI.get(model::Optimizer, ::MOI.ObjectiveFunctionType)
    if model.objective_sense == MOI.FEASIBILITY_SENSE
        return nothing
    else
        return typeof(model.objective_function)
    end
end

# TODO: modify the objective and the constraints.

# ConstraintBasisStatus makes no sense.
