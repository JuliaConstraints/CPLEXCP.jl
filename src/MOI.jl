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

    # Variable bounds: either integers or floats. Needed for is_valid on SingleVariable and bound constraints.
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

    VariableInfo(index::MOI.VariableIndex, variable::Variable) = new(index, variable, "", CONTINUOUS, -IloInfinity, IloInfinity, nothing, nothing, "", "", "", "", "", "", 0, nothing)
end

mutable struct ConstraintInfo
    index::MOI.ConstraintIndex
    constraint::Constraint
    f::MOI.AbstractScalarFunction
    set::MOI.AbstractSet
    name::String

    ConstraintInfo(index::MOI.ConstraintIndex, constraint::Constraint, f::MOI.AbstractScalarFunction, set::MOI.AbstractSet) = new(index, constraint, f, set, "")
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
    # if model.silent
    #     MOI.set(model, MOI.RawParameter("CPXPARAM_ScreenOutput"), 0)
    # end
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

function MOI.supports_constraint(::Optimizer, ::Type{MOI.SingleVariable}, ::Type{F}) where {F <: Union{
    MOI.EqualTo{Float64},
    MOI.LessThan{Float64},
    MOI.GreaterThan{Float64},
    MOI.Interval{Float64},
    MOI.EqualTo{Int}, # TODO: Int (platform-dependent) or Int64?
    MOI.LessThan{Int},
    MOI.GreaterThan{Int},
    MOI.Interval{Int},
    MOI.ZeroOne,
    MOI.Integer
}}
    return true
end

function MOI.supports_constraint(::Optimizer, ::Type{MOI.ScalarAffineFunction{Float64}}, ::Type{F}) where {F <: Union{
    MOI.EqualTo{Float64},
    MOI.LessThan{Float64},
    MOI.GreaterThan{Float64},
    MOI.Interval{Float64},
    MOI.EqualTo{Int},
    MOI.LessThan{Int},
    MOI.GreaterThan{Int},
    MOI.Interval{Int}
}}
    return true
end

function MOI.supports_constraint(::Optimizer, ::Type{MOI.ScalarQuadraticFunction{Float64}}, ::Type{F}) where {F <: Union{
    MOI.EqualTo{Float64},
    MOI.LessThan{Float64},
    MOI.GreaterThan{Float64},
    MOI.Interval{Float64},
    MOI.EqualTo{Int},
    MOI.LessThan{Int},
    MOI.GreaterThan{Int},
    MOI.Interval{Int}
}}
    return true
end

MOI.supports(::Optimizer, ::MOI.VariableName, ::Type{MOI.VariableIndex}) = true
MOI.supports(::Optimizer, ::MOI.ConstraintName, ::Type{<:MOI.ConstraintIndex}) = true

MOI.supports(::Optimizer, ::MOI.Name) = true
MOI.supports(::Optimizer, ::MOI.Silent) = true
MOI.supports(::Optimizer, ::MOI.NumberOfThreads) = true
MOI.supports(::Optimizer, ::MOI.TimeLimitSec) = true # TODO
MOI.supports(::Optimizer, ::MOI.ObjectiveSense) = true
MOI.supports(::Optimizer, ::MOI.RawParameter) = true # TODO

# It is possible to use the default copy behaviour, including with names.
MOI.Utilities.supports_default_copy_to(::Optimizer, ::Bool) = true

function MOI.copy_to(dest::Optimizer, src::MOI.ModelLike; kwargs...)
    return MOI.Utilities.automatic_copy_to(dest, src; kwargs...)
end

function MOI.get(model::Optimizer, ::MOI.ListOfVariableAttributesSet)
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

function MOI.get(model::Optimizer, ::MOI.ListOfConstraintAttributesSet)
    return MOI.AbstractConstraintAttribute[MOI.ConstraintName()]
end

## Variables

function _info(model::Optimizer, key::MOI.VariableIndex)
    if haskey(model.variable_info, key)
        return model.variable_info[key]
    end
    throw(MOI.InvalidIndex(key))
end

function _make_var(model::Optimizer, variable::Variable)
    # Initialize `VariableInfo` with a dummy `VariableIndex` and a column,
    # because we need `add_item` to tell us what the `VariableIndex` is.
    index = CleverDicts.add_item(model.variable_info, VariableInfo(MOI.VariableIndex(0), variable))
    _info(model, index).index = index
    return index
end

function _make_var(model::Optimizer, variable::Variable, set::MOI.AbstractScalarSet)
    index = _make_var(model, variable)
    return index, MOI.ConstraintIndex{MOI.SingleVariable, typeof(set)}(index.value)
end

function _make_vars(model::Optimizer, variables::Vector{<:Variable})
    # Barely used, because add_constrained_variables may have variable sets (except for AbstractVectorSet).
    # Only implemented in the unconstrained case.
    indices = Vector{MOI.VariableIndex}(undef, length(variables))
    for i in 1:length(variables)
        indices[i] = CleverDicts.add_item(model.variable_info, VariableInfo(MOI.VariableIndex(0), variables[i]))
        _info(model, indices[i]).index = indices[i]
    end
    return indices
end

function _make_numvar(model::Optimizer, set::MOI.AbstractScalarSet; lb::Float64=-IloInfinity, ub::Float64=IloInfinity)
    return _make_var(model, cpo_java_numvar(model.inner, lb, ub), set)
end

function _make_intvar(model::Optimizer, set::MOI.AbstractScalarSet; lb::Int=-IloMinInt, ub::Int=IloMaxInt)
    return _make_var(model, cpo_java_intvar(model.inner, lb, ub), set)
end

function _make_boolvar(model::Optimizer, set::MOI.AbstractScalarSet)
    return _make_var(model, cpo_java_boolvar(model.inner), set)
end

function supports_add_constrained_variables(::Optimizer, ::Type{F}) where {F <: Union{
    MOI.EqualTo{Float64},
    MOI.LessThan{Float64},
    MOI.GreaterThan{Float64},
    MOI.Interval{Float64},
    MOI.EqualTo{Int},
    MOI.LessThan{Int},
    MOI.GreaterThan{Int},
    MOI.Interval{Int},
    MOI.ZeroOne,
    MOI.Integer
}}
    return true
end

function MOI.add_variable(model::Optimizer)
    return _make_var(model, cpo_java_numvar(model.inner, -IloInfinity, IloInfinity))
end

function MOI.add_variables(model::Optimizer, N::Int)
    return _make_vars(model, cpo_java_numvararray(model.inner, N, -IloInfinity, IloInfinity))
end

function MOI.add_constrained_variable(model::Optimizer, set::MOI.GreaterThan{T}) where {T <: Real}
    return _make_numvar(model, set, lb=set.lower)
end

function MOI.add_constrained_variable(model::Optimizer, set::MOI.LessThan{T}) where {T <: Real}
    return _make_numvar(model, set, lb=set.upper)
end

function MOI.add_constrained_variable(model::Optimizer, set::MOI.EqualTo{T}) where {T <: Real}
    return _make_numvar(model, set, lb=set.value, ub=set.value)
end

function MOI.add_constrained_variable(model::Optimizer, set::MOI.Interval{T}) where {T <: Real}
    return _make_numvar(model, set, lb=set.lower, ub=set.upper)
end

function MOI.add_constrained_variable(model::Optimizer, set::MOI.GreaterThan{T}) where {T <: Integer}
    return _make_intvar(model, set, lb=set.lower)
end

function MOI.add_constrained_variable(model::Optimizer, set::MOI.LessThan{T}) where {T <: Integer}
    return _make_intvar(model, set, lb=set.upper)
end

function MOI.add_constrained_variable(model::Optimizer, set::MOI.EqualTo{T}) where {T <: Integer}
    return _make_intvar(model, set, lb=set.value, ub=set.value)
end

function MOI.add_constrained_variable(model::Optimizer, set::MOI.Interval{T}) where {T <: Integer}
    return _make_intvar(model, set, lb=set.lower, ub=set.upper)
end

function MOI.add_constrained_variable(model::Optimizer, set::MOI.ZeroOne)
    return _make_boolvar(model, set)
end

function MOI.add_constrained_variable(model::Optimizer, set::MOI.Integer)
    return _make_intvar(model, set)
end

function MOI.is_valid(model::Optimizer, v::MOI.VariableIndex)
    return haskey(model.variable_info, v)
end

function MOI.delete(model::Optimizer, v::MOI.VariableIndex)
    # Actual deletion of the variable.
    info = _info(model, v)
    if info.n_socs > 0
        throw(MOI.DeleteNotAllowed(v))
    end
    cpo_java_remove(model.inner, info.variable)
    delete!(model.variable_info, v)

    # We throw away name_to_constraint so we will rebuild SingleVariable
    # constraint names without v.
    model.name_to_variable = nothing
    model.name_to_constraint = nothing

    return
end

function MOI.get(model::Optimizer, ::Type{MOI.VariableIndex}, name::String)
    if model.name_to_variable === nothing
        _rebuild_name_to_variable(model)
    end
    return get(model.name_to_variable, name, nothing)
end

function _rebuild_name_to_variable(model::Optimizer)
    model.name_to_variable = Dict{String, MOI.VariableIndex}()
    for (index, info) in model.variable_info
        if info.name == ""
            continue
        end

        if haskey(model.name_to_variable, info.name)
            error("Duplicate variable name detected: $(info.name)")
        end
        model.name_to_variable[info.name] = index
    end
    return
end

function MOI.get(model::Optimizer, ::MOI.VariableName, v::MOI.VariableIndex)
    return _info(model, v).name
end

function MOI.set(model::Optimizer, ::MOI.VariableName, v::MOI.VariableIndex, name::String)
    info = _info(model, v)
    info.name = name
    cpo_java_addable_setname(model.inner, info.variable, name)

    model.name_to_variable = nothing
    model.name_to_constraint = nothing

    return
end

## Expression parsing (not part of MOI API)

function _parse(model::Optimizer, expr)
    error("_parse not yet implemented for type: $(typeof(expr))")
end

function _parse(model::Optimizer, f::MOI.SingleVariable)
    # A Concert Variable is already an expression.
    return _info(model, f.variable).variable
end

function _parse(model::Optimizer, f::MOI.VariableIndex)
    return _info(model, f).variable
end

function _parse(model::Optimizer, terms::Vector{MOI.ScalarAffineTerm{T}}) where {T <: Real}
    cp = model.inner
    coeffs = T[t.coefficient for t in terms]
    vars = NumVar[_parse(model, t.variable_index) for t in terms]
    return cpo_java_scalprod(cp, coeffs, vars)
end

function _parse(model::Optimizer, terms::Vector{MOI.ScalarQuadraticTerm{T}}) where {T <: Real}
    # IloCP.scalprod only works for unweighted vectors of variables, which is not always the case here.
    cp = model.inner

    prod_vars(term::MOI.ScalarQuadraticTerm{T}) =
        cpo_java_prod(cp, _parse(model, term.variable_index_1), _parse(model, term.variable_index_2))
    prod(term::MOI.ScalarQuadraticTerm{T}) =
        cpo_java_prod(cp, term.coefficient, prod_vars(term))

    e = prod(terms[1])
    for t in terms[2:end]
        e = cpo_java_sum(cp, e, prod(t))
    end
    return e
end

function _parse(model::Optimizer, f::MOI.ScalarAffineFunction{T}) where {T <: Real}
    f = MOI.Utilities.canonical(f)
    e = _parse(model, f.terms)
    if !iszero(f.constant)
        cp = model.inner
        e = cpo_java_sum(cp, e, cpo_java_constant(cp, f.constant))
    end
    return e
end

function _parse(model::Optimizer, f::MOI.ScalarQuadraticFunction{T}) where {T <: Real}
    f = MOI.Utilities.canonical(f)
    cp = model.inner
    e = _parse(model, f.quadratic_terms)
    if length(f.affine_terms) > 0
        e = cpo_java_sum(cp, e, _parse(model, f.affine_terms))
    end
    if !iszero(f.constant)
        e = cpo_java_sum(cp, e, cpo_java_constant(cp, f.constant))
    end
    return e
end

function _parse(model::Optimizer, f::MOI.VectorOfVariables, s::MOI.SecondOrderCone)
    # SOC is the cone: t ≥ ||x||₂ ≥ 0. In quadratic form, this is
    # t² - Σᵢ xᵢ² ≥ 0 and t ≥ 0.
    # This function returns the expression t² - Σᵢ xᵢ².
    vars = [_info(model, v) for v in f.variables]
    e = cpo_java_square(model, vars[1].variable)
    for i in 2:length(vars)
        e = cpo_java_diff(model, e, vars[i].variable)
    end
    return e
end

## Objective
# TODO: what about @objective(m, Max, count(x .== 1))? Automatically add a constraint (i.e. bridge)? And/or support the constraint as a function?
# TODO: -> rather all constraints, more consistent with how other solvers work.

function _update_objective(model::Optimizer)
    # If the sense is feasibility and there is an internal Concert objective, remove it.
    # Otherwise, this is an optimisation problem.
    if model.objective_sense == MOI.FEASIBILITY_SENSE && model.objective_cp !== nothing
        cpo_java_remove(model.inner, model.objective_cp)
        model.objective_cp = nothing
    end

    # If only no function is available, don't do anything.
    if model.objective_function_cp === nothing
        return
    end

    # Set the new objective.
    obj = if model.objective_sense == MOI.MIN_SENSE
        cpo_java_minimize(model.inner, model.objective_function_cp)
    else
        cpo_java_maximize(model.inner, model.objective_function_cp)
    end
    cpo_java_add(model.inner, obj)
end

function MOI.set(model::Optimizer, ::MOI.ObjectiveSense, sense::MOI.OptimizationSense)
    model.objective_sense = sense
    _update_objective(model)
    return
end

function MOI.get(model::Optimizer, ::MOI.ObjectiveSense)
    return model.objective_sense
end

function MOI.set(model::Optimizer, ::MOI.ObjectiveFunction{F}, f::F) where {F <: MOI.SingleVariable}
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        convert(MOI.ScalarAffineFunction{Float64}, f)
    )
    model.objective_type = MOI.SINGLE_VARIABLE
    return
end

function MOI.get(model::Optimizer, ::MOI.ObjectiveFunction{F}) where {F <: MOI.AbstractScalarFunction}
    if typeof(model.objective_function) <: F
        return model.objective_function
    else
        error("Unable to get objective function. Current objective: $(model.objective_function).")
    end
end

function MOI.set(model::Optimizer, ::MOI.ObjectiveFunction{F}, f::F) where {F <: MOI.AbstractScalarFunction}
    model.objective_function = f
    model.objective_function_cp = _parse(model, f)
    _update_objective(model)
    return
end

# TODO: modifications. Easy to do, as we have a pointer on the Concert expression! Hard to do, as the MOI function must be rebuilt.
# function MOI.modify(
#     model::Optimizer,
#     ::MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}},
#     chg::MOI.ScalarConstantChange{Float64}
# )
#     CPLEX.c_api_chgobjoffset(model.inner, chg.new_constant)
#     return
# end

## SingleVariable-in-set

_get_lb(model::Optimizer, index::MOI.VariableIndex, ::Type{Float64}) =
    cpo_java_numvar_getlb(model.inner, _info(model, index).variable)
_get_lb(model::Optimizer, index::MOI.VariableIndex, ::Type{Int}) =
    cpo_java_intvar_getlb(model.inner, _info(model, index).variable)

_get_ub(model::Optimizer, index::MOI.VariableIndex, ::Type{Float64}) =
    cpo_java_numvar_getub(model.inner, _info(model, index).variable)
_get_ub(model::Optimizer, index::MOI.VariableIndex, ::Type{Int}) =
    cpo_java_intvar_getub(model.inner, _info(model, index).variable)

_has_lb(model::Optimizer, index::MOI.VariableIndex, ::Type{Float64}) =
    _get_lb(model, index, Float64) != -IloInfinity
_has_lb(model::Optimizer, index::MOI.VariableIndex, ::Type{Int}) =
    _get_lb(model, index, Int) != IloMinInt

_has_ub(model::Optimizer, index::MOI.VariableIndex, ::Type{Float64}) =
    _get_ub(model, index, Float64) != IloInfinity
_has_ub(model::Optimizer, index::MOI.VariableIndex, ::Type{Int}) =
    _get_ub(model, index, Int) != IloMaxInt

function _bounds_to_set(model::Optimizer, index::MOI.VariableIndex, ::Type{T}) where {T <: Real}
    if _has_lb(model, index, T)
        if _has_ub(model, index, T)
            if _info(model, index).ub == _info(model, index).ub
                return MOI.EqualTo{T}
            else
                return MOI.Interval{T}
            end
        else
            return MOI.GreaterThan{T}
        end
    else
        if _has_ub(model, index, T)
            return MOI.LessThan{T}
        else
            # No bounds set!
            return nothing
        end
    end
end

_assert_no_lb(model::Optimizer, i::MOI.VariableIndex, ::Type{T}) where {T <: Real} =
    _has_lb(model, i, T) && throw(MOI.LowerBoundAlreadySet{_bounds_to_set(i, T), s}(i))
_assert_no_ub(model::Optimizer, i::MOI.VariableIndex, ::Type{T}) where {T <: Real} =
    _has_ub(model, i, T) && throw(MOI.UpperBoundAlreadySet{_bounds_to_set(i, T), s}(i))

function _set_lb(model::Optimizer, index::MOI.VariableIndex, lb::Float64, ::Type{Float64})
    info = _info(model, index)
    info.lb = lb
    cpo_java_numvar_setlb(model.inner, info.variable, lb)
end
function _set_ub(model::Optimizer, index::MOI.VariableIndex, ub::Float64, ::Type{Float64})
    info = _info(model, index)
    info.ub = ub
    cpo_java_numvar_setub(model.inner, info.variable, ub)
end

function MOI.is_valid(model::Optimizer, c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.LessThan{T}}) where {T <: Real}
    index = MOI.VariableIndex(c.value)
    return MOI.is_valid(model, index) && _has_ub(model, index, T)
end

function MOI.is_valid(model::Optimizer, c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.GreaterThan{T}}) where {T <: Real}
    index = MOI.VariableIndex(c.value)
    return MOI.is_valid(model, index) && _has_lb(model, index, T)
end

function MOI.is_valid(model::Optimizer, c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.Interval{T}}) where {T <: Real}
    index = MOI.VariableIndex(c.value)
    return MOI.is_valid(model, index) && _has_lb(model, index, T) && _has_ub(model, index, T)
end

function MOI.is_valid(model::Optimizer, c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.EqualTo{T}}) where {T <: Real}
    index = MOI.VariableIndex(c.value)
    return MOI.is_valid(model, index) && _get_lb(model, index, T) == _get_ub(model, index, T)
end

function MOI.is_valid(model::Optimizer, c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.ZeroOne})
    if !MOI.is_valid(model, MOI.VariableIndex(c.value))
        return false
    end

    index = MOI.VariableIndex(c.value)
    info = _info(model, index)
    return info.type == BINARY || info.binary !== nothing
end

function MOI.is_valid(model::Optimizer, c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.Integer})
    if !MOI.is_valid(model, MOI.VariableIndex(c.value))
        return false
    end

    index = MOI.VariableIndex(c.value)
    info = _info(model, index)
    return info.type == INTEGER || info.integer !== nothing
end

function MOI.get(model::Optimizer, ::MOI.ConstraintFunction, c::MOI.ConstraintIndex{MOI.SingleVariable, <:Any})
    MOI.throw_if_not_valid(model, c)
    return MOI.SingleVariable(MOI.VariableIndex(c.value))
end

function MOI.set(model::Optimizer, ::MOI.ConstraintFunction, c::MOI.ConstraintIndex{MOI.SingleVariable, S}, v::MOI.SingleVariable) where {S}
    throw(MOI.SettingSingleVariableFunctionNotAllowed())
end

function MOI.set(model::Optimizer, ::MOI.ConstraintSet, c::MOI.ConstraintIndex{MOI.SingleVariable, S}, s::S) where {S}
    if S <: MOI.Interval
        _set_ub(model, MOI.VariableIndex(c.value), s.upper, typeof(s.upper))
        _set_lb(model, MOI.VariableIndex(c.value), s.lower, typeof(s.lower))
    elseif S <: MOI.EqualTo
        _set_ub(model, MOI.VariableIndex(c.value), s.value, typeof(s.value))
        _set_lb(model, MOI.VariableIndex(c.value), s.value, typeof(s.value))
    elseif S <: MOI.GreaterThan
        _set_lb(model, MOI.VariableIndex(c.value), s.lower, typeof(s.lower))
    elseif S <: MOI.LessThan
        _set_ub(model, MOI.VariableIndex(c.value), s.upper, typeof(s.upper))
    end
    return
end

function MOI.add_constraint(model::Optimizer, f::MOI.SingleVariable, s::MOI.LessThan{T}) where {T <: Real}
    @assert _info(model, f.variable).type == _type_to_variabletype(T)
    _assert_no_ub(model, f.variable, T)
    _set_ub(model, f.variable, s.upper, T)
    return MOI.ConstraintIndex{MOI.SingleVariable, typeof(s)}(f.variable.value)
end

function MOI.add_constraint(model::Optimizer, f::MOI.SingleVariable, s::MOI.GreaterThan{T}) where {T <: Real}
    @assert _info(model, f.variable).type == _type_to_variabletype(T)
    _assert_no_lb(model, f.variable, T)
    _set_lb(model, f.variable, s.lower, T)
    return MOI.ConstraintIndex{MOI.SingleVariable, typeof(s)}(f.variable.value)
end

function MOI.add_constraint(model::Optimizer, f::MOI.SingleVariable, s::MOI.EqualTo{T}) where {T <: Real}
    @assert _info(model, f.variable).type == _type_to_variabletype(T)
    _assert_no_lb(model, f.variable, T)
    _assert_no_ub(model, f.variable, T)
    _set_lb(model, f.variable, s.value, T)
    _set_ub(model, f.variable, s.value, T)
    return MOI.ConstraintIndex{MOI.SingleVariable, typeof(s)}(f.variable.value)
end

function MOI.add_constraint(model::Optimizer, f::MOI.SingleVariable, s::MOI.Interval{T}) where {T <: Real}
    @assert _info(model, f.variable).type == _type_to_variabletype(T)
    _assert_no_lb(model, f.variable, T)
    _assert_no_ub(model, f.variable, T)
    _set_lb(model, f.variable, s.lower, T)
    _set_ub(model, f.variable, s.upper, T)
    return MOI.ConstraintIndex{MOI.SingleVariable, typeof(s)}(f.variable.value)
end

function MOI.add_constraint(model::Optimizer, f::MOI.SingleVariable, s::MOI.ZeroOne)
    info = _info(model, f.variable)
    @assert info.type == CONTINUOUS || info.type == INTEGER
    @assert info.binary === nothing

    # Bad trick: create a binary variable, impose equality. Don't show this variable to MOI, though.
    bindex, cindex = MOI.add_constrained_variable(model, MOI.ZeroOne())
    eqcstr = cpo_java_eq(model.inner, info.variable, _info(model, bindex).variable)
    cpo_java_add(model.inner, eqcstr)
    info.binary = (bindex, cindex, eqcstr)

    return MOI.ConstraintIndex{MOI.SingleVariable, MOI.ZeroOne}(f.variable.value)
end

function MOI.add_constraint(model::Optimizer, f::MOI.SingleVariable, s::MOI.Integer)
    info = _info(model, f.variable)
    @assert info.type == CONTINUOUS
    @assert info.integer === nothing

    # Bad trick: create an integer variable, impose equality. Don't show this variable to MOI, though.
    bindex, cindex = MOI.add_constrained_variable(model, MOI.Integer())
    eqcstr = cpo_java_eq(model.inner, info.variable, _info(model, bindex).variable)
    cpo_java_add(model.inner, eqcstr)
    info.integer = (bindex, cindex, eqcstr)

    return MOI.ConstraintIndex{MOI.SingleVariable, MOI.ZeroOne}(f.variable.value)
end

# No similar function for a vector of variables, no way to do it more efficiently.

function MOI.delete(model::Optimizer, c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.LessThan{T}}) where {T <: Real}
    MOI.throw_if_not_valid(model, c)
    _set_ub(model, MOI.VariableIndex(c.value), (T == Float64) ? IloInfinity : IloMaxInt, T)
    _info(model, MOI.VariableIndex(c.value)).ub_name = ""
    model.name_to_constraint = nothing
    return
end

function MOI.delete(model::Optimizer, c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.GreaterThan{T}}) where {T <: Real}
    MOI.throw_if_not_valid(model, c)
    _set_lb(model, MOI.VariableIndex(c.value), (T == Float64) ? -IloInfinity : IloMinInt, T)
    _info(model, MOI.VariableIndex(c.value)).lb_name = ""
    model.name_to_constraint = nothing
    return
end

function MOI.delete(model::Optimizer, c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.EqualTo{T}}) where {T <: Real}
    MOI.throw_if_not_valid(model, c)
    _set_ub(model, MOI.VariableIndex(c.value), (T == Float64) ? IloInfinity : IloMaxInt, T)
    _set_lb(model, MOI.VariableIndex(c.value), (T == Float64) ? -IloInfinity : IloMinInt, T)
    _info(model, MOI.VariableIndex(c.value)).equalto_name = ""
    model.name_to_constraint = nothing
    return
end

function MOI.delete(model::Optimizer, c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.Interval{T}}) where {T <: Real}
    MOI.throw_if_not_valid(model, c)
    _set_ub(model, MOI.VariableIndex(c.value), (T == Float64) ? IloInfinity : IloMaxInt, T)
    _set_lb(model, MOI.VariableIndex(c.value), (T == Float64) ? -IloInfinity : IloMinInt, T)
    _info(model, MOI.VariableIndex(c.value)).interval_name = ""
    model.name_to_constraint = nothing
    return
end

function MOI.delete(model::Optimizer, c::MOI.ConstraintIndex{MOI.SingleVariable, S}) where {S <: Union{MOI.ZeroOne, MOI.Integer}}
    MOI.throw_if_not_valid(model, c)
    info = _info(model, MOI.VariableIndex(c.value))
    @assert info.type == CONTINUOUS || info.type == INTEGER
    @assert info.binary !== nothing

    if S == MOI.ZeroOne
        cpo_java_remove(model.inner, info.binary[3])
        info.binary = nothing
    else
        cpo_java_remove(model.inner, info.integer[3])
        info.integer = nothing
    end

    return
end

function MOI.get(model::Optimizer, ::MOI.ConstraintSet, c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.GreaterThan{T}}) where {T <: Real}
    MOI.throw_if_not_valid(model, c)
    return MOI.GreaterThan(_get_lb(model, MOI.VariableIndex(c.value), T))
end

function MOI.get(model::Optimizer, ::MOI.ConstraintSet, c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.LessThan{T}}) where {T <: Real}
    MOI.throw_if_not_valid(model, c)
    return MOI.LessThan(_get_ub(model, MOI.VariableIndex(c.value), T))
end

function MOI.get(model::Optimizer, ::MOI.ConstraintSet, c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.EqualTo{T}}) where {T <: Real}
    MOI.throw_if_not_valid(model, c)
    return MOI.EqualTo(_get_ub(model, MOI.VariableIndex(c.value), T))
end

function MOI.get(model::Optimizer, ::MOI.ConstraintSet, c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.Interval{T}}) where {T <: Real}
    MOI.throw_if_not_valid(model, c)
    return MOI.Interval(_get_lb(model, MOI.VariableIndex(c.value), T), _get_ub(model, MOI.VariableIndex(c.value), T))
end

function MOI.get(model::Optimizer, ::MOI.ConstraintSet, c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.ZeroOne})
    MOI.throw_if_not_valid(model, c)
    return MOI.ZeroOne()
end

function MOI.get(model::Optimizer, ::MOI.ConstraintSet, c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.Integer})
    MOI.throw_if_not_valid(model, c)
    return MOI.Integer()
end

function MOI.get(model::Optimizer, ::MOI.ConstraintSet, c::MOI.ConstraintIndex{F, S}) where {F, S}
    return _info(model, c).set
end

# TODO: MOI.set? Remove the constraint and rebuild it?
# TODO: make variable integer/binary?
# TODO: constraint names here? They don't get passed to the solver.

## Constraint names

function MOI.get(model::Optimizer, ::MOI.ConstraintName, c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.LessThan{T}}) where {T <: Real}
    return _info(model, MOI.VariableIndex(c.value)).ub_name
end

function MOI.get(model::Optimizer, ::MOI.ConstraintName, c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.GreaterThan{T}}) where {T <: Real}
    return _info(model, MOI.VariableIndex(c.value)).lb_name
end

function MOI.get(model::Optimizer, ::MOI.ConstraintName, c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.Interval{T}}) where {T <: Real}
    return _info(model, MOI.VariableIndex(c.value)).interval_name
end

function MOI.get(model::Optimizer, ::MOI.ConstraintName, c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.EqualTo{T}}) where {T <: Real}
    return _info(model, MOI.VariableIndex(c.value)).equalto_name
end

function MOI.get(model::Optimizer, ::MOI.ConstraintName, c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.Integer})
    return _info(model, MOI.VariableIndex(c.value)).integer_name
end

function MOI.get(model::Optimizer, ::MOI.ConstraintName, c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.ZeroOne})
    return _info(model, MOI.VariableIndex(c.value)).binary_name
end

function MOI.get(model::Optimizer, ::MOI.ConstraintName, c::MOI.ConstraintIndex)
    return _info(model, c).name
end

function MOI.set(model::Optimizer, ::MOI.ConstraintName, c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.LessThan{T}}, name::String) where {T <: Real}
    _info(model, MOI.VariableIndex(c.value)).ub_name = name
    model.name_to_constraint = nothing
    return
end

function MOI.set(model::Optimizer, ::MOI.ConstraintName, c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.GreaterThan{T}}, name::String) where {T <: Real}
    _info(model, MOI.VariableIndex(c.value)).lb_name = name
    model.name_to_constraint = nothing
    return
end

function MOI.set(model::Optimizer, ::MOI.ConstraintName, c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.Interval{T}}, name::String) where {T <: Real}
    _info(model, MOI.VariableIndex(c.value)).interval_name = name
    model.name_to_constraint = nothing
    return
end

function MOI.set(model::Optimizer, ::MOI.ConstraintName, c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.EqualTo{T}}, name::String) where {T <: Real}
    _info(model, MOI.VariableIndex(c.value)).equalto_name = name
    model.name_to_constraint = nothing
    return
end

function MOI.set(model::Optimizer, ::MOI.ConstraintName, c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.Integer}, name::String)
    _info(model, MOI.VariableIndex(c.value)).integer_name = name
    model.name_to_constraint = nothing
    return
end

function MOI.set(model::Optimizer, ::MOI.ConstraintName, c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.ZeroOne}, name::String)
    _info(model, MOI.VariableIndex(c.value)).binary_name = name
    model.name_to_constraint = nothing
    return
end

function MOI.set(model::Optimizer, ::MOI.ConstraintName, c::MOI.ConstraintIndex, name::String)
    info = _info(model, c)
    info.name = name
    cpo_java_addable_setname(model.inner, info.constraint, name)
    model.name_to_constraint = nothing
    return
end

function MOI.get(model::Optimizer, ::Type{MOI.ConstraintIndex}, name::String)
    if model.name_to_constraint === nothing
        _rebuild_name_to_constraint(model)
    end
    return get(model.name_to_constraint, name, nothing)
end

function MOI.get(
    model::Optimizer, C::Type{MOI.ConstraintIndex{F, S}}, name::String
) where {F, S}
    index = MOI.get(model, MOI.ConstraintIndex, name)
    if typeof(index) == C
        return index::MOI.ConstraintIndex{F, S}
    end
    return nothing
end

function _rebuild_name_to_constraint_add!(model::Optimizer, name::String, cindex::MOI.ConstraintIndex)
    if name == ""
        return
    end

    if haskey(model.name_to_constraint, name)
        error("Duplicate variable name detected: $(name)")
    end
    model.name_to_constraint[name] = cindex
end

function _rebuild_name_to_constraint(model::Optimizer)
    model.name_to_constraint = Dict{String, MOI.ConstraintIndex}()

    # Variable bounds.
    for (index, info) in model.variable_info
        T = _variabletype_to_type(info.type)

        if _check_bound_compatible(model, index, MOI.EqualTo{T})
            _rebuild_name_to_constraint_add!(model, info.equalto_name, MOI.ConstraintIndex{MOI.SingleVariable, MOI.EqualTo{T}}(index.value))
        end
        if _check_bound_compatible(model, index, MOI.Interval{T})
            _rebuild_name_to_constraint_add!(model, info.interval_name, MOI.ConstraintIndex{MOI.SingleVariable, MOI.Interval{T}}(index.value))
        end
        if _check_bound_compatible(model, index, MOI.LessThan{T})
            _rebuild_name_to_constraint_add!(model, info.ub_name, MOI.ConstraintIndex{MOI.SingleVariable, MOI.LessThan{T}}(index.value))
        end
        if _check_bound_compatible(model, index, MOI.GreaterThan{T})
            _rebuild_name_to_constraint_add!(model, info.lb_name, MOI.ConstraintIndex{MOI.SingleVariable, MOI.GreaterThan{T}}(index.value))
        end

        if info.type == INTEGER || info.integer !== nothing
            _rebuild_name_to_constraint_add!(model, info.integer_name, index)
        elseif info.type == BINARY || info.binary !== nothing
            _rebuild_name_to_constraint_add!(model, info.binary_name, index)
        end
    end

    # Other constraints.
    for (index, info) in model.constraint_info
        _rebuild_name_to_constraint_add!(model, info.name, MOI.ConstraintIndex{typeof(info.f), typeof(info.set)}(index.value))
    end

    return
end

## ScalarAffineFunction-in-Set
## ScalarQuadraticFunction-in-Set

function _info(model::Optimizer, key::MOI.ConstraintIndex{MOI.SingleVariable, <:Any})
    throw(MOI.InvalidIndex(key))
end

function _info(model::Optimizer, key::MOI.ConstraintIndex)
    if haskey(model.constraint_info, key)
        return model.constraint_info[key]
    end
    throw(MOI.InvalidIndex(key))
end

function MOI.is_valid(model::Optimizer, c::MOI.ConstraintIndex{F, S}) where {S, T <: Real, F <: Union{MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T}}}
    info = get(model.constraint_info, c, nothing)
    return info !== nothing && typeof(info.set) == S
end

function MOI.add_constraint(model::Optimizer, f::F, s::MOI.GreaterThan{T}) where {T <: Real, F <: Union{MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T}}}
    index = MOI.ConstraintIndex{typeof(f), typeof(s)}(length(model.constraint_info) + 1)
    constr = cpo_java_ge(model.inner, _parse(model, f), s.lower)
    cpo_java_add(model.inner, constr)
    model.constraint_info[index] = ConstraintInfo(index, constr, f, s)
    return index
end

function MOI.add_constraint(model::Optimizer, f::F, s::MOI.LessThan{T}) where {T <: Real, F <: Union{MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T}}}
    index = MOI.ConstraintIndex{typeof(f), typeof(s)}(length(model.constraint_info) + 1)
    constr = cpo_java_le(model.inner, _parse(model, f), s.upper)
    cpo_java_add(model.inner, constr)
    model.constraint_info[index] = ConstraintInfo(index, constr, f, s)
    return index
end

function MOI.add_constraint(model::Optimizer, f::F, s::MOI.EqualTo{T}) where {T <: Real, F <: Union{MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T}}}
    index = MOI.ConstraintIndex{typeof(f), typeof(s)}(length(model.constraint_info) + 1)
    constr = cpo_java_eq(model.inner, _parse(model, f), s.value)
    cpo_java_add(model.inner, constr)
    model.constraint_info[index] = ConstraintInfo(index, constr, f, s)
    return index
end

function MOI.add_constraint(model::Optimizer, f::F, s::MOI.Interval{T}) where {T <: Real, F <: Union{MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T}}}
    if s.lower == Inf
        return MOI.add_constraint(model, f, MOI.LessThan(s.upper))
    elseif s.upper == Inf
        return MOI.add_constraint(model, f, MOI.GreaterThan(s.lower))
    elseif s.lower == s.upper
        return MOI.add_constraint(model, f, MOI.EqualTo(s.lower))
    end

    index = MOI.ConstraintIndex{typeof(f), typeof(s)}(length(model.constraint_info) + 1)
    constr = cpo_java_range(model.inner, s.lower, _parse(model, f), s.upper)
    cpo_java_add(model.inner, constr)
    model.constraint_info[index] = ConstraintInfo(index, constr, f, s)
    return index
end

# No vector of constraints, there is no more efficient way to do it.

function MOI.delete(model::Optimizer, c::MOI.ConstraintIndex{F, S}) where {T <: Real, F <: Union{MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T}}, S}
    cpo_java_remove(model.inner, _info(model, c).constraint)
    delete!(model.constraint_info, c)
    return
end

# TODO: function MOI.set(model::Optimizer, ::MOI.ConstraintSet, c::MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64}, S}, s::S) where {S}

function MOI.get(model::Optimizer, ::MOI.ConstraintFunction, c::MOI.ConstraintIndex{F, S}) where {S, F}
    return _info(model, c).f
end

## VectorOfVariables-in-SOS{I|II}
# Not available.

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
    MOI.check_result_index_bounds(model, attr)
    variable = _info(model, x).variable
    return cpo_java_getvalue(model.inner, variable)
end

function MOI.get(model::Optimizer, attr::MOI.ConstraintPrimal, c::MOI.ConstraintIndex{MOI.SingleVariable, <:Any})
    _throw_if_optimize_in_progress(model, attr)
    MOI.check_result_index_bounds(model, attr)
    return MOI.get(model, MOI.VariablePrimal(), MOI.VariableIndex(c.value))
end

function MOI.get(model::Optimizer, attr::MOI.ConstraintPrimal, c::MOI.ConstraintIndex)
    _throw_if_optimize_in_progress(model, attr)
    MOI.check_result_index_bounds(model, attr)
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

# TODO
# function MOI.get(model::Optimizer, attr::MOI.SolveTime)
#     _throw_if_optimize_in_progress(model, attr)
#     # https://www.ibm.com/support/knowledgecenter/SSSA5P_12.10.0/ilog.odms.cpo.help/refjavacpoptimizer/html/ilog/cp/IloCP.DoubleInfo.html#SolveTime
# end

# No SimplexIterations or BarrierIterations.

# TODO
# function MOI.get(model::Optimizer, attr::MOI.NodeCount)
#     _throw_if_optimize_in_progress(model, attr)
#     # https://www.ibm.com/support/knowledgecenter/SSSA5P_12.10.0/ilog.odms.cpo.help/refjavacpoptimizer/html/ilog/cp/IloCP.IntInfo.html#NumberOfBranches
# end

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

# # TODO: initial values (also for constraints?). Use these values in optimize!?
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

_check_bound_compatible(model::Optimizer, idx::MOI.VariableIndex, ::Type{<:MOI.LessThan{T}}) where T = _has_ub(model, idx, T)
_check_bound_compatible(model::Optimizer, idx::MOI.VariableIndex, ::Type{<:MOI.GreaterThan{T}}) where T = _has_lb(model, idx, T)
_check_bound_compatible(model::Optimizer, idx::MOI.VariableIndex, ::Type{<:MOI.Interval{T}}) where T = _has_lb(model, idx, T) && _has_ub(model, idx, T)
_check_bound_compatible(model::Optimizer, idx::MOI.VariableIndex, ::Type{<:MOI.EqualTo{T}}) where T = _get_lb(model, idx, T) == _get_ub(model, idx, T)
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
# TODO: SOCP.

function MOI.get(model::Optimizer, ::MOI.ListOfConstraints)
    constraints = Set{Tuple{DataType, DataType}}()

    for info in values(model.variable_info)
        if _get_lb(model, info.index, T) == _get_ub(model, info.index, T)
            push!(constraints, (MOI.SingleVariable, MOI.EqualTo{typeof(_get_lb(model, info.index, T))}))
        elseif _has_lb(model, idx, T) && _has_ub(model, idx, T)
            push!(constraints, (MOI.SingleVariable, MOI.Interval{typeof(_get_lb(model, info.index, T))}))
        elseif _has_ub(model, idx, T)
            push!(constraints, (MOI.SingleVariable, MOI.LessThan{typeof(_get_lb(model, info.index, T))}))
        elseif _has_lb(model, idx, T)
            push!(constraints, (MOI.SingleVariable, MOI.GreaterThan{typeof(_get_lb(model, info.index, T))}))
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

## VectorOfVariables-in-SecondOrderCone

function MOI.add_constraint(model::Optimizer, f::MOI.VectorOfVariables, s::MOI.SecondOrderCone)
    if length(f.variables) != s.dimension
        error("Dimension of $(s) does not match number of terms in $(f)")
    end

    # First, check the lower bound on t.
    t_info = _info(model, f.variables[1])
    if !_has_lb(model, t_info.index, Float64) || _get_lb(model, t_info.index, Float64) < 0.0
        if _get_lb(model, t_info.index, Float64) < 0.0
            t_info.old_lb = _get_lb(model, t_info.index, Float64)
        end

        t_info.n_socs += 1
        _set_lb(model, t_info.index, 0.0, Float64)
    end

    # Then, add the quadratic constraint.
    cindex = MOI.ConstraintIndex{MOI.VectorOfVariables, MOI.SecondOrderCone}(length(model.constraint_info) + 1)
    expr = _parse(f, s)
    constr = cpo_java_gt(model, expr, 0)
    model.constraint_info[cindex] = ConstraintInfo(cindex, constr, f, s)
    return cindex
end

function MOI.is_valid(model::Optimizer, c::MOI.ConstraintIndex{MOI.VectorOfVariables, MOI.SecondOrderCone})
    info = get(model.constraint_info, c.value, nothing)
    return info !== nothing && typeof(info.set) == MOI.SecondOrderCone
end

function MOI.delete(model::Optimizer, c::MOI.ConstraintIndex{MOI.VectorOfVariables, MOI.SecondOrderCone})
    # Remove the constraint.
    cpo_java_remove(model, _info(model, c).constraint)

    # Maybe restore the old bound on t.
    t_info = _info(model, f.variables[1])
    t_info.n_socs -= 1

    if t_info.n_socs == 0
        _set_lb(model, t_info.index, t_info.old_lb, typeof(t_info.old_lb))
        t_info.old_lb = nothing
    end

    return
end

function MOI.get(model::Optimizer, ::MOI.ConstraintSet, c::MOI.ConstraintIndex{MOI.VectorOfVariables, MOI.SecondOrderCone})
    return _info(model, c).set
end

# ConstraintFunction, ConstraintPrimal, and ConstraintName handled in the most generic case.
