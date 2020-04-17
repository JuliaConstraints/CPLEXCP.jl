# Inspired by CPLEX.jl.

@enum(
    VariableType,
    CONTINUOUS,
    BINARY,
    INTEGER,
    # SEMIINTEGER, # TODO: Only for IloMPModeler; useful to support? Must be implemented from scratch...
    # SEMICONTINUOUS, # TODO: Only for IloMPModeler; useful to support? Must be implemented from scratch...
    INTERVAL,
    SEQUENCEINTERVAL
)

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

    VariableInfo(index::MOI.VariableIndex, variable::Variable) = new(index, variable, "", CONTINUOUS, -IloInfinity, IloInfinity)
end

mutable struct ConstraintInfo
    index::MOI.ConstraintIndex
    constraint::Constraint
    f::MOI.AbstractScalarFunction
    set::MOI.AbstractSet
    name::String

    ConstraintInfo(index::MOI.ConstraintIndex, constraint::Constraint, set::MOI.AbstractSet) = new(index, constraint, set, "")
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
    objective_sense::OptimizationSense
    objective_function::Union{Nothing, MOI.AbstractScalarFunction}
    objective_function_cp::Union{Nothing, NumExpr}
    objective_cp::Union{Nothing, IloObjective}

    # Cache parts of a solution.
    cached_solution_state::Union{Nothing, Bool}

    # Handle callbacks. WIP.
    callback_state::CallbackState

    # # Mappings from variable and constraint names to their indices. These are
    # # lazily built on-demand, so most of the time, they are `nothing`.
    # name_to_variable::Union{Nothing, Dict{String, Union{Nothing, MOI.VariableIndex}}}
    # name_to_constraint_index::Union{Nothing, Dict{String, Union{Nothing, MOI.ConstraintIndex}}}
    # # TODO: Or rather use the solver's functionalities?
    # # TODO: For SingleVariable-in-Anything, constraint names are never passed to the solver (handled as bounds on the variable).

    """
        Optimizer()

    Create a new Optimizer object.
    """
    function Optimizer()
        model = new()
        model.inner = cpo_java_model()

        # TODO: set the solver to silent.
        # MOI.set(model, MOI.RawParameter("CPXPARAM_ScreenOutput"), 1)
        model.silent = false

        model.variable_info = CleverDicts.CleverDict{MOI.VariableIndex, VariableInfo}()
        model.constraint_info = CleverDicts.CleverDict{MOI.ConstraintIndex, VariableInfo}()

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
    model.cached_solution_state !== nothing && return false
    model.callback_state != CB_NONE && return false
    return true
end

MOI.get(::Optimizer, ::MOI.SolverName) = "CPLEX CP Optimizer"

## Types of objectives and constraints that are supported.
# TODO: everything CP.

function MOI.supports(
    ::Optimizer,
    ::MOI.ObjectiveFunction{F}
) where {F <: Union{
    MOI.SingleVariable,
    MOI.ScalarAffineFunction{Float64},
    MOI.ScalarQuadraticFunction{Float64}
}}
    return true
end

function MOI.supports_constraint(
    ::Optimizer, ::Type{MOI.SingleVariable}, ::Type{F}
) where {F <: Union{
    MOI.EqualTo{Float64},
    MOI.LessThan{Float64},
    MOI.GreaterThan{Float64},
    MOI.Interval{Float64},
    MOI.EqualTo{Int}, # TODO: Int (platform-dependent) or Int64?
    MOI.LessThan{Int},
    MOI.GreaterThan{Int},
    MOI.Interval{Int}
    # No ZeroOne or Integer, this is supposed to be done when creating a variable.
}}
    return true
end

function MOI.supports_constraint(
    ::Optimizer, ::Type{MOI.ScalarAffineFunction{Float64}}, ::Type{F}
) where {F <: Union{
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

function MOI.supports_constraint(
    ::Optimizer, ::Type{MOI.ScalarQuadraticFunction{Float64}}, ::Type{F}
) where {F <: Union{
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

# TODO: supports attributes (not implemented in lower-level API for now).

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
# Don't implement add_variable[s]: this does not give enough information to
# call the underlying API.

function _info(model::Optimizer, key::MOI.VariableIndex)
    if haskey(model.variable_info, key)
        return model.variable_info[key]
    end
    throw(MOI.InvalidIndex(key))
end

function _make_var(model::Optimizer, variable::Variable, set::MOI.AbstractScalarSet)
    # Initialize `VariableInfo` with a dummy `VariableIndex` and a column,
    # because we need `add_item` to tell us what the `VariableIndex` is.
    index = CleverDicts.add_item(model.variable_info, VariableInfo(MOI.VariableIndex(0), 0))
    info = _info(model, index)
    info.index = index
    info.variable = variable
    return index, MOI.ConstraintIndex{MOI.SingleVariable, typeof(set)}(index)
end

function _make_vars(model::Optimizer, variables::Vector{Variable}, sets::Vector{<:MOI.AbstractScalarSet})
    # Unused, because add_constrained_variables may have variable sets (except for AbstractVectorSet).
    indices = Vector{MOI.VariableIndex}(undef, length(sets))
    cindices = Vector{MOI.ConstraintIndex}(undef, length(sets))
    for i in 1:length(sets)
        indices[i], cindices[i] = _make_var(model, variables[i], sets[i])
    end
    return indices, cindices
end

function _make_numvar(model::Optimizer, set::AbstractScalarSet; lb::Float64=-IloInfinity, ub::Float64=IloInfinity)
    return _make_var(model, cpo_java_numvar(model.inner, lb, ub))
end

function _make_intvar(model::Optimizer, set::AbstractScalarSet; lb::Int=-IloMinInt, ub::Int=IloMaxInt)
    return _make_var(model, cpo_java_intvar(model.inner, lb, ub))
end

function _make_boolvar(model::Optimizer, set::AbstractScalarSet)
    return _make_var(model, cpo_java_boolvar(model.inner))
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

# TODO: how to implement delete()?
# TODO: implement getting variable from name? MOI.get(model::Optimizer, ::Type{MOI.VariableIndex}, name::String)

function MOI.get(model::Optimizer, ::MOI.VariableName, v::MOI.VariableIndex)
    return _info(model, v).name
end

function MOI.set(
    model::Optimizer, ::MOI.VariableName, v::MOI.VariableIndex, name::String
)
    info = _info(model, v)
    info.name = name
    cpo_java_addable_setname(model.inner.cp, info.variable, name)
    # model.name_to_variable = nothing
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
    cp = model.inner.cp
    coeffs = T[t.coefficient for t in terms.terms]
    vars = NumVar[_parse(t.variable_index) for t in terms.terms]
    return cpo_java_scalprod(cp, coeffs, vars)
end

function _parse(model::Optimizer, terms::Vector{MOI.ScalarQuadraticTerm{T}}) where {T <: Real}
    # IloCP.scalprod only works for unweighted vectors of variables, which is not always the case here.
    cp = model.inner.cp

    prod_vars(term::MOI.ScalarQuadraticTerm{T}) where {T <: Integer} =
        cpo_java_prod(cp, _parse(term.variable_index_1), _parse(term.variable_index_2))
    prod(term::MOI.ScalarQuadraticTerm{T}) where {T <: Integer} =
        cpo_java_prod(cp, term.coefficient, prod_vars(term))

    e = prod(terms[1])
    for t in terms[2:end]
        e = cpo_java_sum(cp, e, prod(t))
    end
    return e
end

function _parse(model::Optimizer, f::MOI.ScalarAffineFunction{T}) where {T <: Integer}
    f = MOI.Utilities.canonical(f)
    e = _parse(model, f.terms)
    if !iszero(f.constant)
        cp = model.inner.cp
        e = cpo_java_sum(cp, e, cpo_java_constant(cp, f.constant))
    end
    return e
end

function _parse(model::Optimizer, f::MOI.ScalarQuadraticFunction{T}) where {T <: Integer}
    f = MOI.Utilities.canonical(f)
    cp = model.inner.cp
    e = _parse(model, f.quadratic_terms)
    if length(f.affine_terms) > 0
        e = cpo_java_sum(cp, e, _parse(model, f.affine_terms))
    end
    if !iszero(f.constant)
        e = cpo_java_sum(cp, e, cpo_java_constant(cp, f.constant))
    end
end

## Objective
# TODO: what about @objective(m, Max, count(x .== 1))? Automatically add a constraint (i.e. bridge)? And/or support the constraint as a function?
# TODO: -> rather all constraints, more consistent with how other solvers work.

function _update_objective(model::Optimizer)
    # If the sense is feasibility and there is an internal Concert objective, remove it.
    # Otherwise, this is an optimisation problem.
    if model.objective_sense == MOI.FEASIBILITY_SENSE && model.objective_cp !== nothing
        cpo_java_remove(model.inner.cp, model.objective_cp)
        model.objective_cp = nothing
    end

    # If only no function is available, don't do anything.
    if model.objective_function_cp === nothing
        return
    end

    # Set the new objective.
    if model.objective_sense == MOI.MIN_SENSE
        cpo_java_minimize(model.inner.cp, model.objective_function_cp)
    else
        cpo_java_maximize(model.inner.cp, model.objective_function_cp)
    end
end

function MOI.set(model::Optimizer, ::MOI.ObjectiveSense, sense::MOI.OptimizationSense)
    model.objective_sense = sense
    _update_objective(model)
    return
end

function MOI.get(model::Optimizer, ::MOI.ObjectiveSense)
    return model.objective_sense
end

function MOI.set(
    model::Optimizer, ::MOI.ObjectiveFunction{F}, f::F
) where {F <: MOI.SingleVariable}
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        convert(MOI.ScalarAffineFunction{Float64}, f)
    )
    model.objective_type = SINGLE_VARIABLE
    return
end

function MOI.get(model::Optimizer, ::MOI.ObjectiveFunction{F}) where {F <: AbstractScalarFunction}
    if model.objective_function <: F
        return model.objective_function
    else
        error("Unable to get objective function. Current objective: $(model.objective_function).")
    end
end

function MOI.set(model::Optimizer, ::MOI.ObjectiveFunction{F}, f::F) where {F <: AbstractScalarFunction}
    model.objective_function = f
    model.objective_function_cp = _parse(f)
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

_get_lb(model::Optimizer, index::VariableIndex, ::Type{Float64}) =
    cpo_java_numvar_getlb(model.inner.cp, _info(model, index))
_get_lb(model::Optimizer, index::VariableIndex, ::Type{Int}) =
    cpo_java_intvar_getlb(model.inner.cp, _info(model, index))

_get_ub(model::Optimizer, index::VariableIndex, ::Type{Float64}) =
    cpo_java_numvar_getub(model.inner.cp, _info(model, index))
_get_ub(model::Optimizer, index::VariableIndex, ::Type{Int}) =
    cpo_java_intvar_getub(model.inner.cp, _info(model, index))

_has_lb(model::Optimizer, index::VariableIndex, ::Type{Float64}) =
    _get_lb(model, index, Float64) != -IloInfinity
_has_lb(model::Optimizer, index::VariableIndex, ::Type{Int}) =
    _get_lb(model, index, Int) != IloMinInt

_has_ub(model::Optimizer, index::VariableIndex, ::Type{Float64}) =
    _get_ub(model, index, Float64) != IloInfinity
_has_ub(model::Optimizer, index::VariableIndex, ::Type{Int}) =
    _get_ub(model, index, Int) != IloMaxInt

function _bounds_to_set(model::Optimizer, index::VariableIndex, ::Type{T}) where {T <: Real}
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

_assert_no_lb(model::Optimizer, i::VariableIndex, ::Type{T}) where {T <: Real} =
    _has_lb(model, i, T) && throw(MOI.LowerBoundAlreadySet{_bounds_to_set(i, T), s}(i))
_assert_no_ub(model::Optimizer, i::VariableIndex, ::Type{T}) where {T <: Real} =
    _has_ub(model, i, T) && throw(MOI.UpperBoundAlreadySet{_bounds_to_set(i, T), s}(i))

function _set_lb(model::Optimizer, index::MOI.VariableIndex, lb::Float64, ::Type{Float64})
    info = _info(model, index)
    info.lb = lb
    cpo_java_numvar_setlb(model.inner.cp, info.variable, lb)
end
function _set_ub(model::Optimizer, index::MOI.VariableIndex, ub::Float64, ::Type{Float64})
    info = _info(model, index)
    info.ub = ub
    cpo_java_numvar_setub(model.inner.cp, info.variable, ub)
end

function MOI.is_valid(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.LessThan{T}}
) where {T <: Real}
    return MOI.is_valid(MOI.VariableIndex(c.value)) && _has_ub(model, c.variable, T)
end

function MOI.is_valid(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.GreaterThan{T}}
) where {T <: Real}
    return MOI.is_valid(MOI.VariableIndex(c.value)) && _has_lb(model, c.variable, T)
end

function MOI.is_valid(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.Interval{T}}
) where {T <: Real}
    return MOI.is_valid(MOI.VariableIndex(c.value)) && _has_lb(model, c.variable, T) && _has_ub(model, c.variable, T)
end

function MOI.is_valid(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.EqualTo{T}}
) where {T <: Real}
    return MOI.is_valid(MOI.VariableIndex(c.value)) && _get_lb(model, c.variable, T) == _get_ub(model, c.variable, T)
end

function MOI.is_valid(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.ZeroOne}
) where {T <: Real}
    return MOI.is_valid(MOI.VariableIndex(c.value)) && _info(model, c).type == BINARY
end

function MOI.is_valid(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.Integer}
) where {T <: Real}
    return MOI.is_valid(MOI.VariableIndex(c.value)) && _info(model, c).type == INTEGER
end

function MOI.get(
    model::Optimizer, ::MOI.ConstraintFunction,
    c::MOI.ConstraintIndex{MOI.SingleVariable, <:Any}
)
    MOI.throw_if_not_valid(model, c)
    return MOI.SingleVariable(MOI.VariableIndex(c.value))
end

function MOI.set(
    model::Optimizer, ::MOI.ConstraintFunction,
    c::MOI.ConstraintIndex{MOI.SingleVariable, <:Any}, ::MOI.SingleVariable
)
    return throw(MOI.SettingSingleVariableFunctionNotAllowed())
end

function MOI.add_constraint(
    model::Optimizer, f::MOI.SingleVariable, s::MOI.LessThan{T}
) where {T <: Real}
    @assert _info(model, f.variable).type == (T == Float64) ? CONTINUOUS : INTEGER
    _assert_no_ub(model, f.variable, T)
    _set_ub(model, f.variable, s.upper, T)
end

function MOI.add_constraint(
    model::Optimizer, f::MOI.SingleVariable, s::MOI.GreaterThan{T}
) where {T <: Real}
    @assert _info(model, f.variable).type == (T == Float64) ? CONTINUOUS : INTEGER
    _assert_no_lb(model, f.variable, T)
    _set_lb(model, f.variable, s.lower, T)
end

function MOI.add_constraint(
    model::Optimizer, f::MOI.SingleVariable, s::MOI.EqualTo{T}
) where {T <: Real}
    @assert _info(model, f.variable).type == (T == Float64) ? CONTINUOUS : INTEGER
    _assert_no_lb(model, f.variable, T)
    _assert_no_ub(model, f.variable, T)
    _set_lb(model, f.variable, s.value, T)
    _set_ub(model, f.variable, s.value, T)
end

function MOI.add_constraint(
    model::Optimizer, f::MOI.SingleVariable, s::MOI.Interval{T}
) where {T <: Real}
    @assert _info(model, f.variable).type == (T == Float64) ? CONTINUOUS : INTEGER
    _assert_no_lb(model, f.variable, T)
    _assert_no_ub(model, f.variable, T)
    _set_lb(model, f.variable, s.lower, T)
    _set_ub(model, f.variable, s.upper, T)
end

# No similar function for a vector of variables, no way to do it more efficiently.

function MOI.delete(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.LessThan{T}}
) where {T <: Real}
    MOI.throw_if_not_valid(model, c)
    _set_ub(model, f.variable, (T == Float64) ? IloInfinity : IloMaxInt), T)
end

function MOI.delete(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.GreaterThan{T}}
) where {T <: Real}
    MOI.throw_if_not_valid(model, c)
    _set_ub(model, f.variable, (T == Float64) ? IloInfinity : IloMaxInt), T)
    _set_lb(model, f.variable, (T == Float64) ? -IloInfinity : IloMinInt), T)
end

function MOI.delete(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.EqualTo{T}}
) where {T <: Real}
    MOI.throw_if_not_valid(model, c)
    _set_ub(model, f.variable, (T == Float64) ? IloInfinity : IloMaxInt), T)
    _set_lb(model, f.variable, (T == Float64) ? -IloInfinity : IloMinInt), T)
end

function MOI.delete(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.Interval{T}}
) where {T <: Real}
    MOI.throw_if_not_valid(model, c)
    _set_ub(model, f.variable, (T == Float64) ? IloInfinity : IloMaxInt), T)
    _set_lb(model, f.variable, (T == Float64) ? -IloInfinity : IloMinInt), T)
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.GreaterThan{T}}
) where {T <: Real}
    MOI.throw_if_not_valid(model, c)
    return MOI.GreaterThan(_get_lb(model, c.value))
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.LessThan{T}}
) where {T <: Real}
    MOI.throw_if_not_valid(model, c)
    return MOI.LessThan(_get_ub(model, c.value))
end

function MOI.get(
    model::Optimizer, ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.EqualTo{T}}
) where {T <: Real}
    MOI.throw_if_not_valid(model, c)
    return MOI.EqualTo(_get_ub(model, c.value))
end

function MOI.get(
    model::Optimizer, ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.Interval{T}}
) where {T <: Real}
    MOI.throw_if_not_valid(model, c)
    return MOI.Interval(_get_lb(model, c.value), _get_ub(model, c.value))
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.ZeroOne}
)
    MOI.throw_if_not_valid(model, c)
    return MOI.ZeroOne()
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{MOI.SingleVariable, MOI.Integer}
)
    MOI.throw_if_not_valid(model, c)
    return MOI.Integer()
end

# TODO: MOI.set?
# TODO: make variable integer/binary?
# TODO: constraint names here? They don't get passed to the solver.

## Constraint names

function MOI.get(model::Optimizer, ::MOI.ConstraintName, c::MOI.ConstraintIndex)
    return _info(model, c).name
end

function MOI.set(model::Optimizer, ::MOI.ConstraintName, c::MOI.ConstraintIndex, name::String)
    info = _info(model, c)
    info.name = name
    cpo_java_addable_setname(model.inner.cp, info.constraint, name)
    return
end

# TODO: function MOI.get(model::Optimizer, ::Type{MOI.ConstraintIndex}, name::String)
# TODO: function MOI.get(model::Optimizer, C::Type{MOI.ConstraintIndex{F, S}}, name::String)

## ScalarAffineFunction-in-Set
## ScalarQuadraticFunction-in-Set

function _info(
    model::Optimizer,
    key::MOI.ConstraintIndex
)
    if haskey(model.constraint_info, key.value)
        return model.constraint_info[key.value]
    end
    throw(MOI.InvalidIndex(key))
end

function MOI.is_valid(
    model::Optimizer,
    c::MOI.ConstraintIndex{F, S}
) where {S, T <: Real, F <: Union{MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T}}}
    info = get(model.constraint_info, c.value, nothing)
    if info === nothing
        return false
    else
        return typeof(info.set) == S
    end
end

function MOI.add_constraint(model::Optimizer, f::F, s::MOI.GreaterThan{T})
where {T <: Real, F <: Union{MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T}}}
    index = MOI.ConstraintIndex{typeof(f), typeof(s)}(length(model.constraint_info) + 1)
    constr = cpo_java_gt(model.inner.cp, _parse(model, f), zero(T))
    model.constraint_info[index] = ConstraintInfo(index, constr, s)
    return index
end

function MOI.add_constraint(model::Optimizer, f::F, s::MOI.LessThan{T})
where {T <: Real, F <: Union{MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T}}}
    index = MOI.ConstraintIndex{typeof(f), typeof(s)}(length(model.constraint_info) + 1)
    constr = cpo_java_lt(model.inner.cp, _parse(model, f), zero(T))
    model.constraint_info[index] = ConstraintInfo(index, constr, s)
    return index
end

function MOI.add_constraint(model::Optimizer, f::F}, s::MOI.EqualTo{T})
where {T <: Real, F <: Union{MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T}}}
    index = MOI.ConstraintIndex{typeof(f), typeof(s)}(length(model.constraint_info) + 1)
    constr = cpo_java_eq(model.inner.cp, _parse(model, f), zero(T))
    model.constraint_info[index] = ConstraintInfo(index, constr, s)
    return index
end

# No vector of constraints, there is no more efficient way to do it.

function MOI.delete(model::Optimizer, c::MOI.ConstraintIndex{F})
where {T <: Real, F <: Union{MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T}}}
    cpo_java_remove(model.inner.cp, _info(c).constraint)
    delete!(model.constraint_info, c)
    return
end

function MOI.get(model::Optimizer, ::MOI.ConstraintSet, c::MOI.ConstraintIndex{F, S})
where {S, T <: Real, F <: Union{MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T}}}
    return _info(c).set
end

# TODO: function MOI.set(model::Optimizer, ::MOI.ConstraintSet, c::MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64}, S}, s::S) where {S}

function MOI.get(model::Optimizer, ::MOI.ConstraintFunction, c::MOI.ConstraintIndex{F, S})
where {S, T <: Real, F <: Union{MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T}}}
    return _info(c).f
end

## VectorOfVariables-in-SOS{I|II}
# Not available.

## Optimize methods

function MOI.optimize!(model::Optimizer)
    model.cached_solution_state = cpo_java_solve(model.inner.cp)
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
    return cpo_java_getvalue(model.inner.cp, variable)
end

function MOI.get(
    model::Optimizer, attr::MOI.ConstraintPrimal,
    c::MOI.ConstraintIndex{MOI.SingleVariable, <:Any}
)
    _throw_if_optimize_in_progress(model, attr)
    MOI.check_result_index_bounds(model, attr)
    return MOI.get(model, MOI.VariablePrimal(), MOI.VariableIndex(c.value))
end

function MOI.get(model::Optimizer, attr::MOI.ConstraintPrimal, c::MOI.ConstraintIndex)
    _throw_if_optimize_in_progress(model, attr)
    MOI.check_result_index_bounds(model, attr)
    constraint = _info(model, x).constraint # IloConstraint <: IloIntExpr
    return cpo_java_getvalue(model.inner.cp, constraint)
end

# No dual values are available.

function MOI.get(model::Optimizer, attr::MOI.ObjectiveValue)
    _throw_if_optimize_in_progress(model, attr)
    MOI.check_result_index_bounds(model, attr)
    return cpo_java_getobjvalue(model.inner.cp)
end

function MOI.get(model::Optimizer, attr::MOI.ObjectiveBound)
    _throw_if_optimize_in_progress(model, attr)
    MOI.check_result_index_bounds(model, attr)
    return cpo_java_getobjbound(model.inner.cp)
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
    return cpo_java_getobjgap(model.inner.cp)
end

# No DualObjectiveValue.

function MOI.get(model::Optimizer, ::MOI.Silent)
    return model.silent
end

# TODO
# function MOI.get(model::Optimizer, attr::MOI.ResultCount)
#     _throw_if_optimize_in_progress(model, attr)
#     # https://www.ibm.com/support/knowledgecenter/SSSA5P_12.10.0/ilog.odms.cpo.help/refjavacpoptimizer/html/ilog/cp/IloCP.IntInfo.html#NumberOfSolutions
# end

# TODO
# function MOI.set(model::Optimizer, ::MOI.Silent, flag::Bool)
#     model.silent = flag
#     MOI.set(model, MOI.RawParameter("CPX_PARAM_SCRIND"), flag ? 0 : 1)
#     return
# end

# TODO
# function MOI.get(model::Optimizer, ::MOI.NumberOfThreads)
#     # https://www.ibm.com/support/knowledgecenter/SSSA5P_12.10.0/ilog.odms.cpo.help/refjavacpoptimizer/html/ilog/cp/IloCP.IntParam.html#Workers
#     return Int(MOI.get(model, MOI.RawParameter("CPX_PARAM_THREADS")))
# end

# TODO
# function MOI.set(model::Optimizer, ::MOI.NumberOfThreads, x::Int)
#     # https://www.ibm.com/support/knowledgecenter/SSSA5P_12.10.0/ilog.odms.cpo.help/refjavacpoptimizer/html/ilog/cp/IloCP.IntParam.html#Workers
#     return MOI.set(model, MOI.RawParameter("CPX_PARAM_THREADS"), x)
# end

function MOI.get(model::Optimizer, ::MOI.Name)
    return model.name
end

function MOI.set(model::Optimizer, ::MOI.Name, name::String)
    model.name = name
    cpo_java_setname(model.inner.cp, name)
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
# function MOI.set(
#     model::Optimizer,
#     ::MOI.VariablePrimalStart,
#     x::MOI.VariableIndex,
#     value::Union{Nothing, Float64}
# )
#     info = _info(model, x)
#     info.start = value
#     return
# end
#
# function MOI.get(
#     model::Optimizer, ::MOI.VariablePrimalStart, x::MOI.VariableIndex
# )
#     return _info(model, x).start
# end
#
# function MOI.supports(
#     ::Optimizer, ::MOI.VariablePrimalStart, ::Type{MOI.VariableIndex})
#     return true
# end

function MOI.get(model::Optimizer, ::MOI.NumberOfConstraints{F, S}) where {F, S}
    # TODO: this could be more efficient.
    return length(MOI.get(model, MOI.ListOfConstraintIndices{F, S}()))
end

_type_enums(::Type{MOI.ZeroOne}) = (BINARY,)
_type_enums(::Type{MOI.Integer}) = (INTEGER,)
_type_enums(::Type{<:MOI.Semicontinuous}) = (SEMICONTINUOUS,)
_type_enums(::Type{<:MOI.Semiinteger}) = (SEMIINTEGER,)
_type_enums(::Any) = (nothing,)

_check_bound_compatible(model::Optimizer, idx::VariableIndex, ::Type{<:MOI.LessThan{T}}) where T = _has_ub(model, idx, T)
_check_bound_compatible(model::Optimizer, idx::VariableIndex, ::Type{<:MOI.GreaterThan{T}}) where T = _has_lb(model, idx, T)
_check_bound_compatible(model::Optimizer, idx::VariableIndex, ::Type{<:MOI.Interval{T}}) where T = _has_lb(model, idx, T) && _has_ub(model, idx, T)
_check_bound_compatible(model::Optimizer, idx::VariableIndex, ::Type{<:MOI.EqualTo{T}}) where T = _get_lb(model, idx, T) == _get_ub(model, idx, T)
_check_bound_compatible(model::Optimizer, idx::VariableIndex, ::Any) = false

function MOI.get(
    model::Optimizer, ::MOI.ListOfConstraintIndices{MOI.SingleVariable, S}
) where {S}
    indices = MOI.ConstraintIndex{MOI.SingleVariable, S}[]
    for (key, info) in model.variable_info
        if _check_bound_compatible(model, key, S) || info.type in _type_enums(S)
            push!(indices, MOI.ConstraintIndex{MOI.SingleVariable, S}(key.value))
        end
    end
    return sort!(indices, by = x -> x.value)
end

function MOI.get(model::Optimizer, ::MOI.ListOfConstraintIndices{F, S}) where {F, S}
    indices = MOI.ConstraintIndex{F, S}[]
    for (key, info) in model.constraint_info
        if typeof(info.set) == S
            push!(indices, MOI.ConstraintIndex{F, S}(key))
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
