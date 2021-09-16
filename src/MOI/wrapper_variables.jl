function _info(model::Optimizer, key::MOI.VariableIndex)
    if haskey(model.variable_info, key)
        return model.variable_info[key]
    end
    throw(MOI.InvalidIndex(key))
    return
end

function _make_var(model::Optimizer, variable::Variable)
    # Initialize `VariableInfo` with a dummy `VariableIndex` and a column,
    # because we need `add_item` to tell us what the `VariableIndex` is.
    index = CleverDicts.add_item(
        model.variable_info,
        VariableInfo(MOI.VariableIndex(0), variable),
    )
    _info(model, index).index = index
    return index
end

function _make_var(
    model::Optimizer,
    variable::Variable,
    set::MOI.AbstractScalarSet,
)
    index = _make_var(model, variable)
    return index,
    MOI.ConstraintIndex{MOI.VariableIndex, typeof(set)}(index.value)
end

function _make_vars(model::Optimizer, variables::Vector{<:Variable})
    # Barely used, because add_constrained_variables may have variable sets (except for AbstractVectorSet).
    # Only implemented in the unconstrained case.
    indices = Vector{MOI.VariableIndex}(undef, length(variables))
    for i in 1:length(variables)
        indices[i] = CleverDicts.add_item(
            model.variable_info,
            VariableInfo(MOI.VariableIndex(0), variables[i]),
        )
        _info(model, indices[i]).index = indices[i]
    end
    return indices
end

function _make_numvar(
    model::Optimizer,
    set::MOI.AbstractScalarSet;
    lb::Float64=-IloInfinity,
    ub::Float64=IloInfinity,
)
    return _make_var(model, cpo_java_numvar(model.inner, lb, ub), set)
end

function _make_intvar(
    model::Optimizer,
    set::MOI.AbstractScalarSet;
    lb::Int32=-IloMinInt,
    ub::Int32=IloMaxInt,
)
    vindex, cindex = _make_var(
        model,
        cpo_java_intvar(model.inner, Int32(lb), Int32(ub)),
        set,
    )
    _info(model, vindex).type = INTEGER
    return vindex, cindex
end

function _make_boolvar(model::Optimizer, set::MOI.AbstractScalarSet)
    vindex, cindex = _make_var(model, cpo_java_boolvar(model.inner), set)
    _info(model, vindex).type = BINARY
    return vindex, cindex
end

function supports_add_constrained_variables(
    ::Optimizer,
    ::Type{F},
) where {
    F <: Union{
        MOI.EqualTo{Float64},
        MOI.LessThan{Float64},
        MOI.GreaterThan{Float64},
        MOI.Interval{Float64},
        MOI.EqualTo{Int},
        MOI.LessThan{Int},
        MOI.GreaterThan{Int},
        MOI.Interval{Int},
        MOI.ZeroOne,
        MOI.Integer,
    },
}
    return true
end

function MOI.add_variable(model::Optimizer)
    return _make_var(
        model,
        cpo_java_numvar(model.inner, -IloInfinity, IloInfinity),
    )
end

function MOI.add_variables(model::Optimizer, N::Int)
    return _make_vars(
        model,
        cpo_java_numvararray(model.inner, Int32(N), -IloInfinity, IloInfinity),
    )
end

function MOI.add_constrained_variable(
    model::Optimizer,
    set::MOI.GreaterThan{T},
) where {T <: Real}
    return _make_numvar(model, set, lb=set.lower)
end

function MOI.add_constrained_variable(
    model::Optimizer,
    set::MOI.LessThan{T},
) where {T <: Real}
    return _make_numvar(model, set, lb=set.upper)
end

function MOI.add_constrained_variable(
    model::Optimizer,
    set::MOI.EqualTo{T},
) where {T <: Real}
    return _make_numvar(model, set, lb=set.value, ub=set.value)
end

function MOI.add_constrained_variable(
    model::Optimizer,
    set::MOI.Interval{T},
) where {T <: Real}
    return _make_numvar(model, set, lb=set.lower, ub=set.upper)
end

function MOI.add_constrained_variable(
    model::Optimizer,
    set::MOI.GreaterThan{T},
) where {T <: Integer}
    return _make_intvar(model, set, lb=set.lower)
end

function MOI.add_constrained_variable(
    model::Optimizer,
    set::MOI.LessThan{T},
) where {T <: Integer}
    return _make_intvar(model, set, lb=set.upper)
end

function MOI.add_constrained_variable(
    model::Optimizer,
    set::MOI.EqualTo{T},
) where {T <: Integer}
    return _make_intvar(model, set, lb=set.value, ub=set.value)
end

function MOI.add_constrained_variable(
    model::Optimizer,
    set::MOI.Interval{T},
) where {T <: Integer}
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

    # We throw away name_to_constraint so we will rebuild VariableIndex
    # constraint names (without v) on the next access to it.
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

function MOI.set(
    model::Optimizer,
    ::MOI.VariableName,
    v::MOI.VariableIndex,
    name::String,
)
    info = _info(model, v)
    info.name = name
    cpo_java_addable_setname(model.inner, info.variable, name)

    model.name_to_variable = nothing
    model.name_to_constraint = nothing

    return
end
