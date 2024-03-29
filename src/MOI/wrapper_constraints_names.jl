function MOI.set(
    model::Optimizer,
    ::MOI.ConstraintName,
    c::MOI.ConstraintIndex,
    name::String,
)
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
    model::Optimizer,
    C::Type{MOI.ConstraintIndex{F, S}},
    name::String,
) where {F, S}
    index = MOI.get(model, MOI.ConstraintIndex, name)
    if typeof(index) == C
        return index::MOI.ConstraintIndex{F, S}
    end
    return nothing
end

function _rebuild_name_to_constraint_add!(
    model::Optimizer,
    name::String,
    cindex::MOI.ConstraintIndex,
)
    if name == ""
        return
    end

    if haskey(model.name_to_constraint, name)
        error("Duplicate variable name detected: $(name)")
    end
    return model.name_to_constraint[name] = cindex
end

function _rebuild_name_to_constraint(model::Optimizer)
    model.name_to_constraint = Dict{String, MOI.ConstraintIndex}()

    # Variable bounds.
    for (index, info) in model.variable_info
        T = _variabletype_to_type(info.type)

        if _check_bound_compatible(model, index, MOI.EqualTo{T})
            _rebuild_name_to_constraint_add!(
                model,
                info.equalto_name,
                MOI.ConstraintIndex{MOI.VariableIndex, MOI.EqualTo{T}}(
                    index.value,
                ),
            )
        end
        if _check_bound_compatible(model, index, MOI.Interval{T})
            _rebuild_name_to_constraint_add!(
                model,
                info.interval_name,
                MOI.ConstraintIndex{MOI.VariableIndex, MOI.Interval{T}}(
                    index.value,
                ),
            )
        end
        if _check_bound_compatible(model, index, MOI.LessThan{T})
            _rebuild_name_to_constraint_add!(
                model,
                info.ub_name,
                MOI.ConstraintIndex{MOI.VariableIndex, MOI.LessThan{T}}(
                    index.value,
                ),
            )
        end
        if _check_bound_compatible(model, index, MOI.GreaterThan{T})
            _rebuild_name_to_constraint_add!(
                model,
                info.lb_name,
                MOI.ConstraintIndex{MOI.VariableIndex, MOI.GreaterThan{T}}(
                    index.value,
                ),
            )
        end

        if info.type == INTEGER || info.integer !== nothing
            _rebuild_name_to_constraint_add!(model, info.integer_name, index)
        elseif info.type == BINARY || info.binary !== nothing
            _rebuild_name_to_constraint_add!(model, info.binary_name, index)
        end
    end

    # Other constraints.
    for (index, info) in model.constraint_info
        _rebuild_name_to_constraint_add!(
            model,
            info.name,
            MOI.ConstraintIndex{typeof(info.f), typeof(info.set)}(index.value),
        )
    end

    return
end
