# Generic case for most constraints, excluding those on single variables.
function _info(::Optimizer, key::MOI.ConstraintIndex{MOI.SingleVariable, <:Any})
    throw(MOI.InvalidIndex(key))
    return
end

function _info(model::Optimizer, key::MOI.ConstraintIndex)
    if haskey(model.constraint_info, key)
        return model.constraint_info[key]
    end
    throw(MOI.InvalidIndex(key))
    return
end

function MOI.is_valid(
    model::Optimizer,
    c::MOI.ConstraintIndex{F, S},
) where {F <: MOI.AbstractFunction, S <: MOI.AbstractSet}
    info = get(model.constraint_info, c, nothing)
    return info !== nothing && typeof(info.set) == S
end

function MOI.add_constraint(
    model::Optimizer,
    f::F,
    s::S,
) where {F <: MOI.AbstractFunction, S <: MOI.AbstractSet}
    index = MOI.ConstraintIndex{F, S}(length(model.constraint_info) + 1)
    constr = _build_constraint(model, f, s)
    cpo_java_add(model.inner, constr)
    model.constraint_info[index] = ConstraintInfo(index, constr, f, s)
    return index
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintFunction,
    c::MOI.ConstraintIndex{F, S},
) where {F <: MOI.AbstractFunction, S <: MOI.AbstractSet}
    return _info(model, c).f
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{F, S},
) where {F <: MOI.AbstractFunction, S <: MOI.AbstractSet}
    return _info(model, c).set
end
