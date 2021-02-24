## ScalarAffineFunction-in-Set
## ScalarQuadraticFunction-in-Set

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

function MOI.delete(model::Optimizer, c::MOI.ConstraintIndex{F, S}) where {
            T <: Real, 
            F <: Union{MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T}}, 
            S <: Union{MOI.GreaterThan{T}, MOI.LessThan{T}, MOI.EqualTo{T}, MOI.Interval{T}}
        }
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

## VectorOfVariables-in-SecondOrderCone

function MOI.add_constraint(model::Optimizer, f::MOI.VectorOfVariables, s::MOI.SecondOrderCone)
    if length(f.variables) != s.dimension
        error("Dimension of $(s) does not match number of terms in $(f)")
    end

    # First, check the lower bound on t.
    t_info = _info(model, f.variables[1])
    if !_has_lb(model, t_info.index) || _get_lb(model, t_info.index) < 0.0
        if _get_lb(model, t_info.index) < 0.0
            t_info.old_lb = _get_lb(model, t_info.index)
        end

        t_info.n_socs += 1
        _set_lb(model, t_info.index, 0.0)
    end

    # Then, add the quadratic constraint.
    cindex = MOI.ConstraintIndex{MOI.VectorOfVariables, MOI.SecondOrderCone}(length(model.constraint_info) + 1)
    expr = _parse(model, f, s)
    constr = cpo_java_gt(model, expr, 0)
    model.constraint_info[cindex] = ConstraintInfo(cindex, constr, f, s)
    return cindex
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