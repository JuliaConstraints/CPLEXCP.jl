## ScalarAffineFunction-in-Set
## ScalarQuadraticFunction-in-Set

function _build_constraint(model::Optimizer, f::F, s::MOI.GreaterThan{T}) where {T <: Real, F <: Union{MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T}}}
    return cpo_java_ge(model.inner, _parse(model, f), s.lower)
end

function _build_constraint(model::Optimizer, f::F, s::MOI.LessThan{T}) where {T <: Real, F <: Union{MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T}}}
    return cpo_java_le(model.inner, _parse(model, f), s.upper)
end

function _build_constraint(model::Optimizer, f::F, s::MOI.EqualTo{T}) where {T <: Real, F <: Union{MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T}}}
    return cpo_java_eq(model.inner, _parse(model, f), s.value)
end

function _build_constraint(model::Optimizer, f::F, s::MOI.EqualTo{T}) where {T <: Integer, F <: Union{MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T}}}
    return cpo_java_eq(model.inner, _parse(model, f), Int32(s.value))
end

function _build_constraint(model::Optimizer, f::F, s::MOI.Interval{T}) where {T <: Real, F <: Union{MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T}}}
    if s.lower == Inf
        return _build_constraint(model, f, MOI.LessThan(s.upper))
    elseif s.upper == Inf
        return _build_constraint(model, f, MOI.GreaterThan(s.lower))
    elseif s.lower == s.upper
        return _build_constraint(model, f, MOI.EqualTo(s.lower))
    end

    return cpo_java_range(model.inner, s.lower, _parse(model, f), s.upper)
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

## VectorOfVariables-in-SOS{I|II}
# Not available. Bridge it?

## VectorOfVariables-in-SecondOrderCone

function _build_constraint(model::Optimizer, f::MOI.VectorOfVariables, s::MOI.SecondOrderCone)
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
    expr = _parse(model, f, s)
    return cpo_java_gt(model, expr, 0)
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