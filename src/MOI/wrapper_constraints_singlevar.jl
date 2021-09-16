function _get_lb(model::Optimizer, index::MOI.VariableIndex)
    return cpo_java_numvar_getlb(model.inner, _info(model, index).variable)
end
function _get_ub(model::Optimizer, index::MOI.VariableIndex)
    return cpo_java_numvar_getub(model.inner, _info(model, index).variable)
end
function _has_lb(model::Optimizer, index::MOI.VariableIndex)
    return _get_lb(model, index) != -IloInfinity &&
           _get_lb(model, index) != Float64(IloMinInt)
end
function _has_ub(model::Optimizer, index::MOI.VariableIndex)
    return _get_ub(model, index) != IloInfinity &&
           _get_ub(model, index) != Float64(IloMaxInt)
end

function _bounds_to_set(model::Optimizer, index::MOI.VariableIndex)
    if _has_lb(model, index)
        if _has_ub(model, index)
            if _info(model, index).ub == _info(model, index).ub
                return MOI.EqualTo{typeof(_info(model, index).ub)}
            else
                return MOI.Interval{typeof(_info(model, index).ub)}
            end
        else
            return MOI.GreaterThan{typeof(_info(model, index).lb)}
        end
    else
        if _has_ub(model, index)
            return MOI.LessThan{typeof(_info(model, index).ub)}
        else
            # No bounds set!
            return nothing
        end
    end
end

function _assert_no_lb(model::Optimizer, i::MOI.VariableIndex, s)
    return _has_lb(model, i) &&
           throw(MOI.LowerBoundAlreadySet{_bounds_to_set(model, i), s}(i))
end
function _assert_no_ub(model::Optimizer, i::MOI.VariableIndex, s)
    return _has_ub(model, i) &&
           throw(MOI.UpperBoundAlreadySet{_bounds_to_set(model, i), s}(i))
end

function _set_lb(model::Optimizer, index::MOI.VariableIndex, lb::Real)
    lb = Float64(lb)
    info = _info(model, index)
    info.lb = lb
    cpo_java_numvar_setlb(model.inner, info.variable, lb)
    return
end
function _set_ub(model::Optimizer, index::MOI.VariableIndex, ub::Real)
    ub = Float64(ub)
    info = _info(model, index)
    info.ub = ub
    cpo_java_numvar_setub(model.inner, info.variable, ub)
    return
end

function MOI.is_valid(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VariableIndex, MOI.LessThan{T}},
) where {T <: Real}
    index = MOI.VariableIndex(c.value)
    return MOI.is_valid(model, index) && _has_ub(model, index)
end

function MOI.is_valid(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VariableIndex, MOI.GreaterThan{T}},
) where {T <: Real}
    index = MOI.VariableIndex(c.value)
    return MOI.is_valid(model, index) && _has_lb(model, index)
end

function MOI.is_valid(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VariableIndex, MOI.Interval{T}},
) where {T <: Real}
    index = MOI.VariableIndex(c.value)
    return MOI.is_valid(model, index) &&
           _has_lb(model, index) &&
           _has_ub(model, index)
end

function MOI.is_valid(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VariableIndex, MOI.EqualTo{T}},
) where {T <: Real}
    index = MOI.VariableIndex(c.value)
    return MOI.is_valid(model, index) &&
           _get_lb(model, index) == _get_ub(model, index)
end

function MOI.is_valid(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VariableIndex, MOI.ZeroOne},
)
    index = MOI.VariableIndex(c.value)
    if !MOI.is_valid(model, index)
        return false
    end

    info = _info(model, index)
    return info.type == BINARY || info.binary !== nothing
end

function MOI.is_valid(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VariableIndex, MOI.Integer},
)
    index = MOI.VariableIndex(c.value)
    if !MOI.is_valid(model, index)
        return false
    end

    info = _info(model, index)
    return info.type == INTEGER || info.integer !== nothing
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintFunction,
    c::MOI.ConstraintIndex{MOI.VariableIndex, <:Any},
)
    println(">> $c")
    MOI.throw_if_not_valid(model, c)
    return MOI.VariableIndex(MOI.VariableIndex(c.value))
end

function MOI.set(
    ::Optimizer,
    ::MOI.ConstraintFunction,
    ::MOI.ConstraintIndex{MOI.VariableIndex, S},
    ::MOI.VariableIndex,
) where {S}
    throw(MOI.SettingVariableIndexFunctionNotAllowed())
    return
end

function MOI.set(
    model::Optimizer,
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{MOI.VariableIndex, S},
    s::S,
) where {S}
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

function MOI.add_constraint(
    model::Optimizer,
    f::MOI.VariableIndex,
    s::MOI.LessThan{T},
) where {T <: Real}
    @assert _info(model, f).type == _type_to_variabletype(T)
    _assert_no_ub(model, f, s)
    _set_ub(model, f, s.upper)
    return MOI.ConstraintIndex{MOI.VariableIndex, typeof(s)}(f.value)
end

function MOI.add_constraint(
    model::Optimizer,
    f::MOI.VariableIndex,
    s::MOI.GreaterThan{T},
) where {T <: Real}
    @assert _info(model, f).type == _type_to_variabletype(T)
    _assert_no_lb(model, f, s)
    _set_lb(model, f, s.lower)
    return MOI.ConstraintIndex{MOI.VariableIndex, typeof(s)}(f.value)
end

function MOI.add_constraint(
    model::Optimizer,
    f::MOI.VariableIndex,
    s::MOI.EqualTo{T},
) where {T <: Real}
    @assert _info(model, f).type == _type_to_variabletype(T)
    _assert_no_lb(model, f, s)
    _assert_no_ub(model, f, s)
    _set_lb(model, f, s.value)
    _set_ub(model, f, s.value)
    return MOI.ConstraintIndex{MOI.VariableIndex, typeof(s)}(f.value)
end

function MOI.add_constraint(
    model::Optimizer,
    f::MOI.VariableIndex,
    s::MOI.Interval{T},
) where {T <: Real}
    @assert _info(model, f).type == _type_to_variabletype(T)
    _assert_no_lb(model, f, s)
    _assert_no_ub(model, f, s)
    _set_lb(model, f, s.lower)
    _set_ub(model, f, s.upper)
    return MOI.ConstraintIndex{MOI.VariableIndex, typeof(s)}(f.value)
end

function MOI.add_constraint(
    model::Optimizer,
    f::MOI.VariableIndex,
    ::MOI.ZeroOne,
)
    info = _info(model, f)
    @assert info.type == CONTINUOUS || info.type == INTEGER
    @assert info.binary === nothing

    # CPLEX Java API expects variables to be created with the right type.
    # Bad trick: create a binary variable, impose equality. Don't show this variable to MOI, though.
    bindex, cindex = MOI.add_constrained_variable(model, MOI.ZeroOne())
    eqcstr =
        cpo_java_eq(model.inner, info.variable, _info(model, bindex).variable)
    cpo_java_add(model.inner, eqcstr)
    info.binary = (bindex, cindex, eqcstr)

    return MOI.ConstraintIndex{MOI.VariableIndex, MOI.ZeroOne}(
        f.value,
    )
end

function MOI.add_constraint(
    model::Optimizer,
    f::MOI.VariableIndex,
    ::MOI.Integer,
)
    info = _info(model, f)
    @assert info.type == CONTINUOUS
    @assert info.integer === nothing

    # CPLEX Java API expects variables to be created with the right type.
    # Bad trick: create an integer variable, impose equality. Don't show this variable to MOI, though.
    bindex, cindex = MOI.add_constrained_variable(model, MOI.Integer())
    eqcstr =
        cpo_java_eq(model.inner, info.variable, _info(model, bindex).variable)
    cpo_java_add(model.inner, eqcstr)
    info.integer = (bindex, cindex, eqcstr)

    return MOI.ConstraintIndex{MOI.VariableIndex, MOI.ZeroOne}(
        f.value,
    )
end

# No similar function for a vector of variables, no way to do it more efficiently.

function MOI.delete(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VariableIndex, MOI.LessThan{T}},
) where {T <: Real}
    MOI.throw_if_not_valid(model, c)
    _set_ub(
        model,
        MOI.VariableIndex(c.value),
        (T == Float64) ? IloInfinity : IloMaxInt,
    )
    _info(model, MOI.VariableIndex(c.value)).ub_name = ""
    model.name_to_constraint = nothing
    return
end

function MOI.delete(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VariableIndex, MOI.GreaterThan{T}},
) where {T <: Real}
    MOI.throw_if_not_valid(model, c)
    _set_lb(
        model,
        MOI.VariableIndex(c.value),
        (T == Float64) ? -IloInfinity : IloMinInt,
    )
    _info(model, MOI.VariableIndex(c.value)).lb_name = ""
    model.name_to_constraint = nothing
    return
end

function MOI.delete(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VariableIndex, MOI.EqualTo{T}},
) where {T <: Real}
    MOI.throw_if_not_valid(model, c)
    _set_ub(
        model,
        MOI.VariableIndex(c.value),
        (T == Float64) ? IloInfinity : IloMaxInt,
    )
    _set_lb(
        model,
        MOI.VariableIndex(c.value),
        (T == Float64) ? -IloInfinity : IloMinInt,
    )
    _info(model, MOI.VariableIndex(c.value)).equalto_name = ""
    model.name_to_constraint = nothing
    return
end

function MOI.delete(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VariableIndex, MOI.Interval{T}},
) where {T <: Real}
    MOI.throw_if_not_valid(model, c)
    _set_ub(
        model,
        MOI.VariableIndex(c.value),
        (T == Float64) ? IloInfinity : IloMaxInt,
    )
    _set_lb(
        model,
        MOI.VariableIndex(c.value),
        (T == Float64) ? -IloInfinity : IloMinInt,
    )
    _info(model, MOI.VariableIndex(c.value)).interval_name = ""
    model.name_to_constraint = nothing
    return
end

function MOI.delete(
    model::Optimizer,
    c::MOI.ConstraintIndex{MOI.VariableIndex, S},
) where {S <: Union{MOI.ZeroOne, MOI.Integer}}
    MOI.throw_if_not_valid(model, c)
    info = _info(model, MOI.VariableIndex(c.value))
    @assert info.type == CONTINUOUS || info.type == INTEGER

    if S == MOI.ZeroOne
        @assert info.binary !== nothing
        cpo_java_remove(model.inner, info.binary[3])
        info.binary = nothing
    else
        @assert info.integer !== nothing
        cpo_java_remove(model.inner, info.integer[3])
        info.integer = nothing
    end

    return
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{MOI.VariableIndex, MOI.GreaterThan{T}},
) where {T <: Real}
    MOI.throw_if_not_valid(model, c)
    return MOI.GreaterThan(_get_lb(model, MOI.VariableIndex(c.value)))
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{MOI.VariableIndex, MOI.LessThan{T}},
) where {T <: Real}
    MOI.throw_if_not_valid(model, c)
    return MOI.LessThan(_get_ub(model, MOI.VariableIndex(c.value)))
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{MOI.VariableIndex, MOI.EqualTo{T}},
) where {T <: Real}
    MOI.throw_if_not_valid(model, c)
    return MOI.EqualTo(_get_ub(model, MOI.VariableIndex(c.value)))
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{MOI.VariableIndex, MOI.Interval{T}},
) where {T <: Real}
    MOI.throw_if_not_valid(model, c)
    return MOI.Interval(
        _get_lb(model, MOI.VariableIndex(c.value)),
        _get_ub(model, MOI.VariableIndex(c.value)),
    )
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{MOI.VariableIndex, MOI.ZeroOne},
)
    MOI.throw_if_not_valid(model, c)
    return MOI.ZeroOne()
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{MOI.VariableIndex, MOI.Integer},
)
    MOI.throw_if_not_valid(model, c)
    return MOI.Integer()
end

function MOI.get(
    model::Optimizer,
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{F, S},
) where {F, S}
    return _info(model, c).set
end

# TODO: MOI.set? Remove the constraint and rebuild it?
# TODO: make variable integer/binary?
# TODO: constraint names here? They don't get passed to the solver.

# Implementations of _build_constraint that are mostly useful for reification.
# MOI.add_constraint rather works on the variable bounds than new constraints in this case.
function _build_constraint(
    model::Optimizer,
    f::MOI.VariableIndex,
    s::MOI.GreaterThan{T},
) where {T <: Real}
    return cpo_java_ge(model.inner, _parse(model, f), s.lower)
end

function _build_constraint(
    model::Optimizer,
    f::MOI.VariableIndex,
    s::MOI.LessThan{T},
) where {T <: Real}
    return cpo_java_le(model.inner, _parse(model, f), s.upper)
end

function _build_constraint(
    model::Optimizer,
    f::MOI.VariableIndex,
    s::MOI.EqualTo{T},
) where {T <: Real}
    return cpo_java_eq(model.inner, _parse(model, f), s.value)
end

function _build_constraint(
    model::Optimizer,
    f::MOI.VariableIndex,
    s::MOI.Interval{T},
) where {T <: Real}
    if s.lower == Inf
        return _build_constraint(model, f, MOI.LessThan(s.upper))
    elseif s.upper == Inf
        return _build_constraint(model, f, MOI.GreaterThan(s.lower))
    elseif s.lower == s.upper
        return _build_constraint(model, f, MOI.EqualTo(s.lower))
    end

    return cpo_java_range(model.inner, s.lower, _parse(model, f), s.upper)
end
