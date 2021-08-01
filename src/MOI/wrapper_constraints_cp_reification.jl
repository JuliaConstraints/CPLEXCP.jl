# CP.Reification
function MOI.supports_constraint(
    o::Optimizer,
    ::Type{MOI.VectorOfVariables},
    ::Type{S},
) where {S2 <: MOI.AbstractSet, S <: CP.Reification{S2}}
    # No information about the dimension of the function, only works if 2.
    return MOI.supports_constraint(o, MOI.SingleVariable, S2) ||
           MOI.supports_constraint(o, MOI.VectorOfVariables, S2)
end

function MOI.supports_constraint(
    o::Optimizer,
    ::Type{F},
    ::Type{S},
) where {
    T <: Int,
    F <: MOI.VectorAffineFunction{T},
    S2 <: MOI.AbstractSet,
    S <: CP.Reification{S2},
}
    # No information about the dimension of the function, only works if 2.
    return MOI.supports_constraint(o, MOI.ScalarAffineFunction{T}, S2) ||
           MOI.supports_constraint(o, MOI.VectorAffineFunction{T}, S2)
end

function _build_constraint(
    model::Optimizer,
    f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    s::CP.Reification{S2},
) where {S2 <: MOI.AbstractSet, T <: Int}
    # Split the dimensions in the right parts. Only parse the first component, 
    # as the rest will be handled by the reified constraint.
    dim_set = MOI.dimension(s.set)
    f_parsed = _parse(model, f)
    reify_indicator = f_parsed[1]
    reify_set_variables = _slice(f, 2:(dim_set + 1))

    # Ensure that the indicator is a binary variable.
    # TODO: allow this constraint to be deleted at the same time as the reified constraint.
    cpo_java_add(
        model.inner,
        cpo_java_range(model.inner, 0, reify_indicator, 1),
    )

    # Build the constraint.
    indicator = cpo_java_eq(model.inner, reify_indicator, Int32(true))
    set = _build_constraint(model, reify_set_variables, s.set)
    return cpo_java_equiv(model.inner, indicator, set)
end

# CP.Equivalence
function MOI.supports_constraint(
    ::Optimizer,
    ::Type{F},
    ::Type{S},
) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S1 <: MOI.AbstractSet,
    S2 <: MOI.AbstractSet,
    S <: CP.Equivalence{S1, S2},
}
    return true
end

function _build_constraint(
    model::Optimizer,
    f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    s::CP.Equivalence{S1, S2},
) where {T <: Int, S1 <: MOI.AbstractSet, S2 <: MOI.AbstractSet}
    dim_first = MOI.dimension(s.set1)
    dim_second = MOI.dimension(s.set2)
    dim_end = dim_first + dim_second

    first = _slice(f, 1:dim_first)
    second = _slice(f, (1 + dim_first):dim_end)

    return cpo_java_equiv(
        model.inner,
        _build_constraint(model, first, s.set1),
        _build_constraint(model, second, s.set2),
    )
end

# CP.EquivalenceNot
function MOI.supports_constraint(
    ::Optimizer,
    ::Type{F},
    ::Type{S},
) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S1 <: MOI.AbstractSet,
    S2 <: MOI.AbstractSet,
    S <: CP.EquivalenceNot{S1, S2},
}
    return true
end

function _build_constraint(
    model::Optimizer,
    f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    s::CP.EquivalenceNot{S1, S2},
) where {T <: Int, S1 <: MOI.AbstractSet, S2 <: MOI.AbstractSet}
    dim_first = MOI.dimension(s.set1)
    dim_second = MOI.dimension(s.set2)
    dim_end = dim_first + dim_second

    first = _slice(f, 1:dim_first)
    second = _slice(f, (1 + dim_first):dim_end)

    return cpo_java_neq(
        model.inner,
        _build_constraint(model, first, s.set1),
        _build_constraint(model, second, s.set2),
    )
end

# CP.IfThenElse
function MOI.supports_constraint(
    ::Optimizer,
    ::Type{F},
    ::Type{S},
) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S1 <: MOI.AbstractSet,
    S2 <: MOI.AbstractSet,
    S3 <: MOI.AbstractSet,
    S <: CP.IfThenElse{S1, S2, S3},
}
    return true
end

function _build_constraint(
    model::Optimizer,
    f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    s::CP.IfThenElse{S1, S2, S3},
) where {
    T <: Int,
    S1 <: MOI.AbstractSet,
    S2 <: MOI.AbstractSet,
    S3 <: MOI.AbstractSet,
}
    dim_first = MOI.dimension(s.condition)
    dim_second = MOI.dimension(s.true_constraint)
    dim_third = MOI.dimension(s.false_constraint)
    dim_end = dim_first + dim_second + dim_third

    first = _slice(f, 1:dim_first)
    second = _slice(f, (1 + dim_first):(dim_first + dim_second))
    third = _slice(f, (1 + dim_first + dim_second):dim_end)

    return cpo_java_ifthenelse(
        model.inner,
        _build_constraint(model, first, s.condition),
        _build_constraint(model, second, s.true_constraint),
        _build_constraint(model, third, s.false_constraint),
    )
end

# CP.Imply
function MOI.supports_constraint(
    ::Optimizer,
    ::Type{F},
    ::Type{S},
) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S1 <: MOI.AbstractSet,
    S2 <: MOI.AbstractSet,
    S <: CP.Imply{S1, S2},
}
    return true
end

function _build_constraint(
    model::Optimizer,
    f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    s::CP.Imply{S1, S2},
) where {T <: Int, S1 <: MOI.AbstractSet, S2 <: MOI.AbstractSet}
    dim_first = MOI.dimension(s.antecedent)
    dim_second = MOI.dimension(s.consequent)
    dim_end = dim_first + dim_second

    first = _slice(f, 1:dim_first)
    second = _slice(f, (1 + dim_first):dim_end)

    return cpo_java_imply(
        model.inner,
        _build_constraint(model, first, s.antecedent),
        _build_constraint(model, second, s.consequent),
    )
end

# CP.True
function MOI.supports_constraint(
    ::Optimizer,
    ::Type{F},
    ::Type{S},
) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S <: CP.True,
}
    return true
end

function _build_constraint(
    model::Optimizer,
    f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    s::CP.True,
) where {T <: Int}
    return cpo_java_trueconstraint(model.inner)
end

# CP.False
function MOI.supports_constraint(
    ::Optimizer,
    ::Type{F},
    ::Type{S},
) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S <: CP.False,
}
    return true
end

function _build_constraint(
    model::Optimizer,
    f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    s::CP.False,
) where {T <: Int}
    return cpo_java_falseconstraint(model.inner)
end
