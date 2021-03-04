# CP.ReificationSet
function MOI.supports_constraint(
    o::Optimizer,
    f::Type{F},
    ::Type{S},
) where {
    F <: MOI.VectorOfVariables,
    S2 <: MOI.AbstractSet,
    S <: CP.ReificationSet{S2},
}
    # TODO: the output value should depend on the number of values in the function (either 2 or more).
    # return if MOI.output_dimension(f) == 2
    #     MOI.supports_constraint(o, MOI.SingleVariable(f.variables[2]), S2)
    # else
    #     MOI.supports_constraint(o, MOI.VectorOfVariables(f.variables[2:end]), S2)
    # end
    return MOI.supports_constraint(o, MOI.SingleVariable, S2) ||
           MOI.supports_constraint(o, MOI.VectorOfVariables, S2)
end

function MOI.supports_constraint(
    o::Optimizer,
    f::Type{F},
    ::Type{S},
) where {
    T <: Int,
    F <: MOI.VectorAffineFunction{T},
    S2 <: MOI.AbstractSet,
    S <: CP.ReificationSet{S2},
}
    # TODO: the output value should depend on the number of values in the function (either 2 or more).
    # return if MOI.output_dimension(f) == 2
    #     MOI.supports_constraint(o, MOI.ScalarAffineFunction(f.terms[2], f.constants[2]), S2)
    # else
    #     MOI.supports_constraint(o, MOI.VectorAffineFunction(f.terms[2:end], f.constants[2:end]), S2)
    # end
    return MOI.supports_constraint(o, MOI.ScalarAffineFunction{T}, S2) ||
           MOI.supports_constraint(o, MOI.VectorAffineFunction{T}, S2)
end

function _build_constraint(
    model::Optimizer,
    f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    s::CP.ReificationSet{S2},
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

# CP.EquivalenceSet
function MOI.supports_constraint(
    ::Optimizer,
    ::Type{F},
    ::Type{S},
) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S1 <: MOI.AbstractSet,
    S2 <: MOI.AbstractSet,
    S <: CP.EquivalenceSet{S1, S2},
}
    return true
end

function _build_constraint(
    model::Optimizer,
    f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    s::CP.EquivalenceSet{S1, S2},
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

# CP.IfThenElseSet
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
    S <: CP.IfThenElseSet{S1, S2, S3},
}
    return true
end

function _build_constraint(
    model::Optimizer,
    f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    s::CP.IfThenElseSet{S1, S2, S3},
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

# CP.ImplySet
function MOI.supports_constraint(
    ::Optimizer,
    ::Type{F},
    ::Type{S},
) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S1 <: MOI.AbstractSet,
    S2 <: MOI.AbstractSet,
    S <: CP.ImplySet{S1, S2},
}
    return true
end

function _build_constraint(
    model::Optimizer,
    f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    s::CP.ImplySet{S1, S2},
) where {
    T <: Int,
    S1 <: MOI.AbstractSet,
    S2 <: MOI.AbstractSet,
}
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
