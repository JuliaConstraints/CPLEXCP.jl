# CP.ReificationSet
function MOI.supports_constraint(o::Optimizer, f::Type{F}, ::Type{S}) where {
    F <: MOI.VectorOfVariables,
    S2 <: MOI.AbstractSet,
    S <: CP.ReificationSet{S2}
}
    # TODO: the output value should depend on the number of values in the function (either 2 or more).
    # return if MOI.output_dimension(f) == 2
    #     MOI.supports_constraint(o, MOI.SingleVariable(f.variables[2]), S2)
    # else
    #     MOI.supports_constraint(o, MOI.VectorOfVariables(f.variables[2:end]), S2)
    # end
    return MOI.supports_constraint(o, MOI.SingleVariable, S2) || MOI.supports_constraint(o, MOI.VectorOfVariables, S2)
end

function MOI.supports_constraint(o::Optimizer, f::Type{F}, ::Type{S}) where {
    T <: Int,
    F <: MOI.VectorAffineFunction{T},
    S2 <: MOI.AbstractSet,
    S <: CP.ReificationSet{S2}
}
    # TODO: the output value should depend on the number of values in the function (either 2 or more).
    # return if MOI.output_dimension(f) == 2
    #     MOI.supports_constraint(o, MOI.ScalarAffineFunction(f.terms[2], f.constants[2]), S2)
    # else
    #     MOI.supports_constraint(o, MOI.VectorAffineFunction(f.terms[2:end], f.constants[2:end]), S2)
    # end
    return MOI.supports_constraint(o, MOI.ScalarAffineFunction{T}, S2) || MOI.supports_constraint(o, MOI.VectorAffineFunction{T}, S2)
end

function _build_constraint(model::Optimizer, f::MOI.VectorOfVariables, s::CP.ReificationSet{S2}) where S2 <: MOI.AbstractSet
    # Split the dimensions in the right parts.
    f_parsed = _parse(model, f)
    reify_indicator = f_parsed[1]
    reify_set_variables_raw = if MOI.output_dimension(f) == 2
        MOI.SingleVariable(f.variables[2])
    else
        MOI.VectorOfVariables(f.variables[2:end])
    end

    # Ensure that the indicator is a binary variable.
    # TODO: allow this constraint to be deleted at the same time as the reified constraint.
    cpo_java_add(model.inner, cpo_java_range(model.inner, 0, reify_indicator, 1))

    # Build the constraint.
    indicator = cpo_java_eq(model.inner, reify_indicator, Int32(true))
    set = _build_constraint(model, reify_set_variables_raw, s.set)
    return cpo_java_equiv(model.inner, indicator, set)
end

function _build_constraint(model::Optimizer, f::MOI.VectorAffineFunction{T}, s::CP.ReificationSet{S2}) where {T <: Int, S2 <: MOI.AbstractSet}
    # Split the dimensions in the right parts.
    f_parsed = _parse(model, f)
    reify_indicator = f_parsed[1]
    reify_set_variables_raw = if MOI.output_dimension(f) == 2
        collect(MOIU.eachscalar(f))[2]
    else
        MOI.VectorAffineFunction(f.terms[2:end], f.constants[2:end])
    end

    # Ensure that the indicator is a binary variable.
    # TODO: allow this constraint to be deleted at the same time as the reified constraint.
    cpo_java_add(model.inner, cpo_java_range(model.inner, 0, reify_indicator, 1))

    # Build the constraint.
    indicator = cpo_java_eq(model.inner, reify_indicator, Int32(true))
    set = _build_constraint(model, reify_set_variables_raw, s.set)
    return cpo_java_equiv(model.inner, indicator, set)
end

# CP.EquivalenceSet
function MOI.supports_constraint(::Optimizer, ::Type{F}, ::Type{S}) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S1 <: MOI.AbstractSet, 
    S2 <: MOI.AbstractSet,
    S <: CP.EquivalenceSet{S1, S2}
}
    return true
end

function _build_constraint(model::Optimizer, f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}}, s::CP.EquivalenceSet{S1, S2}) where {
        T <: Int, 
        S1 <: MOI.AbstractSet, 
        S2 <: MOI.AbstractSet
    }
    f_parsed = _parse(model, f)

    equivalence_first = f_parsed[1:MOI.dimension(s.set1)]
    equivalence_second = f_parsed[(1 + MOI.dimension(s.set1)) : (MOI.dimension(s.set1) + MOI.dimension(s.set2))]
    
    return cpo_java_equiv(model.inner, _build_constraint(model, equivalence_first, s.set1), _build_constraint(model, equivalence_second, s.set2))
end
