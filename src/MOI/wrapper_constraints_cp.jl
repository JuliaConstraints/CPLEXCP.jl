# CP.AllDifferent
function MOI.supports_constraint(::Optimizer, ::Type{F}, ::Type{S}) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S <: CP.AllDifferent
}
    return true
end

function _build_constraint(model::Optimizer, f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}}, s::CP.AllDifferent) where {T}
    return cpo_java_alldiff(model.inner, _parse(model, f))
end

# CP.Domain
function MOI.supports_constraint(::Optimizer, ::Type{F}, ::Type{S}) where {
    T <: Int,
    F <: Union{MOI.SingleVariable, MOI.ScalarAffineFunction{T}},
    S <: CP.Domain{T}
}
    return true
end

function _build_constraint(model::Optimizer, f::Union{MOI.SingleVariable, MOI.ScalarAffineFunction{T}}, s::CP.Domain{T}) where {T <: Int}
    return cpo_java_allowedassignments(model.inner, _parse(model, f), collect(Int32(v) for v in s.values))
end

# CP.AntiDomain
function MOI.supports_constraint(::Optimizer, ::Type{F}, ::Type{S}) where {
    T <: Int,
    F <: Union{MOI.SingleVariable, MOI.ScalarAffineFunction{T}},
    S <: CP.AntiDomain{T}
}
    return true
end

function _build_constraint(model::Optimizer, f::Union{MOI.SingleVariable, MOI.ScalarAffineFunction{T}}, s::CP.AntiDomain{T}) where {T <: Int}
    return cpo_java_forbiddenassignments(model.inner, _parse(model, f), collect(Int32(v) for v in s.values))
end

# CP.Membership
# TODO: not available, make a bridge for this.

# CP.DifferentFrom
function MOI.supports_constraint(::Optimizer, ::Type{F}, ::Type{S}) where {
    T <: Int,
    F <: Union{MOI.SingleVariable, MOI.ScalarAffineFunction{T}},
    S <: CP.DifferentFrom{T}
}
    return true
end

function _build_constraint(model::Optimizer, f::Union{MOI.SingleVariable, MOI.ScalarAffineFunction{T}}, s::CP.DifferentFrom{T}) where {T <: Int}
    return cpo_java_neq(model.inner, _parse(model, f), Int32(s.value))
end

# CP.Count
function MOI.supports_constraint(::Optimizer, ::Type{F}, ::Type{S}) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S <: CP.Count{T}
}
    return true
end

function _build_constraint(model::Optimizer, f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}}, s::CP.Count{T}) where {T <: Int}
    @assert MOI.output_dimension(f) >= 2

    f_parsed = _parse(model, f)
    count_assign = f_parsed[1]
    count_values = f_parsed[2:end]

    expr = cpo_java_count(model.inner, count_values, Int32(s.value))
    return cpo_java_eq(model.inner, count_assign, expr)
end

# CP.Count
function MOI.supports_constraint(::Optimizer, ::Type{F}, ::Type{S}) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S <: CP.CountDistinct
}
    return true
end

function _build_constraint(model::Optimizer, f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}}, s::CP.CountDistinct) where {T <: Int}
    @assert MOI.output_dimension(f) >= 2

    f_parsed = _parse(model, f)
    count_assign = f_parsed[1]
    count_values = f_parsed[2:end]

    expr = cpo_java_countdifferent(model.inner, count_values)
    return cpo_java_eq(model.inner, count_assign, expr)
end

# CP.Strictly
function MOI.supports_constraint(::Optimizer, ::Type{F}, ::Type{S}) where {
    T <: Int,
    Sense <: Union{MOI.LessThan{T}, MOI.GreaterThan{T}},
    F <: Union{MOI.SingleVariable, MOI.ScalarAffineFunction{T}},
    S <: CP.Strictly{T, Sense}
}
    return true
end

function _build_constraint(model::Optimizer, f::Union{MOI.SingleVariable, MOI.ScalarAffineFunction{T}}, s::CP.Strictly{T, MOI.LessThan{T}}) where {T <: Int}
    return cpo_java_lt(model.inner, _parse(model, f), cpo_java_constant(model.inner, s.set.upper))
end

function _build_constraint(model::Optimizer, f::Union{MOI.SingleVariable, MOI.ScalarAffineFunction{T}}, s::CP.Strictly{T, MOI.GreaterThan{T}}) where {T <: Int}
    return cpo_java_gt(model.inner, _parse(model, f), cpo_java_constant(model.inner, s.set.lower))
end

# CP.Element
function MOI.supports_constraint(::Optimizer, ::Type{F}, ::Type{S}) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S <: CP.Element{T}
}
    return true
end

function _build_constraint(model::Optimizer, f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}}, s::CP.Element{T}) where {T <: Int}
    # Split the dimensions in the right parts.
    f_parsed = _parse(model, f)
    element_assign = f_parsed[2]
    element_index = f_parsed[1]

    # Build the constraint.
    expr = cpo_java_element(model.inner, convert(Vector{Int32}, s.values), element_index)
    return cpo_java_eq(model.inner, element_assign, expr)
end

# CP.Sort and CP.SortPermutation are not natively supported by CPLEX.
# TODO: bridges.

# CP.BinPacking
# Unlike other CP solvers, the item weights are fixed (i.e. constant expressions, not variables).
function MOI.supports_constraint(::Optimizer, ::Type{F}, ::Type{S}) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S <: CP.BinPacking{T}
}
    return true
end

function _build_constraint(model::Optimizer, f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}}, s::CP.BinPacking{T}) where {T <: Int}
    f = MOI.Utilities.canonical(f)

    @assert MOI.output_dimension(f) == s.n_bins + s.n_items

    # Split the dimensions in the right parts.
    f_parsed = _parse(model, f)
    pack_load = f_parsed[1:s.n_bins]
    pack_assigned = f_parsed[(s.n_bins + 1) : (s.n_bins + s.n_items)]

    # Build the constraint.
    return cpo_java_pack(model.inner, pack_load, pack_assigned, convert(Vector{Int32}, s.weights))
end

# CP.CapacitatedBinPacking
# TODO: bridgde it by bounding the load variables.
# https://www.ibm.com/support/knowledgecenter/SSSA5P_12.10.0/ilog.odms.cpo.help/refjavacpoptimizer/html/ilog/cp/IloCP.html#pack(ilog.concert.IloIntExpr[],%20ilog.concert.IloIntExpr[],%20int[],%20ilog.concert.IloIntExpr)

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

# CP.MinimumDistance
function MOI.supports_constraint(::Optimizer, ::Type{F}, ::Type{S}) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S <: CP.MinimumDistance{T}
}
    return true
end

function _build_constraint(model::Optimizer, f::MOI.VectorOfVariables, s::CP.MinimumDistance{T}) where {T <: Int}
    # The Java API does not support single variables as argument, only expressions.
    return _build_constraint(model, MOI.VectorAffineFunction{T}(f), s)
end

function _build_constraint(model::Optimizer, f::MOI.VectorAffineFunction{T}, s::CP.MinimumDistance{T}) where {T <: Int}
    f_parsed = _parse(model, f)
    return cpo_java_allmindistance(model.inner, f_parsed, Int32(s.k))
end

# CP.Inverse
function MOI.supports_constraint(::Optimizer, ::Type{F}, ::Type{S}) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S <: CP.Inverse
}
    return true
end

function _build_constraint(model::Optimizer, f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}}, s::CP.Inverse) where {T <: Int}
    f_parsed = _parse(model, f)
    inverse_first = f_parsed[1:s.dimension]
    inverse_second = f_parsed[(1 + s.dimension) : (2 * s.dimension)]

    return cpo_java_inverse(model.inner, inverse_first, inverse_second)
end

# CP.LexicographicallyLessThan
function MOI.supports_constraint(::Optimizer, ::Type{F}, ::Type{S}) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S <: CP.LexicographicallyLessThan
}
    return true
end

function _build_constraint(model::Optimizer, f::MOI.VectorOfVariables, s::CP.LexicographicallyLessThan)
    # The Java API does not support single variables as argument, only expressions.
    return _build_constraint(model, MOI.VectorAffineFunction{Int}(f), s)
end

function _build_constraint(model::Optimizer, f::MOI.VectorAffineFunction{T}, s::CP.LexicographicallyLessThan) where {T <: Int}
    f_parsed = _parse(model, f)
    lex_first = f_parsed[1:s.dimension]
    lex_second = f_parsed[(s.dimension + 1) : (2 * s.dimension)]

    return cpo_java_lexicographic(model.inner, lex_first, lex_second)
end

function _build_constraint(model::Optimizer, f::MOI.VectorOfVariables, s::CP.Strictly{CP.LexicographicallyLessThan})
    # The Java API does not support single variables as argument, only expressions.
    return _build_constraint(model, MOI.VectorAffineFunction{Int}(f), s)
end

function _build_constraint(model::Optimizer, f::MOI.VectorAffineFunction{T}, s::CP.Strictly{CP.LexicographicallyLessThan}) where {T <: Int}
    f_parsed = _parse(model, f)
    lex_first = f_parsed[1:s.dimension]
    lex_second = f_parsed[(s.dimension + 1) : (2 * s.dimension)]

    return cpo_java_strictlexicographic(model.inner, lex_first, lex_second)
end
