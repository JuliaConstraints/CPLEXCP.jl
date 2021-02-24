# Generic part
function MOI.is_valid(model::Optimizer, c::MOI.ConstraintIndex{F, S}) where {F <: MOI.AbstractFunction, S <: MOI.AbstractSet}
    info = get(model.constraint_info, c, nothing)
    return info !== nothing && typeof(info.set) == S
end

function MOI.add_constraint(model::Optimizer, f::F, s::S) where {F <: MOI.AbstractFunction, S <: MOI.AbstractSet}
    index = MOI.ConstraintIndex{F, S}(length(model.constraint_info) + 1)
    constr = _build_constraint(model, f, s)
    cpo_java_add(model.inner, constr)
    model.constraint_info[index] = ConstraintInfo(index, constr, f, s)
    return index
end

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
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S <: ReificationSet{S2}
    S2 <: MOI.AbstractSet
}
    return MOI.supports_constraint(o, f, S2)
end

function _build_constraint(model::Optimizer, f::MOI.VectorOfVariables, s::CP.ReificationSet{S2}) where S2 <: MOI.AbstractSet
    # Split the dimensions in the right parts.
    f_parsed = _parse(model, f)
    reify_indicator = f_parsed[1]
    reify_set_variables_raw = MOI.VectorOfVariables(f.variables[2:end])

    # Build the constraint.
    indicator = cpo_java_eq(model.inner, reify_indicator, Int32(true))
    set = _build_constraint(model, reify_set_variables_raw, s.set)
    return cpo_java_equiv(model.inner, indicator, set)
end

function _build_constraint(model::Optimizer, f::MOI.VectorAffineFunction{T}, s::CP.ReificationSet{S2}) where {T <: Int, S2 <: MOI.AbstractSet}
    # Split the dimensions in the right parts.
    f_parsed = _parse(model, f)
    reify_indicator = f_parsed[1]
    reify_set_variables_raw = MOI.VectorAffineFunction(f.terms[2:end], f.constants[2:end])

    # Build the constraint.
    indicator = cpo_java_eq(model.inner, reify_indicator, Int32(true))
    set = _build_constraint(model, reify_set_variables_raw, s.set)
    return cpo_java_equiv(model.inner, indicator, set)
end
