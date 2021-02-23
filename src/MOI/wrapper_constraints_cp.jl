# CP.AllDifferent
function MOI.supports_constraint(::Optimizer, ::Type{F}, ::Type{S}) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S <: CP.AllDifferent
}
    return true
end

function MOI.is_valid(model::Optimizer, c::MOI.ConstraintIndex{F, S}) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S <: CP.AllDifferent
}
    info = get(model.constraint_info, c, nothing)
    return info !== nothing && typeof(info.set) == S
end

function MOI.add_constraint(model::Optimizer, f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}}, s::CP.AllDifferent) where {T}
    index = MOI.ConstraintIndex{typeof(f), typeof(s)}(length(model.constraint_info) + 1)
    constr = cpo_java_alldiff(model.inner, _parse(model, f))
    cpo_java_add(model.inner, constr)
    model.constraint_info[index] = ConstraintInfo(index, constr, f, s)
    return index
end

# CP.Domain
function MOI.supports_constraint(::Optimizer, ::Type{F}, ::Type{S}) where {
    T <: Int,
    F <: Union{MOI.SingleVariable, MOI.ScalarAffineFunction{T}},
    S <: CP.Domain{T}
}
    return true
end

function MOI.is_valid(model::Optimizer, c::MOI.ConstraintIndex{F, S}) where {
    T <: Int,
    F <: Union{MOI.SingleVariable, MOI.ScalarAffineFunction{T}},
    S <: CP.Domain{T}
}
    info = get(model.constraint_info, c, nothing)
    return info !== nothing && typeof(info.set) == S
end

function MOI.add_constraint(model::Optimizer, f::Union{MOI.SingleVariable, MOI.ScalarAffineFunction{T}}, s::CP.Domain{T}) where {T <: Int}
    index = MOI.ConstraintIndex{typeof(f), typeof(s)}(length(model.constraint_info) + 1)
    constr = cpo_java_allowedassignments(model.inner, _parse(model, f), collect(Int32(v) for v in s.values))
    cpo_java_add(model.inner, constr)
    model.constraint_info[index] = ConstraintInfo(index, constr, f, s)
    return index
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

function MOI.is_valid(model::Optimizer, c::MOI.ConstraintIndex{F, S}) where {
    T <: Int,
    F <: Union{MOI.SingleVariable, MOI.ScalarAffineFunction{T}},
    S <: CP.DifferentFrom{T}
}
    info = get(model.constraint_info, c, nothing)
    return info !== nothing && typeof(info.set) == S
end

function MOI.add_constraint(model::Optimizer, f::Union{MOI.SingleVariable, MOI.ScalarAffineFunction{T}}, s::CP.DifferentFrom{T}) where {T <: Int}
    index = MOI.ConstraintIndex{typeof(f), typeof(s)}(length(model.constraint_info) + 1)
    constr = cpo_java_neq(model.inner, _parse(model, f), Int32(s.value))
    cpo_java_add(model.inner, constr)
    model.constraint_info[index] = ConstraintInfo(index, constr, f, s)
    return index
end

# CP.Count
function MOI.supports_constraint(::Optimizer, ::Type{F}, ::Type{S}) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S <: CP.Count{T}
}
    return true
end

function MOI.is_valid(model::Optimizer, c::MOI.ConstraintIndex{F, S}) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S <: CP.Count{T}
}
    info = get(model.constraint_info, c, nothing)
    return info !== nothing && typeof(info.set) == S
end

function MOI.add_constraint(model::Optimizer, f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}}, s::CP.Count{T}) where {T <: Int}
    @assert MOI.output_dimension(f) >= 2

    f_parsed = _parse(model, f)
    count_assign = f_parsed[1]
    count_values = f_parsed[2:end]

    index = MOI.ConstraintIndex{typeof(f), typeof(s)}(length(model.constraint_info) + 1)
    expr = cpo_java_count(model.inner, count_values, Int32(s.value))
    constr = cpo_java_eq(model.inner, count_assign, expr)
    cpo_java_add(model.inner, constr)
    model.constraint_info[index] = ConstraintInfo(index, constr, f, s)
    return index
end

# CP.Count
function MOI.supports_constraint(::Optimizer, ::Type{F}, ::Type{S}) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S <: CP.CountDistinct
}
    return true
end

function MOI.is_valid(model::Optimizer, c::MOI.ConstraintIndex{F, S}) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S <: CP.CountDistinct
}
    info = get(model.constraint_info, c, nothing)
    return info !== nothing && typeof(info.set) == S
end

function MOI.add_constraint(model::Optimizer, f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}}, s::CP.CountDistinct) where {T <: Int}
    @assert MOI.output_dimension(f) >= 2

    f_parsed = _parse(model, f)
    count_assign = f_parsed[1]
    count_values = f_parsed[2:end]

    index = MOI.ConstraintIndex{typeof(f), typeof(s)}(length(model.constraint_info) + 1)
    expr = cpo_java_countdifferent(model.inner, count_values)
    constr = cpo_java_eq(model.inner, count_assign, expr)
    cpo_java_add(model.inner, constr)
    model.constraint_info[index] = ConstraintInfo(index, constr, f, s)
    return index
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

function MOI.is_valid(model::Optimizer, c::MOI.ConstraintIndex{F, S}) where {
    T <: Int,
    Sense <: Union{MOI.LessThan{T}, MOI.GreaterThan{T}},
    F <: Union{MOI.SingleVariable, MOI.ScalarAffineFunction{T}},
    S <: CP.Strictly{T, Sense}
}
    info = get(model.constraint_info, c, nothing)
    return info !== nothing && typeof(info.set) == S
end

function MOI.add_constraint(model::Optimizer, f::Union{MOI.SingleVariable, MOI.ScalarAffineFunction{T}}, s::CP.Strictly{T, MOI.LessThan{T}}) where {T <: Int}
    index = MOI.ConstraintIndex{typeof(f), typeof(s)}(length(model.constraint_info) + 1)
    constr = cpo_java_lt(model.inner, _parse(model, f), cpo_java_constant(model.inner, s.set.upper))
    cpo_java_add(model.inner, constr)
    model.constraint_info[index] = ConstraintInfo(index, constr, f, s)
    return index
end

function MOI.add_constraint(model::Optimizer, f::Union{MOI.SingleVariable, MOI.ScalarAffineFunction{T}}, s::CP.Strictly{T, MOI.GreaterThan{T}}) where {T <: Int}
    index = MOI.ConstraintIndex{typeof(f), typeof(s)}(length(model.constraint_info) + 1)
    constr = cpo_java_gt(model.inner, _parse(model, f), cpo_java_constant(model.inner, s.set.lower))
    cpo_java_add(model.inner, constr)
    model.constraint_info[index] = ConstraintInfo(index, constr, f, s)
    return index
end

# CP.Element
function MOI.supports_constraint(::Optimizer, ::Type{F}, ::Type{S}) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S <: CP.Element{T}
}
    return true
end

function MOI.is_valid(model::Optimizer, c::MOI.ConstraintIndex{F, S}) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S <: CP.Element{T}
}
    info = get(model.constraint_info, c, nothing)
    return info !== nothing && typeof(info.set) == S
end

function MOI.add_constraint(model::Optimizer, f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}}, s::CP.Element{T}) where {T <: Int}
    # Split the dimensions in the right parts.
    f_parsed = _parse(model, f)
    element_assign = f_parsed[2]
    element_index = f_parsed[1]

    # Build the constraint.
    index = MOI.ConstraintIndex{typeof(f), typeof(s)}(length(model.constraint_info) + 1)
    expr = cpo_java_element(model.inner, convert(Vector{Int32}, s.values), element_index)
    constr = cpo_java_eq(model.inner, element_assign, expr)
    cpo_java_add(model.inner, constr)
    model.constraint_info[index] = ConstraintInfo(index, constr, f, s)
    return index
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

function MOI.is_valid(model::Optimizer, c::MOI.ConstraintIndex{F, S}) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S <: CP.BinPacking{T}
}
    info = get(model.constraint_info, c, nothing)
    return info !== nothing && typeof(info.set) == S
end

function MOI.add_constraint(model::Optimizer, f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}}, s::CP.BinPacking{T}) where {T <: Int}
    f = MOI.Utilities.canonical(f)

    @assert MOI.output_dimension(f) == s.n_bins + s.n_items

    # Split the dimensions in the right parts.
    f_parsed = _parse(model, f)
    pack_load = f_parsed[1:s.n_bins]
    pack_assigned = f_parsed[(s.n_bins + 1) : (s.n_bins + s.n_items)]

    # Build the constraint.
    index = MOI.ConstraintIndex{typeof(f), typeof(s)}(length(model.constraint_info) + 1)
    constr = cpo_java_pack(model.inner, pack_load, pack_assigned, convert(Vector{Int32}, s.weights))
    cpo_java_add(model.inner, constr)
    model.constraint_info[index] = ConstraintInfo(index, constr, f, s)
    return index
end

# CP.CapacitatedBinPacking
# TODO: bridgde it by bounding the load variables.
# https://www.ibm.com/support/knowledgecenter/SSSA5P_12.10.0/ilog.odms.cpo.help/refjavacpoptimizer/html/ilog/cp/IloCP.html#pack(ilog.concert.IloIntExpr[],%20ilog.concert.IloIntExpr[],%20int[],%20ilog.concert.IloIntExpr)