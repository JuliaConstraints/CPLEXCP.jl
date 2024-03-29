# CP.AllDifferent
function MOI.supports_constraint(
    ::Optimizer,
    ::Type{F},
    ::Type{S},
) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S <: CP.AllDifferent,
}
    return true
end

function _build_constraint(
    model::Optimizer,
    f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    ::CP.AllDifferent,
) where {T}
    return cpo_java_alldiff(model.inner, _parse(model, f))
end

# CP.Domain
function MOI.supports_constraint(
    ::Optimizer,
    ::Type{F},
    ::Type{S},
) where {
    T <: Int,
    F <: Union{MOI.VariableIndex, MOI.ScalarAffineFunction{T}},
    S <: CP.Domain{T},
}
    return true
end

function _build_constraint(
    model::Optimizer,
    f::Union{MOI.VariableIndex, MOI.ScalarAffineFunction{T}},
    s::CP.Domain{T},
) where {T <: Int}
    return cpo_java_allowedassignments(
        model.inner,
        _parse(model, f),
        collect(Int32(v) for v in s.values),
    )
end

# CP.AntiDomain
function MOI.supports_constraint(
    ::Optimizer,
    ::Type{F},
    ::Type{S},
) where {
    T <: Int,
    F <: Union{MOI.VariableIndex, MOI.ScalarAffineFunction{T}},
    S <: CP.AntiDomain{T},
}
    return true
end

function _build_constraint(
    model::Optimizer,
    f::Union{MOI.VariableIndex, MOI.ScalarAffineFunction{T}},
    s::CP.AntiDomain{T},
) where {T <: Int}
    return cpo_java_forbiddenassignments(
        model.inner,
        _parse(model, f),
        collect(Int32(v) for v in s.values),
    )
end

# CP.DifferentFrom
function MOI.supports_constraint(
    ::Optimizer,
    ::Type{F},
    ::Type{S},
) where {
    T <: Int,
    F <: Union{MOI.VariableIndex, MOI.ScalarAffineFunction{T}},
    S <: CP.DifferentFrom{T},
}
    return true
end

function _build_constraint(
    model::Optimizer,
    f::Union{MOI.VariableIndex, MOI.ScalarAffineFunction{T}},
    s::CP.DifferentFrom{T},
) where {T <: Int}
    return cpo_java_neq(model.inner, _parse(model, f), Int32(s.value))
end

# CP.Count
function MOI.supports_constraint(
    ::Optimizer,
    ::Type{F},
    ::Type{S},
) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S <: CP.Count{MOI.EqualTo{T}},
}
    return true
end

function _build_constraint(
    model::Optimizer,
    f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    s::CP.Count{MOI.EqualTo{T}},
) where {T <: Int}
    @assert MOI.output_dimension(f) >= 2

    f_parsed = _parse(model, f)
    count_assign = f_parsed[1]
    count_values = f_parsed[2:end]

    expr = cpo_java_count(model.inner, count_values, Int32(s.set.value))
    return cpo_java_eq(model.inner, count_assign, expr)
end

# CP.CountDistinct
function MOI.supports_constraint(
    ::Optimizer,
    ::Type{F},
    ::Type{S},
) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S <: CP.CountDistinct,
}
    return true
end

function _build_constraint(
    model::Optimizer,
    f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    ::CP.CountDistinct,
) where {T <: Int}
    @assert MOI.output_dimension(f) >= 2

    f_parsed = _parse(model, f)
    count_assign = f_parsed[1]
    count_values = f_parsed[2:end]

    expr = cpo_java_countdifferent(model.inner, count_values)
    return cpo_java_eq(model.inner, count_assign, expr)
end

# CP.Strictly
function MOI.supports_constraint(
    ::Optimizer,
    ::Type{F},
    ::Type{S},
) where {
    T <: Int,
    Sense <: Union{MOI.LessThan{T}, MOI.GreaterThan{T}},
    F <: Union{MOI.VariableIndex, MOI.ScalarAffineFunction{T}},
    S <: CP.Strictly{Sense, T},
}
    return true
end

function _build_constraint(
    model::Optimizer,
    f::Union{MOI.VariableIndex, MOI.ScalarAffineFunction{T}},
    s::CP.Strictly{MOI.LessThan{T}, T},
) where {T <: Int}
    return cpo_java_lt(
        model.inner,
        _parse(model, f),
        cpo_java_constant(model.inner, s.set.upper),
    )
end

function _build_constraint(
    model::Optimizer,
    f::Union{MOI.VariableIndex, MOI.ScalarAffineFunction{T}},
    s::CP.Strictly{MOI.GreaterThan{T}, T},
) where {T <: Int}
    return cpo_java_gt(
        model.inner,
        _parse(model, f),
        cpo_java_constant(model.inner, s.set.lower),
    )
end

# CP.Element
function MOI.supports_constraint(
    ::Optimizer,
    ::Type{F},
    ::Type{S},
) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S <: CP.Element{T},
}
    return true
end

function _build_constraint(
    model::Optimizer,
    f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    s::CP.Element{T},
) where {T <: Int}
    # Split the dimensions in the right parts.
    f_parsed = _parse(model, f)
    element_assign = f_parsed[2]
    element_index = f_parsed[1]

    # Build the constraint.
    expr = cpo_java_element(
        model.inner,
        convert(Vector{Int32}, s.values),
        element_index,
    )
    return cpo_java_eq(model.inner, element_assign, expr)
end

# CP.BinPacking
# Unlike other CP solvers, the item weights are fixed (i.e. constant expressions, not variables).
function MOI.supports_constraint(
    ::Optimizer,
    ::Type{F},
    ::Type{S},
) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S <: CP.BinPacking{CP.NO_CAPACITY_BINPACKING, T},
}
    return true
end

function _build_constraint(
    model::Optimizer,
    f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    s::CP.BinPacking{CP.NO_CAPACITY_BINPACKING, T},
) where {T <: Int}
    f = MOI.Utilities.canonical(f)

    @assert MOI.output_dimension(f) == s.n_bins + s.n_items

    # Split the dimensions in the right parts.
    f_parsed = _parse(model, f)
    pack_load = f_parsed[1:(s.n_bins)]
    pack_assigned = f_parsed[(s.n_bins + 1):(s.n_bins + s.n_items)]

    # Build the constraint.
    return cpo_java_pack(
        model.inner,
        pack_load,
        pack_assigned,
        convert(Vector{Int32}, s.weights),
    )
end

# CP.MinimumDistance
function MOI.supports_constraint(
    ::Optimizer,
    ::Type{F},
    ::Type{S},
) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S <: CP.MinimumDistance{T},
}
    return true
end

function _build_constraint(
    model::Optimizer,
    f::MOI.VectorOfVariables,
    s::CP.MinimumDistance{T},
) where {T <: Int}
    # The Java API does not support single variables as argument, only expressions.
    return _build_constraint(model, MOI.VectorAffineFunction{T}(f), s)
end

function _build_constraint(
    model::Optimizer,
    f::MOI.VectorAffineFunction{T},
    s::CP.MinimumDistance{T},
) where {T <: Int}
    f_parsed = _parse(model, f)
    return cpo_java_allmindistance(model.inner, f_parsed, Int32(s.k))
end

# CP.Inverse
function MOI.supports_constraint(
    ::Optimizer,
    ::Type{F},
    ::Type{S},
) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S <: CP.Inverse,
}
    return true
end

function _build_constraint(
    model::Optimizer,
    f::Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    s::CP.Inverse,
) where {T <: Int}
    f_parsed = _parse(model, f)

    # Map indices between Julia (starts at 1) and CPLEX (starts at 0).
    f_parsed = [cpo_java_diff(model.inner, f_p, Int32(1)) for f_p in f_parsed]

    inverse_first = f_parsed[1:(s.dimension)]
    inverse_second = f_parsed[(1 + s.dimension):(2 * s.dimension)]

    return cpo_java_inverse(model.inner, inverse_first, inverse_second)
end

# CP.LexicographicallyLessThan
function MOI.supports_constraint(
    ::Optimizer,
    ::Type{F},
    ::Type{S},
) where {
    T <: Int,
    F <: Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}},
    S <: Union{
        CP.LexicographicallyLessThan,
        CP.Strictly{CP.LexicographicallyLessThan},
    },
}
    return true
end

function _build_constraint(
    model::Optimizer,
    f::MOI.VectorOfVariables,
    s::CP.LexicographicallyLessThan,
)
    # The Java API does not support single variables as argument, only expressions.
    return _build_constraint(model, MOI.VectorAffineFunction{Int}(f), s)
end

function _build_constraint(
    model::Optimizer,
    f::MOI.VectorAffineFunction{T},
    s::CP.LexicographicallyLessThan,
) where {T <: Int}
    # _build_constraint can only return one constraint at a time, CPLEX only 
    # has lexicographic constraints between two vectors.
    @assert s.row_dim == 2

    f_parsed = _parse(model, f)
    lex_first = f_parsed[1:(s.column_dim)]
    lex_second = f_parsed[(s.column_dim + 1):(2 * s.column_dim)]

    return cpo_java_lexicographic(model.inner, lex_first, lex_second)
end

function _build_constraint(
    model::Optimizer,
    f::MOI.VectorOfVariables,
    s::CP.Strictly{CP.LexicographicallyLessThan},
)
    # The Java API does not support single variables as argument, only expressions.
    return _build_constraint(model, MOI.VectorAffineFunction{Int}(f), s)
end

function _build_constraint(
    model::Optimizer,
    f::MOI.VectorAffineFunction{T},
    s::CP.Strictly{CP.LexicographicallyLessThan},
) where {T <: Int}
    # _build_constraint can only return one constraint at a time, CPLEX only 
    # has strict lexicographic constraints between two vectors.
    @assert s.set.row_dim == 2

    f_parsed = _parse(model, f)
    lex_first = f_parsed[1:(s.set.column_dim)]
    lex_second = f_parsed[(s.set.column_dim + 1):(2 * s.set.column_dim)]

    return cpo_java_strictlexicographic(model.inner, lex_first, lex_second)
end
