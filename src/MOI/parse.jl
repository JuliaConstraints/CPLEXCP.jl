## Expression parsing (not part of MOI API)

function _parse(::Optimizer, expr)
    error("_parse not yet implemented for type: $(typeof(expr))")
    return
end

function _parse(model::Optimizer, f::MOI.VariableIndex)
    # A Concert Variable is already an expression for CPLEX.
    return _info(model, f).variable
end

function _parse(model::Optimizer, f::MOI.VectorOfVariables)
    return [_parse(model, v) for v in f.variables]
end

function _parse(
    model::Optimizer,
    terms::Vector{MOI.ScalarAffineTerm{T}},
) where {T <: Integer}
    cp = model.inner
    coeffs = Int32[t.coefficient for t in terms] # Type is forced by JNI.
    vars = IloIntVar[_parse(model, t.variable_index) for t in terms]
    return cpo_java_scalprod(cp, coeffs, vars)
end

function _parse(
    model::Optimizer,
    terms::Vector{MOI.ScalarAffineTerm{T}},
) where {T <: Real}
    cp = model.inner
    coeffs = T[t.coefficient for t in terms]
    vars = NumVar[_parse(model, t.variable_index) for t in terms]
    return cpo_java_scalprod(cp, coeffs, vars)
end

function _parse(
    model::Optimizer,
    terms::Vector{MOI.ScalarQuadraticTerm{T}},
) where {T <: Real}
    # IloCP.scalprod only works for unweighted vectors of variables, which is not always the case here.
    cp = model.inner

    function prod_vars(term::MOI.ScalarQuadraticTerm{T})
        return cpo_java_prod(
            cp,
            _parse(model, term.variable_index_1),
            _parse(model, term.variable_index_2),
        )
    end
    function prod(term::MOI.ScalarQuadraticTerm{T})
        return cpo_java_prod(cp, term.coefficient, prod_vars(term))
    end

    e = prod(terms[1])
    for t in terms[2:end]
        e = cpo_java_sum(cp, e, prod(t))
    end
    return e
end

function _parse(
    model::Optimizer,
    f::MOI.ScalarAffineFunction{T},
) where {T <: Real}
    f = MOI.Utilities.canonical(f)
    e = _parse(model, f.terms)
    if !iszero(f.constant)
        cp = model.inner
        e = cpo_java_sum(cp, e, cpo_java_constant(cp, f.constant))
    end
    return e
end

function _parse(
    model::Optimizer,
    f::MOI.VectorAffineFunction{T},
) where {T <: Real}
    f = MOI.Utilities.canonical(f)
    cp = model.inner
    es = [
        cpo_java_constant(cp, f.constants[idx]) for
        idx in 1:MOI.output_dimension(f)
    ]
    for i in 1:MOI.output_dimension(f)
        if i <= length(f.terms)
            idx = f.terms[i].output_index
            term = f.terms[i].scalar_term
            es[idx] = cpo_java_sum(
                cp,
                [
                    es[idx],
                    cpo_java_prod(
                        cp,
                        IntExpr[cpo_java_constant(cp, term.coefficient)],
                        IntExpr[_parse(model, term.variable_index)],
                    ),
                ],
            )
        end
    end
    return es
end

function _parse(
    model::Optimizer,
    f::MOI.ScalarQuadraticFunction{T},
) where {T <: Real}
    f = MOI.Utilities.canonical(f)
    cp = model.inner
    e = _parse(model, f.quadratic_terms)
    if length(f.affine_terms) > 0
        e = cpo_java_sum(cp, e, _parse(model, f.affine_terms))
    end
    if !iszero(f.constant)
        e = cpo_java_sum(cp, e, cpo_java_constant(cp, f.constant))
    end
    return e
end

function _parse(
    model::Optimizer,
    f::MOI.VectorOfVariables,
    ::MOI.SecondOrderCone,
)
    # SOC is the cone: t ≥ ||x||₂ ≥ 0. In quadratic form, this is
    # t² - Σᵢ xᵢ² ≥ 0 and t ≥ 0.
    # This function returns the expression t² - Σᵢ xᵢ².
    vars = [_info(model, v) for v in f.variables]
    e = cpo_java_square(model, vars[1].variable)
    for i in 2:length(vars)
        e = cpo_java_diff(model, e, vars[i].variable)
    end
    return e
end
