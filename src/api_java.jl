"""
    cpo_java_init()

Initialises the JVM to be able to use CPLEX CP Optimizer.
"""
function cpo_java_init()
    JavaCall.addClassPath(libcplexcpojava)
    JavaCall.init()
end

## TODO: everything Ilo*ValueEval and Ilo*VarEval.

struct JavaCPOModel
    cp

    intvar
    intvararray
    intervalvar
    intervalsequencevar
    intervalsequencevararray
    numvar
    numvararray

    intexpr
    intexprarray
    numexpr
    numexprarray
    inttupleset
    numtonumsegmentfunction
    numtonumstepfunction
    cumulfunctionexpr
    cumulfunctionexprarray
    transitiondistance
    statefunction
    statefunctionarray

    constraint
    constraintarray
    alternative
    alternativearray
    isomorphism
    isomorphismarray
    nooverlap
    nooverlaparray
    range
    rangearray
    span
    spanarray
    synchronize
    synchronizearray

    objective
    multicriterionexpr
    solution

    addable

    callback
    conflictstatus
    conflictstatus_possible
    conflictstatus_member
    conflictstatus_excluded
end

function cpo_java_model()
    # Import the required symbols and store them in the model for future use.
    # This avoids @jimport'ing all these types for each function.
    jcp = @jimport ilog.cp.IloCP
    intvar = @jimport ilog.concert.IloIntVar
    intervalvar = @jimport ilog.concert.IloIntervalVar
    intervalsequencevar = @jimport ilog.concert.IloIntervalSequenceVar
    numvar = @jimport ilog.concert.IloNumVar

    intexpr = @jimport ilog.concert.IloIntExpr
    numexpr = @jimport ilog.concert.IloNumExpr
    inttupleset = @jimport ilog.concert.IloIntTupleSet
    numtonumsegmentfunction = @jimport ilog.concert.IloNumToNumSegmentFunction
    numtonumstepfunction = @jimport ilog.concert.IloNumToNumStepFunction
    cumulfunctionexpr = @jimport ilog.concert.IloCumulFunctionExpr
    transitiondistance = @jimport ilog.concert.IloTransitionDistance
    statefunction = @jimport ilog.concert.IloStateFunction

    constraint = @jimport ilog.concert.IloConstraint
    alternative = @jimport ilog.concert.IloAlternative
    isomorphism = @jimport ilog.concert.IloIsomorphism
    nooverlap = @jimport ilog.concert.IloNoOverlap
    range = @jimport ilog.concert.IloRange
    span = @jimport ilog.concert.IloSpan
    synchronize = @jimport ilog.concert.IloSynchronize

    objective = @jimport ilog.concert.IloObjective
    multicriterionexpr = @jimport ilog.concert.IloMultiCriterionExpr
    solution = @jimport ilog.concert.IloSolution

    addable = @jimport ilog.concert.IloAddable

    callback = @jimport ilog.cp.IloCP$Callback
    conflictstatus = @jimport ilog.cp.IloCP$ConflictStatus
    conflictstatus_possible = jfield(conflictstatus, "ConflictPossibleMember", conflictstatus)
    conflictstatus_member = jfield(conflictstatus, "ConflictMember", conflictstatus)
    conflictstatus_excluded = jfield(conflictstatus, "ConflictExcluded", conflictstatus)

    # Actually build the model.
    model = jcp(())

    # Return the right data structure
    return JavaCPOModel(model,
                        intvar, Vector{intvar}, intervalvar, intervalsequencevar,
                        Vector{intervalsequencevar}, numvar, Vector{numvar},
                        intexpr, Vector{intexpr}, numexpr, Vector{numexpr}, inttupleset,
                        numtonumsegmentfunction, numtonumstepfunction,
                        cumulfunctionexpr, Vector{cumulfunctionexpr},
                        transitiondistance, statefunction, Vector{statefunction},
                        constraint, Vector{constraint}, alternative, Vector{alternative},
                        isomorphism, Vector{isomorphism}, nooverlap, Vector{nooverlap},
                        range, Vector{range}, span, Vector{span}, synchronize, Vector{synchronize},
                        objective, multicriterionexpr, solution, addable,
                        conflictstatus, conflictstatus_possible, conflictstatus_member,
                        conflictstatus_excluded)
end

function cpo_java_release(cp::JavaCPOModel)
    jcall(cp.cp, "end", nothing, ())
end

## Variable creation

# Integer variables
function cpo_java_intvar(cp::JavaCPOModel, lb::T, ub::T, name::String="") where {T <: Integer}
    return jcall(cp.cp, "intVar", cp.intvar, (jint, jint, JString), lb, ub, name)
end

function cpo_java_intvar(cp::JavaCPOModel, values::Vector{T}, name::String="") where {T <: Integer}
    if length(name) == 0
        return jcall(cp.cp, "intVar", cp.intvar, (Vector{jint},), values)
    else
        return jcall(cp.cp, "intVar", cp.intvar, (Vector{jint}, JString), values, name)
    end
end

function cpo_java_intvararray(cp::JavaCPOModel, n::T, lb::T, ub::T, name::String="") where {T <: Integer}
    return jcall(cp.cp, "intVarArray", cp.intvararray, (jint, jint, jint, JString), n, lb, ub, name)
end

function cpo_java_intvararray(cp::JavaCPOModel, n::T, values::Vector{T}, name::String="") where {T <: Integer}
    return jcall(cp.cp, "intVarArray", cp.intvararray, (jint, Vector{jint}, JString), n, values, name)
end

# Numerical variables
function cpo_java_numvar(cp::JavaCPOModel, lb::T, ub::T, name::String="") where {T <: Real}
    return jcall(cp.cp, "numVarArray", cp.numvararray, (jint, jdouble, jdouble, JString), 1, lb, ub, name)[1]
end

function cpo_java_numvararray(cp::JavaCPOModel, n::Int, lb::T, ub::T, name::String="") where {T <: Real}
    return jcall(cp.cp, "numVarArray", cp.numvararray, (jint, jdouble, jdouble, JString), n, lb, ub, name)
end

# Interval variables
function cpo_java_intervalvar(cp::JavaCPOModel, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "intervalVar", cp.intervalvar, ())
    else
        return jcall(cp.cp, "intervalVar", cp.intervalvar, (JString,), name)
    end
end

function cpo_java_intervalvar(cp::JavaCPOModel, size::Integer, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "intervalVar", cp.intervalvar, (jint,), size)
    else
        return jcall(cp.cp, "intervalVar", cp.intervalvar, (jint, JString), size, name)
    end
end

function cpo_java_intervalvar(cp::JavaCPOModel, size_lb::Integer, size_ub::Integer)
    return jcall(cp.cp, "intervalVar", cp.intervalvar, (jint, jint), size_lb, size_ub)
end

function cpo_java_intervalvar(cp::JavaCPOModel, size_lb::Integer, size_ub::Integer, opt::Bool, intensity, granularity::Integer)
    return jcall(cp.cp, "intervalVar", cp.intervalvar, (jint, jint, jboolean, cp.numtonumstepfunction, jint), size_lb, size_ub, opt, intensity, granularity)
end

# Sequence-of-intervals variables
function cpo_java_intervalsequencevar(cp::JavaCPOModel, intervalvararray, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "intervalSequenceVar", cp.intervalsequencevar, (cp.intervalvararray,), intervalvararray)
    else
        return jcall(cp.cp, "intervalSequenceVar", cp.intervalsequencevar, (cp.intervalvararray, JString), intervalvararray, name)
    end
end

function cpo_java_intervalsequencevar(cp::JavaCPOModel, intervalvararray, types::Vector{T}, name::String="") where {T <: Integer}
    if length(name) == 0
        return jcall(cp.cp, "intervalSequenceVar", cp.intervalsequencevar, (cp.intervalvararray, Vector{jint}), intervalvararray, types)
    else
        return jcall(cp.cp, "intervalSequenceVar", cp.intervalsequencevar, (cp.intervalvararray, Vector{jint}, JString), intervalvararray, types, name)
    end
end

## Expression creation

function cpo_java_constant(cp::JavaCPOModel, value::Integer)
    return jcall(cp.cp, "constant", cp.intexpr, (jint,), value)
end

function cpo_java_constant(cp::JavaCPOModel, value::Real)
    return jcall(cp.cp, "constant", cp.numexpr, (jdouble,), value)
end

function cpo_java_count(cp::JavaCPOModel, exprs, value::Integer)
    return jcall(cp.cp, "count", cp.intexpr, (cp.intexprarray, jint), exprs, value)
end

function cpo_java_countdifferent(cp::JavaCPOModel, exprs)
    return jcall(cp.cp, "countDifferent", cp.intexpr, (cp.intexprarray,), exprs,)
end

function cpo_java_div(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "div", cp.intexpr, (cp.intexpr, cp.intexpr), expr_a, expr_b)
end

function cpo_java_div(cp::JavaCPOModel, int_a::Integer, expr_b)
    return jcall(cp.cp, "div", cp.intexpr, (jint, cp.intexpr), int_a, expr_b)
end

function cpo_java_div(cp::JavaCPOModel, expr_a, int_b::Integer)
    return jcall(cp.cp, "div", cp.intexpr, (cp.intexpr, jint), expr_a, int_b)
end

function cpo_java_element(cp::JavaCPOModel, values::Vector{T}, expr_index) where {T <: Integer}
    return jcall(cp.cp, "element", cp.intexpr, (Vector{jint}, cp.intexpr), values, expr_index)
end

function cpo_java_element(cp::JavaCPOModel, exprs::Vector, expr_index)
    return jcall(cp.cp, "element", cp.intexpr, (cp.intexprarray, cp.intexpr), exprs, expr_index)
end

function cpo_java_element(cp::JavaCPOModel, values::Vector{T}, expr_index) where {T <: Real}
    return jcall(cp.cp, "element", cp.intexpr, (Vector{jdouble}, cp.intexpr), values, expr_index)
end

function cpo_java_element(cp::JavaCPOModel, a, f)
    return jcall(cp.cp, "endEval", cp.numexpr, (cp.intervalvar, cp.numtonumsegmentfunction), a, f)
end

function cpo_java_element(cp::JavaCPOModel, a, f, absval::Real)
    return jcall(cp.cp, "endEval", cp.numexpr, (cp.intervalvar, cp.numtonumsegmentfunction, jdouble), a, f, absval)
end

function cpo_java_endof(cp::JavaCPOModel, var)
    return jcall(cp.cp, "endOf", cp.intexpr, (cp.intervalvar,), var)
end

function cpo_java_endof(cp::JavaCPOModel, var, absval)
    return jcall(cp.cp, "endOf", cp.intexpr, (cp.intervalvar, jint), var, absval::Integer)
end

function cpo_java_enfofnext(cp::JavaCPOModel, var_seq, var_interval, lastval::Integer)
    return jcall(cp.cp, "endOfNext", cp.intexpr, (cp.intervalsequencevar, cp.intervalvar, jint), var_seq, var_interval, lastval)
end

function cpo_java_enfofnext(cp::JavaCPOModel, var_seq, var_interval, lastval::Integer, absval::Integer)
    return jcall(cp.cp, "endOfNext", cp.intexpr, (cp.intervalsequencevar, cp.intervalvar, jint, jint), var_seq, var_interval, lastval, absval)
end

function cpo_java_enfofprevious(cp::JavaCPOModel, var_seq, var_interval, firstval::Integer)
    return jcall(cp.cp, "endOfPrevious", cp.intexpr, (cp.intervalsequencevar, cp.intervalvar, jint), var_seq, var_interval, firstval)
end

function cpo_java_enfofprevious(cp::JavaCPOModel, var_seq, var_interval, firstval::Integer, absval::Integer)
    return jcall(cp.cp, "endOfPrevious", cp.intexpr, (cp.intervalsequencevar, cp.intervalvar, jint, jint), var_seq, var_interval, firstval, absval)
end

function cpo_java_exponent(cp::JavaCPOModel, expr)
    return jcall(cp.cp, "exponent", cp.numexpr, (cp.numexpr,), expr)
end

function cpo_java_heightatend(cp::JavaCPOModel, a, f)
    return jcall(cp.cp, "heightAtEnd", cp.intexpr, (cp.intervalvar, cp.cumulfunctionexpr), a, f)
end

function cpo_java_heightatend(cp::JavaCPOModel, a, f, absval::Integer)
    return jcall(cp.cp, "heightAtEnd", cp.intexpr, (cp.intervalvar, cp.cumulfunctionexpr, jint), a, f, absval)
end

function cpo_java_heightatstart(cp::JavaCPOModel, a, f)
    return jcall(cp.cp, "heightAtStart", cp.intexpr, (cp.intervalvar, cp.cumulfunctionexpr), a, f)
end

function cpo_java_heightatstart(cp::JavaCPOModel, a, f, absval::Integer)
    return jcall(cp.cp, "heightAtStart", cp.intexpr, (cp.intervalvar, cp.cumulfunctionexpr, jint), a, f, absval)
end

function cpo_java_intexpr(cp::JavaCPOModel, constr)
    return jcall(cp.cp, "intExpr", cp.intexpr, (cp.constraint,), constr)
end

function cpo_java_intexprarray(cp::JavaCPOModel, n::Integer)
    return jcall(cp.cp, "intExprArray", cp.intexprarray, (jint,), n)
end

function cpo_java_lengthof(cp::JavaCPOModel, var)
    return jcall(cp.cp, "lengthOf", cp.intexpr, (cp.intervalvar,), var)
end

function cpo_java_lengthof(cp::JavaCPOModel, var, absval::Integer)
    return jcall(cp.cp, "lengthOf", cp.intexpr, (cp.intervalvar, jint), var, absval)
end

function cpo_java_lengthofnext(cp::JavaCPOModel, sequence, var, lastval::Integer)
    return jcall(cp.cp, "lengthOfNext", cp.intexpr, (cp.intervalsequencevar, cp.intervalvar, jint), sequence, var, lastval)
end

function cpo_java_lengthofnext(cp::JavaCPOModel, sequence, var, lastval::Integer, absval::Integer)
    return jcall(cp.cp, "lengthOfNext", cp.intexpr, (cp.intervalsequencevar, cp.intervalvar, jint, jint), sequence, var, lastval, absval)
end

function cpo_java_lengthofprevious(cp::JavaCPOModel, sequence, var, firstval::Integer)
    return jcall(cp.cp, "lengthOfPrevious", cp.intexpr, (cp.intervalsequencevar, cp.intervalvar, jint), sequence, var, lastval)
end

function cpo_java_lengthofprevious(cp::JavaCPOModel, sequence, var, firstval::Integer, absval::Integer)
    return jcall(cp.cp, "lengthOfPrevious", cp.intexpr, (cp.intervalsequencevar, cp.intervalvar, jint, jint), sequence, var, lastval, absval)
end

function cpo_java_log(cp::JavaCPOModel, expr)
    return jcall(cp.cp, "log", cp.numexpr, (cp.numexpr,), expr)
end

function cpo_java_max_int(cp::JavaCPOModel, exprs) # TODO: cannot use Julia method dispatch due to missing type for expressions (int/num).
    return jcall(cp.cp, "max", cp.intexpr, (cp.intexprarray,), exprs)
end

function cpo_java_max_num(cp::JavaCPOModel, exprs)
    return jcall(cp.cp, "max", cp.numexpr, (cp.numexprarray,), exprs)
end

function cpo_java_min_int(cp::JavaCPOModel, exprs) # TODO: cannot use Julia method dispatch due to missing type for expressions (int/num).
    return jcall(cp.cp, "min", cp.intexpr, (cp.intexprarray,), exprs)
end

function cpo_java_min_num(cp::JavaCPOModel, exprs)
    return jcall(cp.cp, "min", cp.numexpr, (cp.numexprarray,), exprs)
end

function cpo_java_modulo(cp::JavaCPOModel, expr, r)
    return jcall(cp.cp, "modulo", cp.intexpr, (cp.intexpr, jint), expr, r)
end

function cpo_java_numexprarray(cp::JavaCPOModel, n)
    return jcall(cp.cp, "numExprArray", cp.intexpr, (jint,), n)
end

function cpo_java_numvararray(cp::JavaCPOModel, n)
    return jcall(cp.cp, "numVarArray", cp.intexpr, (jint,), n)
end

function cpo_java_overlaplength(cp::JavaCPOModel, var_a, var_b)
    return jcall(cp.cp, "overlapLength", cp.intexpr, (cp.intervalvar, cp.intervalvar), var_a, var_b)
end

function cpo_java_overlaplength(cp::JavaCPOModel, var_a, var_b, absval::Integer)
    return jcall(cp.cp, "overlapLength", cp.intexpr, (cp.intervalvar, cp.intervalvar, jint), var_a, var_b, absval)
end

function cpo_java_overlaplength(cp::JavaCPOModel, var, start::Integer, end_::Integer)
    return jcall(cp.cp, "overlapLength", cp.intexpr, (cp.intervalvar, jint, jint), var, start, end_)
end

function cpo_java_overlaplength(cp::JavaCPOModel, var, start::Integer, end_::Integer, absval::Integer)
    return jcall(cp.cp, "overlapLength", cp.intexpr, (cp.intervalvar, jint, jint, jint), var, start, end_, absval)
end

function cpo_java_piecewiselinear(cp::JavaCPOModel, var, point::Vector{T}, slope::Vector{T}, a::Real, fa::Real) where {T <: Real}
    return jcall(cp.cp, "piecewiseLinear", cp.numexpr, (cp.numexpr, Vector{jdouble}, Vector{jdouble}, jdouble, jdouble), var, point, slope, a, fa)
end

function cpo_java_piecewiselinear(cp::JavaCPOModel, var, firstslope::Real, point::Vector{T}, value::Vector{T}, lastslope::Real) where {T <: Real}
    return jcall(cp.cp, "piecewiseLinear", cp.numexpr, (cp.numexpr, jdouble, Vector{jdouble}, Vector{jdouble}, jdouble), var, firstslope, point, value, lastslope)
end

function cpo_java_piecewiselinearfunction(cp::JavaCPOModel, point::Vector{T}, slope::Vector{T}, a::Real, fa::Real, name::String="") where {T <: Real}
    if length(name) == 0
        return jcall(cp.cp, "piecewiseLinearFunction", cp.numtonumsegmentfunction, (Vector{jdouble}, Vector{jdouble}, jdouble, jdouble), point, slope, a, fa)
    else
        return jcall(cp.cp, "piecewiseLinearFunction", cp.numtonumsegmentfunction, (Vector{jdouble}, Vector{jdouble}, jdouble, jdouble, JString), point, slope, a, fa, name)
    end
end

function cpo_java_power(cp::JavaCPOModel, expr_a::Real, expr_b)
    return jcall(cp.cp, "power", cp.numexpr, (jdouble, cp.numexpr), expr_a, expr_b)
end

function cpo_java_power(cp::JavaCPOModel, expr_a, expr_b::Real)
    return jcall(cp.cp, "power", cp.numexpr, (cp.numexpr, jdouble), expr_a, expr_b)
end

function cpo_java_power(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "power", cp.numexpr, (cp.numexpr, cp.numexpr), expr_a, expr_b)
end

function cpo_java_prod(cp::JavaCPOModel, values::Vector{T}, exprs) where {T <: Integer}
    return jcall(cp.cp, "prod", cp.intexpr, (Vector{jint}, cp.intexprarray), values, exprs)
end

function cpo_java_prod(cp::JavaCPOModel, exprs, values::Vector{T}) where {T <: Integer}
    return jcall(cp.cp, "prod", cp.intexpr, (cp.intexprarray, Vector{jint}), exprs, values)
end

function cpo_java_prod(cp::JavaCPOModel, exprs_a, exprs_b)
    return jcall(cp.cp, "prod", cp.intexpr, (cp.intexprarray, cp.intexprarray), exprs_a, exprs_b)
end

function cpo_java_pulse(cp::JavaCPOModel, var, v::Integer)
    return jcall(cp.cp, "pulse", cp.cumulfunctionexpr, (cp.intervalvar, jint), var, v)
end

function cpo_java_pulse(cp::JavaCPOModel, var, vmin::Integer, vmax::Integer)
    return jcall(cp.cp, "pulse", cp.cumulfunctionexpr, (cp.intervalvar, jint, jint), var, vmin, vmax)
end

function cpo_java_pulse(cp::JavaCPOModel, start::Integer, end_::Integer, v::Integer)
    return jcall(cp.cp, "pulse", cp.cumulfunctionexpr, (jint, jint, jint), start, end_, v)
end

function cpo_java_quot_double_numexpr(cp::JavaCPOModel, expr_a::Real, expr_b)
    return jcall(cp.cp, "quot", cp.numexpr, (jdouble, cp.numexpr), expr_a, expr_b)
end

function cpo_java_quot_numexpr_double(cp::JavaCPOModel, expr_a, expr_b::Real)
    return jcall(cp.cp, "quot", cp.numexpr, (cp.numexpr, jdouble), expr_a, expr_b)
end

function cpo_java_quot_numexpr_numexpr(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "quot", cp.numexpr, (cp.numexpr, cp.numexpr), expr_a, expr_b)
end

function cpo_java_sizeeval(cp::JavaCPOModel, a, f)
    return jcall(cp.cp, "sizeEval", cp.numexpr, (cp.intervalvar, cp.numtonumsegmentfunction), a, f)
end

function cpo_java_sizeeval(cp::JavaCPOModel, a, f, absval::Real)
    return jcall(cp.cp, "sizeEval", cp.numexpr, (cp.intervalvar, cp.numtonumsegmentfunction, jdouble), a, f, absval)
end

function cpo_java_sizeof(cp::JavaCPOModel, a)
    return jcall(cp.cp, "sizeOf", cp.numexpr, (cp.intervalvar,), a)
end

function cpo_java_sizeof(cp::JavaCPOModel, a, absval::Real)
    return jcall(cp.cp, "sizeOf", cp.numexpr, (cp.intervalvar, jdouble), a, absval)
end

function cpo_java_sizeofnext(cp::JavaCPOModel, seq, a, lastval::Integer)
    return jcall(cp.cp, "sizeOfNext", cp.intexpr, (cp.intervalsequencevar, cp.intervalvar, jint), seq, a, lastval)
end

function cpo_java_sizeofnext(cp::JavaCPOModel, seq, a, lastval::Integer, absval::Real)
    return jcall(cp.cp, "sizeOfNext", cp.intexpr, (cp.intervalsequencevar, cp.intervalvar, jint, jdouble), seq, a, lastval, absval)
end

function cpo_java_sizeofprevious(cp::JavaCPOModel, seq, a, firstval::Integer)
    return jcall(cp.cp, "sizeOfPrevious", cp.intexpr, (cp.intervalsequencevar, cp.intervalvar, jint), seq, a, firstval)
end

function cpo_java_sizeofprevious(cp::JavaCPOModel, seq, a, firstval::Integer, absval::Real)
    return jcall(cp.cp, "sizeOfPrevious", cp.intexpr, (cp.intervalsequencevar, cp.intervalvar, jint, jdouble), seq, a, firstval, absval)
end

function cpo_java_standarddeviation(cp::JavaCPOModel, exprs::Vector)
    return jcall(cp.cp, "standardDeviation", cp.numexpr, (cp.intexprarray,), exprs)
end

function cpo_java_standarddeviation(cp::JavaCPOModel, exprs::Vector, mean_lb::Real, mean_ub::Real)
    return jcall(cp.cp, "standardDeviation", cp.numexpr, (cp.intexprarray, jdouble, jdouble), exprs, mean_lb, mean_ub)
end

function cpo_java_starteval(cp::JavaCPOModel, a, f)
    return jcall(cp.cp, "startEval", cp.numexpr, (cp.intervalvar, cp.numtonumsegmentfunction), a, f)
end

function cpo_java_starteval(cp::JavaCPOModel, a, f, absval::Real)
    return jcall(cp.cp, "startEval", cp.numexpr, (cp.intervalvar, cp.numtonumsegmentfunction, jdouble), a, f, absval)
end

function cpo_java_startof(cp::JavaCPOModel, a)
    return jcall(cp.cp, "startOf", cp.numexpr, (cp.intervalvar,), a)
end

function cpo_java_startof(cp::JavaCPOModel, a, absval::Real)
    return jcall(cp.cp, "startOf", cp.numexpr, (cp.intervalvar, jdouble), a, absval)
end

function cpo_java_startofnext(cp::JavaCPOModel, seq, a, lastval)
    return jcall(cp.cp, "startOfNext", cp.numexpr, (cp.intervalsequencevar, cp.intervalvar, jint), seq, a, lastval)
end

function cpo_java_startofnext(cp::JavaCPOModel, seq, a, lastval, absval::Real)
    return jcall(cp.cp, "startOfNext", cp.numexpr, (cp.intervalsequencevar, cp.intervalvar, jint, jdouble), seq, a, lastval, absval)
end

function cpo_java_startofprevious(cp::JavaCPOModel, seq, a, firstval)
    return jcall(cp.cp, "startOfPrevious", cp.numexpr, (cp.intervalsequencevar, cp.intervalvar, jint), seq, a, firstval)
end

function cpo_java_startofprevious(cp::JavaCPOModel, seq, a, firstval, absval::Real)
    return jcall(cp.cp, "startOfPrevious", cp.numexpr, (cp.intervalsequencevar, cp.intervalvar, jint, jdouble), seq, a, firstval, absval)
end

function cpo_java_sum_int(cp::JavaCPOModel, exprs) # TODO: cannot use Julia method dispatch due to missing type for expressions (int/num).
    return jcall(cp.cp, "sum", cp.intexpr, (cp.intexprarray,), exprs)
end

function cpo_java_sum_num(cp::JavaCPOModel, exprs)
    return jcall(cp.cp, "sum", cp.numexpr, (cp.numexprarray,), exprs)
end

function cpo_java_typeofnext(cp::JavaCPOModel, seq, a, lastval::Integer)
    return jcall(cp.cp, "typeOfNext", cp.intexpr, (cp.intervalsequencevar, cp.intervalvar, jint), seq, a, lastval)
end

function cpo_java_typeofnext(cp::JavaCPOModel, seq, a, lastval::Integer, absval::Integer)
    return jcall(cp.cp, "typeOfNext", cp.intexpr, (cp.intervalsequencevar, cp.intervalvar, jint, jint), seq, a, lastval, absval)
end

function cpo_java_typeofprevious(cp::JavaCPOModel, seq, a, firstval::Integer)
    return jcall(cp.cp, "typeOfPrevious", cp.intexpr, (cp.intervalsequencevar, cp.intervalvar, jint), seq, a, firstval)
end

function cpo_java_typeofprevious(cp::JavaCPOModel, seq, a, firstval::Integer, absval::Integer)
    return jcall(cp.cp, "typeOfPrevious", cp.intexpr, (cp.intervalsequencevar, cp.intervalvar, jint, jint), seq, a, firstval, absval)
end

## TODO: IloIntTupleSet: functions

function cpo_java_numtonumsegmentfunction(cp::JavaCPOModel, dimension::Integer)
    return jcall(cp.cp, "intTable", cp.inttupleset, (jint,), dimension)
end

## TODO: IloNumToNumStepFunction and IloNumToNumSegmentFunction: functions

function cpo_java_numtonumsegmentfunction(cp::JavaCPOModel)
    return jcall(cp.cp, "numToNumSegmentFunction", cp.numtonumsegmentfunction, ())
end

function cpo_java_numtonumsegmentfunction(cp::JavaCPOModel, x::Vector{Real}, v::Vector{Real})
    return jcall(cp.cp, "numToNumSegmentFunction", cp.numtonumsegmentfunction, (Vector{jdouble}, Vector{jdouble}), x, v)
end

function cpo_java_numtonumstepfunction(cp::JavaCPOModel)
    return jcall(cp.cp, "numToNumStepFunction", cp.numtonumstepfunction, ())
end

## TODO: IloStateFunction: functions

function cpo_java_statefunction(cp::JavaCPOModel, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "stateFunction", cp.statefunction, ())
    else
        return jcall(cp.cp, "stateFunction", cp.statefunction, (JString,), name)
    end
end

function cpo_java_statefunction(cp::JavaCPOModel, t, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "stateFunction", cp.statefunction, (transitiondistance,), t)
    else
        return jcall(cp.cp, "stateFunction", cp.statefunction, (transitiondistance, JString), t, name)
    end
end

## TODO: IloCumulFunctionExpr: functions

function cpo_java_cumulfunctionexpr(cp::JavaCPOModel, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "cumulFunctionExpr", cp.cumulfunctionexpr, ())
    else
        return jcall(cp.cp, "cumulFunctionExpr", cp.cumulfunctionexpr, (JString,), name)
    end
end

function cpo_java_diff(cp::JavaCPOModel, f1, f2)
    return jcall(cp.cp, "diff", cp.cumulfunctionexpr, (cp.cumulfunctionexpr, cumulfunctionexpr), f1, f2)
end

function cpo_java_getnumberofsegments(cp::JavaCPOModel, f)
    return jcall(cp.cp, "getNumberOfSegments", jint, (cp.cumulfunctionexpr,), f)
end

function cpo_java_getsegmentstart(cp::JavaCPOModel, f, i)
    return jcall(cp.cp, "getSegmentStart", jint, (cp.cumulfunctionexpr, jint), f, i)
end

function cpo_java_getsegmentend(cp::JavaCPOModel, f, i)
    return jcall(cp.cp, "getSegmentEnd", jint, (cp.cumulfunctionexpr, jint), f, i)
end

function cpo_java_getsegmentvalue(cp::JavaCPOModel, f, i)
    return jcall(cp.cp, "getSegmentValue", jint, (cp.cumulfunctionexpr, jint), f, i)
end

function cpo_java_getvalue_cumulfunctionexpr(cp::JavaCPOModel, f, i) # TODO: cannot use Julia method dispatch due to missing type for expressions/variables (int/num).
    return jcall(cp.cp, "getSegmentValue", jint, (cp.cumulfunctionexpr, jint), f, i)
end

function cpo_java_step(cp::JavaCPOModel, t::Integer, v::Integer)
    return jcall(cp.cp, "step", cp.cumulfunctionexpr, (jint, jint), t, v)
end

function cpo_java_stepatend(cp::JavaCPOModel, a, v::Integer)
    return jcall(cp.cp, "stepAtEnd", cp.cumulfunctionexpr, (cp.intervalvar, jint), a, v)
end

function cpo_java_stepatend(cp::JavaCPOModel, a, vmin::Integer, vmax::Integer)
    return jcall(cp.cp, "stepAtEnd", cp.cumulfunctionexpr, (cp.intervalvar, jint, jint), a, vmin, vmax)
end

function cpo_java_stepatstart(cp::JavaCPOModel, a, v::Integer)
    return jcall(cp.cp, "stepAtStart", cp.cumulfunctionexpr, (cp.intervalvar, jint), a, v)
end

function cpo_java_stepatstart(cp::JavaCPOModel, a, vmin::Integer, vmax::Integer)
    return jcall(cp.cp, "stepAtStart", cp.cumulfunctionexpr, (cp.intervalvar, jint, jint), a, vmin, vmax)
end

function cpo_java_sum_cumulfunctionexpr(cp::JavaCPOModel, f1, f2) # TODO: cannot use Julia method dispatch due to missing type for expressions (int/num vs. cumul).
    return jcall(cp.cp, "sum", cp.cumulfunctionexpr, (cp.cumulfunctionexpr, cumulfunctionexpr), f1, f2)
end

## TODO: IloTransitionDistance: functions

function cpo_java_transitiondistance(cp::JavaCPOModel, i::Integer, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "transitionDistance", cp.transitiondistance, (jint,), i)
    else
        return jcall(cp.cp, "transitionDistance", cp.transitiondistance, (jint, JString), i, name)
    end
end

function cpo_java_transitiondistance(cp::JavaCPOModel, dtable::Matrix{T}, name::String="") where {T <: Integer}
    if length(name) == 0
        return jcall(cp.cp, "transitionDistance", cp.transitiondistance, (Vector{Vector{jint}},), dtable)
    else
        return jcall(cp.cp, "transitionDistance", cp.transitiondistance, (Vector{Vector{jint}}, JString), dtable, name)
    end
end

## Constraint creation

function cpo_java_alldiff(cp::JavaCPOModel, exprs)
    return jcall(cp.cp, "allDiff", cp.constraint, (cp.intexprarray,), exprs)
end

function cpo_java_allmindistance(cp::JavaCPOModel, vars)
    return jcall(cp.cp, "allMinDistance", cp.constraint, (cp.intvararray,), vars)
end

function cpo_java_allowedassignments_expr(cp::JavaCPOModel, expr, values::Vector{T}) where {T <: Integer} # TODO: cannot use Julia method dispatch due to missing type for expressions/variables.
    return jcall(cp.cp, "allowedAssignments", cp.constraint, (cp.intvararray, Vector{jint}), vars, values)
end

function cpo_java_allowedassignments_vars(cp::JavaCPOModel, vars, values)
    return jcall(cp.cp, "allowedAssignments", cp.constraint, (cp.intvararray, cp.inttupleset), vars, values)
end

function cpo_java_alternative(cp::JavaCPOModel, interval_a, intervals_b::Vector, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "alternative", cp.constraint, (cp.intervalvar, cp.intervalvararray), interval_a, intervals_b)
    else
        return jcall(cp.cp, "alternative", cp.constraint, (cp.intervalvar, cp.intervalvararray, JString), interval_a, intervals_b, name)
    end
end

function cpo_java_alternative(cp::JavaCPOModel, interval_a, intervals_b::Vector, value::Int, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "alternative", cp.constraint, (cp.intervalvar, cp.intervalvararray, jint), interval_a, intervals_b, value)
    else
        return jcall(cp.cp, "alternative", cp.constraint, (cp.intervalvar, cp.intervalvararray, jint, JString), interval_a, intervals_b, value, name)
    end
end

function cpo_java_alternative(cp::JavaCPOModel, interval_a, intervals_b::Vector, expr, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "alternative", cp.constraint, (cp.intervalvar, cp.intervalvararray, cp.intexpr), interval_a, intervals_b, expr)
    else
        return jcall(cp.cp, "alternative", cp.constraint, (cp.intervalvar, cp.intervalvararray, cp.intexpr, JString), interval_a, intervals_b, expr, name)
    end
end

function cpo_java_alwaysconstant(cp::JavaCPOModel, f, a)
    return jcall(cp.cp, "alwaysConstant", cp.constraint, (cp.statefunction, cp.intervalvar), f, a)
end

function cpo_java_alwaysconstant(cp::JavaCPOModel, f, a, startalign::Bool, endalign::Bool)
    return jcall(cp.cp, "alwaysConstant", cp.constraint, (cp.statefunction, cp.intervalvar, jboolean, jboolean), f, a, startalign, endalign)
end

function cpo_java_alwaysconstant(cp::JavaCPOModel, f, start::Integer, end_::Integer, startalign::Bool, endalign::Bool)
    return jcall(cp.cp, "alwaysConstant", cp.constraint, (cp.statefunction, jint, jint), f, start, end_)
end

function cpo_java_alwaysconstant(cp::JavaCPOModel, f, start::Integer, end_::Integer)
    return jcall(cp.cp, "alwaysConstant", cp.constraint, (cp.statefunction, jint, jint, jboolean, jboolean), f, start, end_, startalign, endalign)
end

function cpo_java_alwaysequal_cumul(cp::JavaCPOModel, f, a, v::Int) # TODO: cannot use Julia method dispatch due to missing type for expressions/functions.
    return jcall(cp.cp, "alwaysEqual", cp.constraint, (cp.cumulfunctionexpr, cp.intervalvar, jint), f, a, v)
end

function cpo_java_alwaysequal_cumul(cp::JavaCPOModel, f, start::Integer, end_::Integer, v::Int)
    return jcall(cp.cp, "alwaysEqual", cp.constraint, (cp.cumulfunctionexpr, jint, jint, jint), f, start, end_, v)
end

function cpo_java_alwaysequal_state(cp::JavaCPOModel, f, a, v::Int)
    return jcall(cp.cp, "alwaysEqual", cp.constraint, (cp.statefunction, cp.intervalvar, jint), f, a, v)
end

function cpo_java_alwaysequal_state(cp::JavaCPOModel, f, a, v::Int, startalign::Bool, endalign::Bool)
    return jcall(cp.cp, "alwaysEqual", cp.constraint, (cp.statefunction, cp.intervalvar, jint, jboolean, jboolean), f, a, v, startalign, endalign)
end

function cpo_java_alwaysequal_state(cp::JavaCPOModel, f, start::Integer, end_::Integer, v::Int, startalign::Bool, endalign::Bool)
    return jcall(cp.cp, "alwaysEqual", cp.constraint, (cp.statefunction, jint, jint, jint), f, start, end_, v)
end

function cpo_java_alwaysequal_state(cp::JavaCPOModel, f, start::Integer, end_::Integer, v::Int)
    return jcall(cp.cp, "alwaysEqual", cp.constraint, (cp.statefunction, jint, jint, jint, jboolean, jboolean), f, start, end_, v, startalign, endalign)
end

function cpo_java_alwaysin_cumul(cp::JavaCPOModel, f, a, vmin::Int, vmax::Int) # TODO: cannot use Julia method dispatch due to missing type for expressions/functions.
    return jcall(cp.cp, "alwaysIn", cp.constraint, (cp.cumulfunctionexpr, cp.intervalvar, jint, jint), f, a, vmin, vmax)
end

function cpo_java_alwaysin_cumul(cp::JavaCPOModel, f, start::Integer, end_::Integer, vmin::Int, vmax::Int)
    return jcall(cp.cp, "alwaysIn", cp.constraint, (cp.cumulfunctionexpr, jint, jint, jint, jint), f, start, end_, vmin, vmax)
end

function cpo_java_alwaysin_state(cp::JavaCPOModel, f, a, vmin::Int, vmax::Int)
    return jcall(cp.cp, "alwaysIn", cp.constraint, (cp.statefunction, cp.intervalvar, jint, jint), f, a, vmin, vmax)
end

function cpo_java_alwaysin_state(cp::JavaCPOModel, f, start::Integer, end_::Integer, vmin::Int, vmax::Int)
    return jcall(cp.cp, "alwaysIn", cp.constraint, (cp.statefunction, jint, jint, jint, jint), f, start, end_, vmin, vmax)
end

function cpo_java_alwaysnostate_state(cp::JavaCPOModel, f, a)
    return jcall(cp.cp, "alwaysNoState", cp.constraint, (cp.statefunction, cp.intervalvar, jint), f, a)
end

function cpo_java_alwaysnostate_state(cp::JavaCPOModel, f, start::Integer, end_::Integer)
    return jcall(cp.cp, "alwaysNoState", cp.constraint, (cp.statefunction, jint, jint), f, start, end_)
end

function cpo_java_before(cp::JavaCPOModel, seq, pred, succ)
    return jcall(cp.cp, "before", cp.constraint, (cp.intervalsequencevar, cp.intervalvar, cp.intervalvar), seq, pred, succ)
end

function cpo_java_distribute(cp::JavaCPOModel, exprs_cards, exprs_vars)
    return jcall(cp.cp, "distribute", cp.constraint, (cp.intexprarray, cp.intexprarray), exprs_cards, exprs_vars)
end

function cpo_java_distribute(cp::JavaCPOModel, exprs_cards, values::Vector{T}, exprs_vars) where {T <: Integer}
    return jcall(cp.cp, "distribute", cp.constraint, (cp.intexprarray, Vector{jint}, cp.intexprarray), exprs_cards, values, exprs_vars)
end

function cpo_java_element_int(cp::JavaCPOModel, expr_var, expr_index, values::Vector{T}) where {T <: Integer} # TODO: cannot use Julia method dispatch due to missing type for expressions (int/num).
    return jcall(cp.cp, "element", cp.constraint, (cp.intexpr, cp.intexpr, Vector{jint}), expr_var, expr_index, values)
end

function cpo_java_element_num(cp::JavaCPOModel, expr_var, expr_index, values::Vector{T}) where {T <: Real}
    return jcall(cp.cp, "element", cp.constraint, (cp.intexpr, cp.intexpr, Vector{jdouble}), expr_var, expr_index, values)
end

function cpo_java_endatend(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "endAtEnd", cp.constraint, (cp.intervalvar, cp.intervalvar), expr_a, expr_b)
end

function cpo_java_endatend(cp::JavaCPOModel, expr_a, expr_b, expr_z::Integer)
    return jcall(cp.cp, "endAtEnd", cp.constraint, (cp.intervalvar, cp.intervalvar, jint), expr_a, expr_b, expr_z)
end

function cpo_java_endatend(cp::JavaCPOModel, expr_a, expr_b, expr_z)
    return jcall(cp.cp, "endAtEnd", cp.constraint, (cp.intervalvar, cp.intervalvar, cp.intexpr), expr_a, expr_b, expr_z)
end

function cpo_java_endatstart(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "endAtStart", cp.constraint, (cp.intervalvar, cp.intervalvar), expr_a, expr_b)
end

function cpo_java_endatstart(cp::JavaCPOModel, expr_a, expr_b, expr_z::Integer)
    return jcall(cp.cp, "endAtStart", cp.constraint, (cp.intervalvar, cp.intervalvar, jint), expr_a, expr_b, expr_z)
end

function cpo_java_endatstart(cp::JavaCPOModel, expr_a, expr_b, expr_z)
    return jcall(cp.cp, "endAtStart", cp.constraint, (cp.intervalvar, cp.intervalvar, cp.intexpr), expr_a, expr_b, expr_z)
end

function cpo_java_endbeforeend(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "endBeforeEnd", cp.constraint, (cp.intervalvar, cp.intervalvar), expr_a, expr_b)
end

function cpo_java_endbeforeend(cp::JavaCPOModel, expr_a, expr_b, expr_z::Integer)
    return jcall(cp.cp, "endBeforeEnd", cp.constraint, (cp.intervalvar, cp.intervalvar, jint), expr_a, expr_b, expr_z)
end

function cpo_java_endbeforeend(cp::JavaCPOModel, expr_a, expr_b, expr_z)
    return jcall(cp.cp, "endBeforeEnd", cp.constraint, (cp.intervalvar, cp.intervalvar, cp.intexpr), expr_a, expr_b, expr_z)
end

function cpo_java_endbeforestart(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "endBeforeStart", cp.constraint, (cp.intervalvar, cp.intervalvar), expr_a, expr_b)
end

function cpo_java_endbeforestart(cp::JavaCPOModel, expr_a, expr_b, expr_z::Integer)
    return jcall(cp.cp, "endBeforeStart", cp.constraint, (cp.intervalvar, cp.intervalvar, jint), expr_a, expr_b, expr_z)
end

function cpo_java_endbeforestart(cp::JavaCPOModel, expr_a, expr_b, expr_z)
    return jcall(cp.cp, "endBeforeStart", cp.constraint, (cp.intervalvar, cp.intervalvar, cp.intexpr), expr_a, expr_b, expr_z)
end

function cpo_java_eq(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "eq", cp.constraint, (cp.intexpr, cp.intexpr), expr_a, expr_b)
end

function cpo_java_eq(cp::JavaCPOModel, expr_a::Integer, expr_b)
    return jcall(cp.cp, "eq", cp.constraint, (jint, cp.intexpr), expr_a, expr_b)
end

function cpo_java_eq(cp::JavaCPOModel, expr_a, expr_b::Integer)
    return jcall(cp.cp, "eq", cp.constraint, (cp.intexpr, jint), expr_a, expr_b)
end

function cpo_java_equiv(cp::JavaCPOModel, constr_a, constr_b)
    return jcall(cp.cp, "eq", cp.constraint, (cp.constraint, cp.constraint), constr_a, constr_b)
end

function cpo_java_falseconstraint(cp::JavaCPOModel)
    return jcall(cp.cp, "falseConstraint", cp.constraint, ())
end

function cpo_java_first(cp::JavaCPOModel, seq, var)
    return jcall(cp.cp, "first", cp.constraint, (cp.intervalsequencevar, cp.interval), seq, var)
end

function cpo_java_forbiddenassignments_expr(cp::JavaCPOModel, expr, values::Vector{T}) where {T <: Integer} # TODO: cannot use Julia method dispatch due to missing type for expressions/variables.
    return jcall(cp.cp, "forbiddenAssignments", cp.constraint, (cp.intvararray, Vector{jint}), vars, values)
end

function cpo_java_forbiddenassignments_vars(cp::JavaCPOModel, vars, values)
    return jcall(cp.cp, "forbiddenAssignments", cp.constraint, (cp.intvararray, cp.inttupleset), vars, values)
end

function cpo_java_forbidend(cp::JavaCPOModel, a, f)
    return jcall(cp.cp, "forbidEnd", cp.constraint, (cp.intervalvar, cp.numtonumstepfunction), a, f)
end

function cpo_java_forbidextent(cp::JavaCPOModel, a, f)
    return jcall(cp.cp, "forbidExtent", cp.constraint, (cp.intervalvar, cp.numtonumstepfunction), a, f)
end

function cpo_java_forbidstart(cp::JavaCPOModel, a, f)
    return jcall(cp.cp, "forbidStart", cp.constraint, (cp.intervalvar, cp.numtonumstepfunction), a, f)
end

function cpo_java_ge_cumul_intexpr(cp::JavaCPOModel, f, vmin) # TODO: cannot use Julia method dispatch due to missing type for variables.
    return jcall(cp.cp, "ge", cp.constraint, (cp.cumulfunctionexpr, cp.intexpr), f, vmin)
end

function cpo_java_ge_cumul_int(cp::JavaCPOModel, f, vmin::Integer)
    return jcall(cp.cp, "ge", cp.constraint, (cp.cumulfunctionexpr, jint), f, vmin)
end

function cpo_java_ge_intexpr_cumul(cp::JavaCPOModel, vmin, f)
    return jcall(cp.cp, "ge", cp.constraint, (cp.intexpr, cp.cumulfunctionexpr), vmin, f)
end

function cpo_java_ge_int_cumul(cp::JavaCPOModel, vmin::Integer, f)
    return jcall(cp.cp, "ge", cp.constraint, (jint, cp.cumulfunctionexpr), vmin, f)
end

function cpo_java_ge(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "ge", cp.constraint, (cp.intexpr, cp.intexpr), expr_a, expr_b)
end

function cpo_java_ge(cp::JavaCPOModel, expr_a::Integer, expr_b)
    return jcall(cp.cp, "ge", cp.constraint, (jint, cp.intexpr), expr_a, expr_b)
end

function cpo_java_ge(cp::JavaCPOModel, expr_a, expr_b::Integer)
    return jcall(cp.cp, "ge", cp.constraint, (cp.intexpr, jint), expr_a, expr_b)
end

function cpo_java_gt(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "gt", cp.constraint, (cp.intexpr, cp.intexpr), expr_a, expr_b)
end

function cpo_java_gt(cp::JavaCPOModel, expr_a::Integer, expr_b)
    return jcall(cp.cp, "gt", cp.constraint, (jint, cp.intexpr), expr_a, expr_b)
end

function cpo_java_gt(cp::JavaCPOModel, expr_a, expr_b::Integer)
    return jcall(cp.cp, "gt", cp.constraint, (cp.intexpr, jint), expr_a, expr_b)
end

function cpo_java_ifthenelse(cp::JavaCPOModel, constr_a, constr_b, constr_c)
    return jcall(cp.cp, "ifThenElse", cp.constraint, (cp.constraint, cp.constraint, cp.constraint), constr_a, constr_b, constr_c)
end

function cpo_java_imply(cp::JavaCPOModel, constr_a, constr_b)
    return jcall(cp.cp, "imply", cp.constraint, (cp.constraint, cp.constraint), constr_a, constr_b)
end

function cpo_java_inverse(cp::JavaCPOModel, constrs_a, constrs_b)
    return jcall(cp.cp, "inverse", cp.constraint, (cp.constraintarray, cp.constraintarray), constrs_a, constrs_b)
end

function cpo_java_isomorphism(cp::JavaCPOModel, vars_a, vars_b, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "isomorphism", cp.isomorphism, (cp.intervalvararray, cp.intervalvararray), vars_a, vars_b)
    else
        return jcall(cp.cp, "isomorphism", cp.isomorphism, (cp.intervalvararray, cp.intervalvararray, JString), vars_a, vars_b, name)
    end
end

function cpo_java_isomorphism(cp::JavaCPOModel, vars_a, vars_b, map, absval::Integer, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "isomorphism", cp.isomorphism, (cp.intervalvararray, cp.intervalvararray, cp.intaxprarray, jint), vars_a, vars_b, map, absval)
    else
        return jcall(cp.cp, "isomorphism", cp.isomorphism, (cp.intervalvararray, cp.intervalvararray, cp.intaxprarray, jint, JString), vars_a, vars_b, map, absval, name)
    end
end

function cpo_java_last(cp::JavaCPOModel, intervalseq, interval)
    return jcall(cp.cp, "last", cp.constraint, (cp.intervalsequencevar, cp.intervalvar), intervalseq, interval)
end

function cpo_java_le(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "le", cp.constraint, (cp.intexpr, cp.intexpr), expr_a, expr_b)
end

function cpo_java_le(cp::JavaCPOModel, expr_a::Integer, expr_b)
    return jcall(cp.cp, "le", cp.constraint, (jint, cp.intexpr), expr_a, expr_b)
end

function cpo_java_le(cp::JavaCPOModel, expr_a, expr_b::Integer)
    return jcall(cp.cp, "le", cp.constraint, (cp.intexpr, jint), expr_a, expr_b)
end

function cpo_java_lexicographic(cp::JavaCPOModel, exprs_a, exprs_b)
    return jcall(cp.cp, "lexicographic", cp.constraint, (cp.intexprarray, cp.intexprarray), exprs_a, exprs_b)
end

function cpo_java_lt(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "lt", cp.constraint, (cp.intexpr, cp.intexpr), expr_a, expr_b)
end

function cpo_java_lt(cp::JavaCPOModel, expr_a::Integer, expr_b)
    return jcall(cp.cp, "lt", cp.constraint, (jint, cp.intexpr), expr_a, expr_b)
end

function cpo_java_lt(cp::JavaCPOModel, expr_a, expr_b::Integer)
    return jcall(cp.cp, "lt", cp.constraint, (cp.intexpr, jint), expr_a, expr_b)
end

function cpo_java_neq(cp::JavaCPOModel, constr_a, constr_b)
    return jcall(cp.cp, "neq", cp.constraint, (cp.constraint, cp.constraint), constr_a, constr_b)
end

function cpo_java_neq(cp::JavaCPOModel, expr_a::Integer, expr_b)
    return jcall(cp.cp, "neq", cp.constraint, (jint, cp.intexpr), expr_a, expr_b)
end

function cpo_java_neq(cp::JavaCPOModel, expr_a, expr_b::Integer)
    return jcall(cp.cp, "neq", cp.constraint, (cp.intexpr, jint), expr_a, expr_b)
end

function cpo_java_nooverlap_seq(cp::JavaCPOModel, seq) # TODO: cannot use Julia method dispatch due to missing type for variables.
    return jcall(cp.cp, "noOverlap", cp.nooverlap, (cp.intervalsequencevar,), seq)
end

function cpo_java_nooverlap_seq(cp::JavaCPOModel, seq, tdist, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "noOverlap", cp.nooverlap, (cp.intervalsequencevar, cp.transitiondistance), seq, tdist)
    else
        return jcall(cp.cp, "noOverlap", cp.nooverlap, (cp.intervalsequencevar, cp.transitiondistance, JString), seq, tdist, name)
    end
end

function cpo_java_nooverlap_seq(cp::JavaCPOModel, seq, tdist, direct::Bool, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "noOverlap", cp.nooverlap, (cp.intervalsequencevar, cp.transitiondistance, jboolean), seq, tdist, direct)
    else
        return jcall(cp.cp, "noOverlap", cp.nooverlap, (cp.intervalsequencevar, cp.transitiondistance, jboolean, JString), seq, tdist, direct, name)
    end
end

function cpo_java_nooverlap_vars(cp::JavaCPOModel, vars, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "noOverlap", cp.nooverlap, (cp.intervalvararray,), vars)
    else
        return jcall(cp.cp, "noOverlap", cp.nooverlap, (cp.intervalvararray, JString), vars, name)
    end
end

function cpo_java_pack(cp::JavaCPOModel, expr_load, expr_where, weight::Vector{T}) where {T <: Integer}
    return jcall(cp.cp, "pack", cp.constraint, (cp.intexprarray, cp.intexprarray, Vector{jint}), expr_load, expr_where, weight)
end

function cpo_java_pack(cp::JavaCPOModel, expr_load, expr_where, weight::Vector{T}, used) where {T <: Integer}
    return jcall(cp.cp, "pack", cp.constraint, (cp.intexprarray, cp.intexprarray, Vector{jint}, cp.intexpr), expr_load, expr_where, weight, used)
end

function cpo_java_presenceof(cp::JavaCPOModel, var)
    return jcall(cp.cp, "presenceOf", cp.constraint, (cp.intervalvar,), var)
end

function cpo_java_previous(cp::JavaCPOModel, seq, prev, next)
    return jcall(cp.cp, "previous", cp.constraint, (cp.intervalsequencevar, cp.intervalvar, cp.intervalvar), seq, prev, next)
end

function cpo_java_range(cp::JavaCPOModel, expr, b::Real)
    return jcall(cp.cp, "range", cp.constraint, (cp.numexpr, jdouble), expr, b)
end

function cpo_java_samecommonsubsequence(cp::JavaCPOModel, seq_1::Vector, seq_2::Vector, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "sameCommonSubsequence", cp.constraint, (cp.intervalsequencevar, cp.intervalsequencevar), seq_1, seq_2)
    else
        return jcall(cp.cp, "sameCommonSubsequence", cp.constraint, (cp.intervalsequencevar, cp.intervalsequencevar, JString), seq_1, seq_2, name)
    end
end

function cpo_java_samecommonsubsequence(cp::JavaCPOModel, seq_1::Vector, seq_2::Vector, a_1::Vector, a_2::Vector, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "sameCommonSubsequence", cp.constraint, (cp.intervalsequencevar, cp.intervalsequencevar, cp.intervalvararray, cp.intervalvararray), seq_1, seq_2, a_1, a_2)
    else
        return jcall(cp.cp, "sameCommonSubsequence", cp.constraint, (cp.intervalsequencevar, cp.intervalsequencevar, cp.intervalvararray, cp.intervalvararray, JString), seq_1, seq_2, a_1, a_2, name)
    end
end

function cpo_java_samesubsequence(cp::JavaCPOModel, seq_1::Vector, seq_2::Vector, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "sameSubsequence", cp.constraint, (cp.intervalsequencevar, cp.intervalsequencevar), seq_1, seq_2)
    else
        return jcall(cp.cp, "sameSubsequence", cp.constraint, (cp.intervalsequencevar, cp.intervalsequencevar, JString), seq_1, seq_2, name)
    end
end

function cpo_java_samesubsequence(cp::JavaCPOModel, seq_1::Vector, seq_2::Vector, a_1::Vector, a_2::Vector, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "sameSubsequence", cp.constraint, (cp.intervalsequencevar, cp.intervalsequencevar, cp.intervalvararray, cp.intervalvararray), seq_1, seq_2, a_1, a_2)
    else
        return jcall(cp.cp, "sameSubsequence", cp.constraint, (cp.intervalsequencevar, cp.intervalsequencevar, cp.intervalvararray, cp.intervalvararray, JString), seq_1, seq_2, a_1, a_2, name)
    end
end

function cpo_java_sequence(cp::JavaCPOModel, nbmin::T, nbmax::T, seqwidth::T, vars::Vector, values::Vector{T}, card::Vector) where {T <: Integer}
    return jcall(cp.cp, "sequence", cp.constraint, (jint, jint, jint, cp.intvararray, Vector{jint}, cp.intvararray), nbmin, nbmax, seqwidth, vars, values, card)
end

function cpo_java_span(cp::JavaCPOModel, a, bs::Vector, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "span", cp.span, (cp.intervalvar, cp.intervalvararray), a, bs)
    else
        return jcall(cp.cp, "span", cp.span, (cp.intervalvar, cp.intervalvararray, JString), a, bs, name)
    end
end

function cpo_java_startatend(cp::JavaCPOModel, a, b)
    return jcall(cp.cp, "startAtEnd", cp.constraint, (cp.intervalvar, cp.intervalvar), a, b)
end

function cpo_java_startatend(cp::JavaCPOModel, a, b, z::Integer)
    return jcall(cp.cp, "startAtEnd", cp.constraint, (cp.intervalvar, cp.intervalvar, jint), a, b, z)
end

function cpo_java_startatend(cp::JavaCPOModel, a, b, z)
    return jcall(cp.cp, "startAtEnd", cp.constraint, (cp.intervalvar, cp.intervalvar, cp.intexpr), a, b, z)
end

function cpo_java_startatstart(cp::JavaCPOModel, a, b)
    return jcall(cp.cp, "startAtStart", cp.constraint, (cp.intervalvar, cp.intervalvar), a, b)
end

function cpo_java_startatstart(cp::JavaCPOModel, a, b, z::Integer)
    return jcall(cp.cp, "startAtStart", cp.constraint, (cp.intervalvar, cp.intervalvar, jint), a, b, z)
end

function cpo_java_startatstart(cp::JavaCPOModel, a, b, z)
    return jcall(cp.cp, "startAtStart", cp.constraint, (cp.intervalvar, cp.intervalvar, cp.intexpr), a, b, z)
end

function cpo_java_startbeforeend(cp::JavaCPOModel, a, b)
    return jcall(cp.cp, "startBeforeEnd", cp.constraint, (cp.intervalvar, cp.intervalvar), a, b)
end

function cpo_java_startbeforeend(cp::JavaCPOModel, a, b, z::Integer)
    return jcall(cp.cp, "startBeforeEnd", cp.constraint, (cp.intervalvar, cp.intervalvar, jint), a, b, z)
end

function cpo_java_startbeforeend(cp::JavaCPOModel, a, b, z)
    return jcall(cp.cp, "startBeforeEnd", cp.constraint, (cp.intervalvar, cp.intervalvar, cp.intexpr), a, b, z)
end

function cpo_java_startbeforestart(cp::JavaCPOModel, a, b)
    return jcall(cp.cp, "startBeforeStart", cp.constraint, (cp.intervalvar, cp.intervalvar), a, b)
end

function cpo_java_startbeforestart(cp::JavaCPOModel, a, b, z::Integer)
    return jcall(cp.cp, "startBeforeStart", cp.constraint, (cp.intervalvar, cp.intervalvar, jint), a, b, z)
end

function cpo_java_startbeforestart(cp::JavaCPOModel, a, b, z)
    return jcall(cp.cp, "startBeforeStart", cp.constraint, (cp.intervalvar, cp.intervalvar, cp.intexpr), a, b, z)
end

function cpo_java_strong(cp::JavaCPOModel, vars)
    return jcall(cp.cp, "strong", cp.constraint, (cp.intvararray,), vars)
end

function cpo_java_subcircuit(cp::JavaCPOModel, vars)
    return jcall(cp.cp, "subCircuit", cp.constraint, (cp.intvararray,), vars)
end

function cpo_java_synchronize(cp::JavaCPOModel, a, bs::Vector, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "synchronize", cp.synchronize, (cp.intervalvar, cp.intervalvararray), a, bs)
    else
        return jcall(cp.cp, "synchronize", cp.synchronize, (cp.intervalvar, cp.intervalvararray, JString), a, bs, name)
    end
end

function cpo_java_trueconstraint(cp::JavaCPOModel)
    return jcall(cp.cp, "trueConstraint", cp.constraint, ())
end

## Objective

function cpo_java_maximize(cp::JavaCPOModel, expr)
    return jcall(cp.cp, "maximize", cp.objective, (cp.intexpr,), expr)
end

function cpo_java_maximize_multicriterion(cp::JavaCPOModel, expr) # TODO: cannot use Julia method dispatch due to missing type for expressions.
    return jcall(cp.cp, "maximize", cp.objective, (cp.multicriterionexpr,), expr)
end

function cpo_java_minimize(cp::JavaCPOModel, expr)
    return jcall(cp.cp, "minimize", cp.objective, (cp.intexpr,), expr)
end

function cpo_java_minimize_multicriterion(cp::JavaCPOModel, expr) # TODO: cannot use Julia method dispatch due to missing type for expressions.
    return jcall(cp.cp, "minimize", cp.objective, (cp.multicriterionexpr,), expr)
end

function cpo_java_staticlex(cp::JavaCPOModel, criteria::Vector, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "staticLex", cp.multicriterionexpr, (cp.numexprarray,), criteria)
    else
        return jcall(cp.cp, "staticLex", cp.multicriterionexpr, (cp.numexprarray, JString), criteria, name)
    end
    # Other staticLex don't need to be mapped, just facility functions in Java for short arrays.
end

staticLex

## Query solution and state

function cpo_java_getallconstrainedilocumulfunctionexprs(cp::JavaCPOModel)
    return jcall(cp.cp, "getAllConstrainedIloCumulFunctionExprs", cp.cumulfunctionexprarray, ())
end

function cpo_java_getallilointervalsequencevars(cp::JavaCPOModel)
    return jcall(cp.cp, "getAllIloIntervalSequenceVars", cp.intervalsequencevararray, ())
end

function cpo_java_getallintervalvars(cp::JavaCPOModel)
    return jcall(cp.cp, "getAllIloIntervalVars", cp.intervalvararray, ())
end

function cpo_java_getallintvars(cp::JavaCPOModel)
    return jcall(cp.cp, "getAllIloIntVars", cp.intvararray, ())
end

function cpo_java_getallstatefunctions(cp::JavaCPOModel)
    return jcall(cp.cp, "getAllIloStateFunctions", cp.statefunctionarray, ())
end

function cpo_java_getallkpinames(cp::JavaCPOModel)
    return jcall(cp.cp, "getAllKPINames", Vector{JString}, ())
end

function cpo_java_getdomain(cp::JavaCPOModel, var)
    return jcall(cp.cp, "getDomain", JString, (cp.intervalvar,), var)
end

function cpo_java_getdomainsize(cp::JavaCPOModel, var)
    return jcall(cp.cp, "getDomainSize", jint, (cp.numvar,), var)
end

function cpo_java_getend(cp::JavaCPOModel, var)
    return jcall(cp.cp, "getEnd", jint, (cp.intervalvar,), var)
end

function cpo_java_getendmax(cp::JavaCPOModel, var)
    return jcall(cp.cp, "getEndMax", jint, (cp.intervalvar,), var)
end

function cpo_java_getendmin(cp::JavaCPOModel, var)
    return jcall(cp.cp, "getEndMin", jint, (cp.intervalvar,), var)
end

function cpo_java_getfirst(cp::JavaCPOModel, var)
    return jcall(cp.cp, "getFirst", cp.intervalvar, (cp.intervalsequencevar,), var)
end

function cpo_java_getincumbentvalue(cp::JavaCPOModel, expr)
    return jcall(cp.cp, "getIncumbentValue", jdouble, (cp.numexpr,), expr)
end

function cpo_java_getintvalue(cp::JavaCPOModel, expr)
    return jcall(cp.cp, "getIncumbentValue", jint, (cp.intexpr,), expr)
end

function cpo_java_getlast(cp::JavaCPOModel, var)
    return jcall(cp.cp, "getLast", cp.intervalvar, (cp.intervalsequencevar,), var)
end

function cpo_java_getlength(cp::JavaCPOModel, var)
    return jcall(cp.cp, "getLength", jint, (cp.intervalvar,), var)
end

function cpo_java_getlengthmax(cp::JavaCPOModel, var)
    return jcall(cp.cp, "getLengthMax", jint, (cp.intervalvar,), var)
end

function cpo_java_getlengthmin(cp::JavaCPOModel, var)
    return jcall(cp.cp, "getLengthMin", jint, (cp.intervalvar,), var)
end

function cpo_java_getmax(cp::JavaCPOModel, var)
    return jcall(cp.cp, "getMax", jdouble, (cp.numvar,), var)
end

function cpo_java_getmin(cp::JavaCPOModel, var)
    return jcall(cp.cp, "getMin", jdouble, (cp.numvar,), var)
end

function cpo_java_getnext(cp::JavaCPOModel, var_seq, var_interval)
    return jcall(cp.cp, "getNext", cp.intervalvar, (cp.intervalsequencevar, cp.intervalvar), var_seq, var_interval)
end

function cpo_java_getobjbound(cp::JavaCPOModel)
    return jcall(cp.cp, "getObjBound", jdouble, ())
end

function cpo_java_getobjbounds(cp::JavaCPOModel)
    return jcall(cp.cp, "getObjBounds", Vector{jdouble}, ())
end

function cpo_java_getobjgap(cp::JavaCPOModel)
    return jcall(cp.cp, "getObjGap", jdouble, ())
end

function cpo_java_getobjgaps(cp::JavaCPOModel)
    return jcall(cp.cp, "getObjGaps", Vector{jdouble}, ())
end

function cpo_java_getobjvalue(cp::JavaCPOModel)
    return jcall(cp.cp, "getObjValue", jdouble, ())
end

function cpo_java_getobjvalues(cp::JavaCPOModel)
    return jcall(cp.cp, "getObjValues", Vector{jdouble}, ())
end

function cpo_java_getprev(cp::JavaCPOModel, var_seq, var_interval)
    return jcall(cp.cp, "getPrev", cp.intervalvar, (cp.intervalsequencevar, cp.intervalvar), var_seq, var_interval)
end

function cpo_java_getsize(cp::JavaCPOModel, var)
    return jcall(cp.cp, "getSize", jint, (cp.intervalvar,), var)
end

function cpo_java_getsizemax(cp::JavaCPOModel, var)
    return jcall(cp.cp, "getSizeMax", jint, (cp.intervalvar,), var)
end

function cpo_java_getsizemin(cp::JavaCPOModel, var)
    return jcall(cp.cp, "getSizeMin", jint, (cp.intervalvar,), var)
end

function cpo_java_getstart(cp::JavaCPOModel, var)
    return jcall(cp.cp, "getStart", jint, (cp.intervalvar,), var)
end

function cpo_java_getstartmax(cp::JavaCPOModel, var)
    return jcall(cp.cp, "getStartMax", jint, (cp.intervalvar,), var)
end

function cpo_java_getstartmin(cp::JavaCPOModel, var)
    return jcall(cp.cp, "getStartMin", jint, (cp.intervalvar,), var)
end

function cpo_java_getvalue_intexpr(cp::JavaCPOModel, expr) # TODO: cannot use Julia method dispatch due to missing type for expressions/variables (int/num).
    return jcall(cp.cp, "getValue", jdouble, (cp.intexpr,), expr)
end

function cpo_java_getvalue_intvar(cp::JavaCPOModel, expr)
    return jcall(cp.cp, "getValue", jdouble, (cp.intvar,), expr)
end

function cpo_java_getvalue_numexpr(cp::JavaCPOModel, expr)
    return jcall(cp.cp, "getValue", jdouble, (cp.numexpr,), expr)
end

function cpo_java_getversion(cp::JavaCPOModel)
    return jcall(cp.cp, "getValue", JString, ())
end

function cpo_java_hasobjective(cp::JavaCPOModel)
    return jcall(cp.cp, "hasObjective", jboolean, ())
end

function cpo_java_isabsent(cp::JavaCPOModel, var)
    return jcall(cp.cp, "isAbsent", jboolean, (cp.intervalvar), var)
end

function cpo_java_isfixed_intervalsequencevar(cp::JavaCPOModel, var) # TODO: cannot use Julia method dispatch due to missing type for variables.
    return jcall(cp.cp, "isFixed", jboolean, (cp.intervalsequencevar), var)
end

function cpo_java_isfixed_intervalvar(cp::JavaCPOModel, var)
    return jcall(cp.cp, "isFixed", jboolean, (cp.intervalvar), var)
end

function cpo_java_isfixed_numvar(cp::JavaCPOModel, var)
    return jcall(cp.cp, "isFixed", jboolean, (cp.numvar), var)
end

function cpo_java_isindomain(cp::JavaCPOModel, var, value::Integer)
    return jcall(cp.cp, "isInDomain", jboolean, (cp.numvar, jint), var, value)
end

function cpo_java_ispresent(cp::JavaCPOModel, var)
    return jcall(cp.cp, "isPresent", jboolean, (cp.intervalvar,), var)
end

function cpo_java_next(cp::JavaCPOModel)
    return jcall(cp.cp, "next", jboolean, ())
end

function cpo_java_propagate(cp::JavaCPOModel)
    return jcall(cp.cp, "propagate", jboolean, ())
end

function cpo_java_propagate(cp::JavaCPOModel, constr)
    return jcall(cp.cp, "propagate", jboolean, (cp.constraint,), constr)
end

function cpo_java_refineconflict(cp::JavaCPOModel)
    return jcall(cp.cp, "refineConflict", jboolean, ())
end

function cpo_java_refineconflict(cp::JavaCPOModel, constrs)
    return jcall(cp.cp, "refineConflict", jboolean, (cp.constraintarray,), constrs)
end

function cpo_java_refineconflict(cp::JavaCPOModel, constrs, prefs::Vector{T}) where {T <: Real}
    return jcall(cp.cp, "refineConflict", jboolean, (cp.constraintarray, Vector{jdouble}), constrs, prefs)
end

function cpo_java_restore(cp::JavaCPOModel, solution)
    return jcall(cp.cp, "restore", jboolean, (cp.solution,), solution)
end

function cpo_java_solve(cp::JavaCPOModel)
    return jcall(cp.cp, "solve", jboolean, ())
end

function cpo_java_startnewsearch(cp::JavaCPOModel)
    return jcall(cp.cp, "startNewSearch", nothing, ())
end

function cpo_java_store(cp::JavaCPOModel, solution)
    return jcall(cp.cp, "store", nothing, (cp.solution,), solution)
end

## Solution IloSolution.

function cpo_java_solution(cp::JavaCPOModel, constrs)
    return jcall(cp.cp, "solution", cp.solution, ())
end

# TODO: functions of IloSolution

## Miscellaneous

function cpo_java_dumpmodel(cp::JavaCPOModel, filename::String)
    return jcall(cp.cp, "dumpModel", nothing, (JString,), filename)
    # TODO: OutputStream?
end

function cpo_java_exportmodel(cp::JavaCPOModel, filename::String)
    return jcall(cp.cp, "exportModel", nothing, (JString,), filename)
    # TODO: OutputStream?
end

function cpo_java_getbuildid(cp::JavaCPOModel)
    return jcall(cp.cp, "getBuildID", JString)
end

function cpo_java_getconflict_constraint(cp::JavaCPOModel, constr) # TODO: cannot use Julia method dispatch due to missing type for expressions (interval/num) and constraints.
    return jcall(cp.cp, "getConflict", cp.conflictstatus, (cp.constraint,), constr)
end

function cpo_java_getconflict_intervalval(cp::JavaCPOModel, var)
    return jcall(cp.cp, "getConflict", cp.conflictstatus, (cp.intervalvar,), constr)
end

function cpo_java_getconflict_numvar(cp::JavaCPOModel, var)
    return jcall(cp.cp, "getConflict", cp.conflictstatus, (cp.numvar,), constr)
end

function cpo_java_importmodel(cp::JavaCPOModel, filename::String)
    return jcall(cp.cp, "importModel", nothing, (JString,), filename)
    # TODO: InputStream?
end

function cpo_java_printinformation(cp::JavaCPOModel)
    return jcall(cp.cp, "printInformation", nothing, ())
    # TODO: OutputStream?
end

function cpo_java_remove(cp::JavaCPOModel, addable)
    return jcall(cp.cp, "remove", cp.addable, (cp.addable,), addable)
end

function cpo_java_removeallcallbacks(cp::JavaCPOModel)
    return jcall(cp.cp, "removeAllCallbacks", nothing, ())
end

function cpo_java_removeallkpis(cp::JavaCPOModel)
    return jcall(cp.cp, "removeAllKPIs", nothing, ())
end

function cpo_java_removecallback(cp::JavaCPOModel, cb)
    return jcall(cp.cp, "removeCallback", nothing, (cp.callback,), cb)
end

function cpo_java_removekpi(cp::JavaCPOModel, kpi::String)
    return jcall(cp.cp, "removeKPI", nothing, (JString,), kpi)
end

function cpo_java_runseeds(cp::JavaCPOModel, n::Integer)
    return jcall(cp.cp, "runSeeds", nothing, (jint,), n)
end

# TODO: searchPhase
