"""
    cpo_java_init()

Initialises the JVM to be able to use CPLEX CP Optimizer.
"""
function cpo_java_init()
    JavaCall.addClassPath(libcplexcpojava)
    JavaCall.init()
end

mutable struct JavaCPOModel # TODO: mutable required?
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

    constraint
    constraintarray
    alternative
    alternativearray
    isomorphism
    isomorphismarray
    nooverlap
    nooverlaparray

    objective
    multicriterionexpr
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

    constraint = @jimport ilog.concert.IloConstraint
    alternative = @jimport ilog.concert.IloAlternative
    isomorphism = @jimport ilog.concert.IloIsomorphism
    nooverlap = @jimport ilog.concert.IloNoOverlap

    objective = @jimport ilog.concert.IloObjective
    multicriterionexpr = @jimport ilog.concert.IloMultiCriterionExpr

    # Actually build the model.
    model = jcp(())

    # Return the right data structure
    return JavaCPOModel(model,
                        intvar, Vector{intvar}, intervalvar, intervalsequencevar,
                        Vector{intervalsequencevar}, numvar, Vector{numvar},
                        intexpr, Vector{intexpr}, numexpr, Vector{numexpr}, inttupleset,
                        numtonumsegmentfunction, numtonumstepfunction,
                        constraint, Vector{constraint}, alternative, Vector{alternative},
                        isomorphism, Vector{isomorphism}, nooverlap, Vector{nooverlap},
                        objective, multicriterionexpr)
end

function cpo_java_release(cp::JavaCPOModel)
    jcall(cp.cp, "end", nothing, ())
end

## Variable creation

# Integer variables
function cpo_java_intvar_bounded(cp::JavaCPOModel, lb::T, ub::T, name::String="") where {T <: Integer}
    return jcall(cp.cp, "intVar", cp.intvar, (jint, jint, JString), lb, ub, name)
end

function cpo_java_intvar_discrete(cp::JavaCPOModel, values::Vector{T}, name::String="") where {T <: Integer}
    if length(name) == 0
        return jcall(cp.cp, "intVar", cp.intvar, (Vector{jint},), values)
    else
        return jcall(cp.cp, "intVar", cp.intvar, (Vector{jint}, JString), values, name)
    end
end

function cpo_java_intvararray_bounded(cp::JavaCPOModel, n::T, lb::T, ub::T, name::String="") where {T <: Integer}
    return jcall(cp.cp, "intVarArray", cp.intvararray, (jint, jint, jint, JString), n, lb, ub, name)
end

function cpo_java_intvararray_discrete(cp::JavaCPOModel, n::T, values::Vector{T}, name::String="") where {T <: Integer}
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

function cpo_java_intervalvar_fixedsize(cp::JavaCPOModel, size::Integer, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "intervalVar", cp.intervalvar, (jint,), size)
    else
        return jcall(cp.cp, "intervalVar", cp.intervalvar, (jint, JString), size, name)
    end
end

function cpo_java_intervalvar_boundedsize(cp::JavaCPOModel, size_lb::Integer, size_ub::Integer)
    return jcall(cp.cp, "intervalVar", cp.intervalvar, (jint, jint), size_lb, size_ub)
end

# TODO: public IloIntervalVar intervalVar(int szmin, int szmax, boolean opt, IloNumToNumStepFunction intensity, int granularity)
# Requires building IloNumToNumStepFunction objects with some API.

# Sequence-of-intervals variables
function cpo_java_intervalsequencevar(cp::JavaCPOModel, intervalvararray, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "intervalSequenceVar", cp.intervalsequencevar, (cp.intervalvararray,), intervalvararray)
    else
        return jcall(cp.cp, "intervalSequenceVar", cp.intervalsequencevar, (cp.intervalvararray, JString), intervalvararray, name)
    end
end

function cpo_java_intervalsequencevar_int(cp::JavaCPOModel, intervalvararray, types::Vector{<:Integer}, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "intervalSequenceVar", cp.intervalsequencevar, (cp.intervalvararray, Vector{jint}), intervalvararray, types)
    else
        return jcall(cp.cp, "intervalSequenceVar", cp.intervalsequencevar, (cp.intervalvararray, Vector{jint}, JString), intervalvararray, types, name)
    end
end

## Expression creation

function cpo_java_constant_int(cp::JavaCPOModel, value::Integer)
    return jcall(cp.cp, "constant", cp.intexpr, (jint,), value)
end

function cpo_java_constant_num(cp::JavaCPOModel, value::Real)
    return jcall(cp.cp, "constant", cp.numexpr, (jdouble,), value)
end

function cpo_java_div_expr_expr(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "div", cp.intexpr, (cp.intexpr, cp.intexpr), expr_a, expr_b)
end

function cpo_java_div_int_expr(cp::JavaCPOModel, int_a::Integer, expr_b)
    return jcall(cp.cp, "div", cp.intexpr, (jint, cp.intexpr), int_a, expr_b)
end

function cpo_java_div_expr_int(cp::JavaCPOModel, expr_a, int_b::Integer)
    return jcall(cp.cp, "div", cp.intexpr, (cp.intexpr, jint), expr_a, int_b)
end

# TODO
# In particular, these functions require IloCumulFunctionExpr:
# TODO: cumulFunctionExpr
# TODO: diff

function cpo_java_count(cp::JavaCPOModel, exprs, value::Integer)
    return jcall(cp.cp, "count", cp.intexpr, (cp.intexprarray, jint), exprs, value)
end

function cpo_java_countdifferent(cp::JavaCPOModel, exprs)
    return jcall(cp.cp, "countDifferent", cp.intexpr, (cp.intexprarray,), exprs,)
end

function cpo_java_element_int(cp::JavaCPOModel, values::Vector{<:Integer}, expr_index)
    return jcall(cp.cp, "element", cp.intexpr, (Vector{jint}, cp.intexpr), values, expr_index)
end

function cpo_java_element_intexpr(cp::JavaCPOModel, exprs::Vector, expr_index)
    return jcall(cp.cp, "element", cp.intexpr, (cp.intexprarray, cp.intexpr), exprs, expr_index)
end

function cpo_java_element_num(cp::JavaCPOModel, values::Vector{<:Real}, expr_index)
    return jcall(cp.cp, "element", cp.intexpr, (Vector{jdouble}, cp.intexpr), values, expr_index)
end

# TODO: requiring IloNumToNumSegmentFunction: endEval

function cpo_java_endof(cp::JavaCPOModel, var)
    return jcall(cp.cp, "endOf", cp.intexpr, (cp.intervalvar,), var)
end

function cpo_java_endof_int(cp::JavaCPOModel, var, absval)
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

# TODO: heightAtEnd, heightAtStart

function cpo_java_intexpr(cp::JavaCPOModel, constr)
    return jcall(cp.cp, "intExpr", cp.intexpr, (cp.constraint,), constr)
end

function cpo_java_intexprarray(cp::JavaCPOModel, n::Integer)
    return jcall(cp.cp, "intExprArray", cp.intexprarray, (jint,), n)
end

function cpo_java_lengthof(cp::JavaCPOModel, var)
    return jcall(cp.cp, "lengthOf", cp.intexpr, (cp.intervalvar,), var)
end

function cpo_java_lengthof_int(cp::JavaCPOModel, var, absval)
    return jcall(cp.cp, "lengthOf", cp.intexpr, (cp.intervalvar, jint), var, absval)
end

function cpo_java_lengthofnext(cp::JavaCPOModel, sequence, var, lastval)
    return jcall(cp.cp, "lengthOfNext", cp.intexpr, (cp.intervalsequencevar, cp.intervalvar, jint), sequence, var, lastval)
end

function cpo_java_lengthofnext_int(cp::JavaCPOModel, sequence, var, lastval, absval)
    return jcall(cp.cp, "lengthOfNext", cp.intexpr, (cp.intervalsequencevar, cp.intervalvar, jint, jint), sequence, var, lastval, absval)
end

function cpo_java_lengthofprevious(cp::JavaCPOModel, sequence, var, firstval)
    return jcall(cp.cp, "lengthOfPrevious", cp.intexpr, (cp.intervalsequencevar, cp.intervalvar, jint), sequence, var, lastval)
end

function cpo_java_lengthofprevious_int(cp::JavaCPOModel, sequence, var, firstval, absval)
    return jcall(cp.cp, "lengthOfPrevious", cp.intexpr, (cp.intervalsequencevar, cp.intervalvar, jint, jint), sequence, var, lastval, absval)
end

function cpo_java_log(cp::JavaCPOModel, expr)
    return jcall(cp.cp, "log", cp.numexpr, (cp.numexpr,), expr)
end

function cpo_java_max_int(cp::JavaCPOModel, exprs)
    return jcall(cp.cp, "max", cp.intexpr, (cp.intexprarray,), exprs)
end

function cpo_java_max_num(cp::JavaCPOModel, exprs)
    return jcall(cp.cp, "max", cp.numexpr, (cp.numexprarray,), exprs)
end

function cpo_java_min_int(cp::JavaCPOModel, exprs)
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

function cpo_java_numtonumsegmentfunction(cp::JavaCPOModel)
    return jcall(cp.cp, "numToNumSegmentFunction", cp.numtonumsegmentfunction, ())
end

function cpo_java_numtonumsegmentfunction(cp::JavaCPOModel, x::Vector{Real}, v::Vector{Real})
    return jcall(cp.cp, "numToNumSegmentFunction", cp.numtonumsegmentfunction, (Vector{jdouble}, Vector{jdouble}), x, v)
end

function cpo_java_numtonumstepfunction(cp::JavaCPOModel)
    return jcall(cp.cp, "numToNumStepFunction", cp.numtonumstepfunction, ())
end

function cpo_java_overlaplength(cp::JavaCPOModel, var_a, var_b)
    return jcall(cp.cp, "overlapLength", cp.intexpr, (cp.intervalvar, cp.intervalvar), var_a, var_b)
end

function cpo_java_overlaplength_absval(cp::JavaCPOModel, var_a, var_b, absval::Integer)
    return jcall(cp.cp, "overlapLength", cp.intexpr, (cp.intervalvar, cp.intervalvar, jint), var_a, var_b, absval)
end

function cpo_java_overlaplength_int(cp::JavaCPOModel, var, start::Integer, end_::Integer)
    return jcall(cp.cp, "overlapLength", cp.intexpr, (cp.intervalvar, jint, jint), var, start, end_)
end

function cpo_java_overlaplength_int(cp::JavaCPOModel, var, start::Integer, end_::Integer, absval::Integer)
    return jcall(cp.cp, "overlapLength", cp.intexpr, (cp.intervalvar, jint, jint, jint), var, start, end_, absval)
end

## Constraint creation

function cpo_java_alldiff(cp::JavaCPOModel, exprs)
    return jcall(cp.cp, "allDiff", cp.constraint, (cp.intexprarray,), exprs)
end

function cpo_java_allmindistance(cp::JavaCPOModel, vars)
    return jcall(cp.cp, "allMinDistance", cp.constraint, (cp.intvararray,), vars)
end

function cpo_java_allowedassignments_expr(cp::JavaCPOModel, expr, values::Vector{<:Integer})
    return jcall(cp.cp, "allowedAssignments", cp.constraint, (cp.intvararray, Vector{jint}), vars, values)
end

function cpo_java_allowedassignments_vars(cp::JavaCPOModel, vars, values::Vector{<:Integer})
    # TODO: IloIntTupleSet
    return jcall(cp.cp, "allowedAssignments", cp.constraint, (cp.intvararray, cp.inttupleset), vars, values)
end

function cpo_java_alternative(cp::JavaCPOModel, interval_a, intervals_b::Vector, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "alternative", cp.constraint, (cp.intervalvar, cp.intervalvararray), interval_a, intervals_b)
    else
        return jcall(cp.cp, "alternative", cp.constraint, (cp.intervalvar, cp.intervalvararray, JString), interval_a, intervals_b, name)
    end
end

function cpo_java_alternative_int(cp::JavaCPOModel, interval_a, intervals_b::Vector, value, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "alternative", cp.constraint, (cp.intervalvar, cp.intervalvararray, jint), interval_a, intervals_b, value)
    else
        return jcall(cp.cp, "alternative", cp.constraint, (cp.intervalvar, cp.intervalvararray, jint, JString), interval_a, intervals_b, value, name)
    end
end

function cpo_java_alternative_expr(cp::JavaCPOModel, interval_a, intervals_b::Vector, expr, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "alternative", cp.constraint, (cp.intervalvar, cp.intervalvararray, cp.intexpr), interval_a, intervals_b, expr)
    else
        return jcall(cp.cp, "alternative", cp.constraint, (cp.intervalvar, cp.intervalvararray, cp.intexpr, JString), interval_a, intervals_b, expr, name)
    end
end

# For these constraints, need to map IloStateFunction.
# TODO: alwaysConstant
# TODO: alwaysEqual
# TODO: alwaysIn
# TODO: alwaysNoState

# TODO: before, need IloIntervalSequenceVar

function cpo_java_distribute(cp::JavaCPOModel, exprs_cards, exprs_vars)
    return jcall(cp.cp, "distribute", cp.constraint, (cp.intexprarray, cp.intexprarray), exprs_cards, exprs_vars)
end

function cpo_java_distribute_int(cp::JavaCPOModel, exprs_cards, values::Vector{<:Integer}, exprs_vars)
    return jcall(cp.cp, "distribute", cp.constraint, (cp.intexprarray, Vector{jint}, cp.intexprarray), exprs_cards, values, exprs_vars)
end

function cpo_java_element_int(cp::JavaCPOModel, expr_var, expr_index, values::Vector{<:Integer})
    return jcall(cp.cp, "element", cp.constraint, (cp.intexpr, cp.intexpr, Vector{jint}), expr_var, expr_index, values)
end

function cpo_java_element_num(cp::JavaCPOModel, expr_var, expr_index, values::Vector{<:Real})
    return jcall(cp.cp, "element", cp.constraint, (cp.intexpr, cp.intexpr, Vector{jdouble}), expr_var, expr_index, values)
end

function cpo_java_endatend(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "endAtEnd", cp.constraint, (cp.intervalvar, cp.intervalvar), expr_a, expr_b)
end

function cpo_java_endatend_int(cp::JavaCPOModel, expr_a, expr_b, expr_z)
    return jcall(cp.cp, "endAtEnd", cp.constraint, (cp.intervalvar, cp.intervalvar, jint), expr_a, expr_b, expr_z)
end

function cpo_java_endatend_intexpr(cp::JavaCPOModel, expr_a, expr_b, expr_z)
    return jcall(cp.cp, "endAtEnd", cp.constraint, (cp.intervalvar, cp.intervalvar, cp.intexpr), expr_a, expr_b, expr_z)
end

function cpo_java_endatstart(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "endAtStart", cp.constraint, (cp.intervalvar, cp.intervalvar), expr_a, expr_b)
end

function cpo_java_endatstart_int(cp::JavaCPOModel, expr_a, expr_b, expr_z)
    return jcall(cp.cp, "endAtStart", cp.constraint, (cp.intervalvar, cp.intervalvar, jint), expr_a, expr_b, expr_z)
end

function cpo_java_endatstart_intexpr(cp::JavaCPOModel, expr_a, expr_b, expr_z)
    return jcall(cp.cp, "endAtStart", cp.constraint, (cp.intervalvar, cp.intervalvar, cp.intexpr), expr_a, expr_b, expr_z)
end

function cpo_java_endbeforeend(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "endBeforeEnd", cp.constraint, (cp.intervalvar, cp.intervalvar), expr_a, expr_b)
end

function cpo_java_endbeforeend_int(cp::JavaCPOModel, expr_a, expr_b, expr_z)
    return jcall(cp.cp, "endBeforeEnd", cp.constraint, (cp.intervalvar, cp.intervalvar, jint), expr_a, expr_b, expr_z)
end

function cpo_java_endbeforeend_intexpr(cp::JavaCPOModel, expr_a, expr_b, expr_z)
    return jcall(cp.cp, "endBeforeEnd", cp.constraint, (cp.intervalvar, cp.intervalvar, cp.intexpr), expr_a, expr_b, expr_z)
end

function cpo_java_endbeforestart(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "endBeforeStart", cp.constraint, (cp.intervalvar, cp.intervalvar), expr_a, expr_b)
end

function cpo_java_endbeforestart_int(cp::JavaCPOModel, expr_a, expr_b, expr_z)
    return jcall(cp.cp, "endBeforeStart", cp.constraint, (cp.intervalvar, cp.intervalvar, jint), expr_a, expr_b, expr_z)
end

function cpo_java_endbeforestart_intexpr(cp::JavaCPOModel, expr_a, expr_b, expr_z)
    return jcall(cp.cp, "endBeforeStart", cp.constraint, (cp.intervalvar, cp.intervalvar, cp.intexpr), expr_a, expr_b, expr_z)
end

function cpo_java_eq_intexpr_intexpr(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "eq", cp.constraint, (cp.intexpr, cp.intexpr), expr_a, expr_b)
end

function cpo_java_eq_int_intexpr(cp::JavaCPOModel, expr_a::Integer, expr_b)
    return jcall(cp.cp, "eq", cp.constraint, (jint, cp.intexpr), expr_a, expr_b)
end

function cpo_java_eq_intexpr_int(cp::JavaCPOModel, expr_a, expr_b::Integer)
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

function cpo_java_forbiddenassignments_expr(cp::JavaCPOModel, expr, values::Vector{<:Integer})
    return jcall(cp.cp, "forbiddenAssignments", cp.constraint, (cp.intvararray, Vector{jint}), vars, values)
end

function cpo_java_forbiddenassignments_vars(cp::JavaCPOModel, vars, values::Vector{<:Integer})
    # TODO: IloIntTupleSet
    return jcall(cp.cp, "forbiddenAssignments", cp.constraint, (cp.intvararray, cp.inttupleset), vars, values)
end

# TODO: Requires IloNumToNumStepFunction:
# TODO: forbidEnd
# TODO: forbidExtent
# TODO: forbidStart
# TODO: ge (4 times)

function cpo_java_ge_intexpr_intexpr(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "ge", cp.constraint, (cp.intexpr, cp.intexpr), expr_a, expr_b)
end

function cpo_java_ge_int_intexpr(cp::JavaCPOModel, expr_a::Integer, expr_b)
    return jcall(cp.cp, "ge", cp.constraint, (jint, cp.intexpr), expr_a, expr_b)
end

function cpo_java_ge_intexpr_int(cp::JavaCPOModel, expr_a, expr_b::Integer)
    return jcall(cp.cp, "ge", cp.constraint, (cp.intexpr, jint), expr_a, expr_b)
end

function cpo_java_gt_intexpr_intexpr(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "gt", cp.constraint, (cp.intexpr, cp.intexpr), expr_a, expr_b)
end

function cpo_java_gt_int_intexpr(cp::JavaCPOModel, expr_a::Integer, expr_b)
    return jcall(cp.cp, "gt", cp.constraint, (jint, cp.intexpr), expr_a, expr_b)
end

function cpo_java_gt_intexpr_int(cp::JavaCPOModel, expr_a, expr_b::Integer)
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

function cpo_java_isomorphism_map(cp::JavaCPOModel, vars_a, vars_b, map, absval, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "isomorphism", cp.isomorphism, (cp.intervalvararray, cp.intervalvararray, cp.intaxprarray, jint), vars_a, vars_b, map, absval)
    else
        return jcall(cp.cp, "isomorphism", cp.isomorphism, (cp.intervalvararray, cp.intervalvararray, cp.intaxprarray, jint, JString), vars_a, vars_b, map, absval, name)
    end
end

function cpo_java_last(cp::JavaCPOModel, intervalseq, interval)
    return jcall(cp.cp, "last", cp.constraint, (cp.intervalsequencevar, cp.intervalvar), intervalseq, interval)
end

function cpo_java_le_intexpr_intexpr(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "le", cp.constraint, (cp.intexpr, cp.intexpr), expr_a, expr_b)
end

function cpo_java_le_int_intexpr(cp::JavaCPOModel, expr_a::Integer, expr_b)
    return jcall(cp.cp, "le", cp.constraint, (jint, cp.intexpr), expr_a, expr_b)
end

function cpo_java_le_intexpr_int(cp::JavaCPOModel, expr_a, expr_b::Integer)
    return jcall(cp.cp, "le", cp.constraint, (cp.intexpr, jint), expr_a, expr_b)
end

function cpo_java_lexicographic(cp::JavaCPOModel, exprs_a, exprs_b)
    return jcall(cp.cp, "lexicographic", cp.constraint, (cp.intexprarray, cp.intexprarray), exprs_a, exprs_b)
end

function cpo_java_lt_intexpr_intexpr(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "lt", cp.constraint, (cp.intexpr, cp.intexpr), expr_a, expr_b)
end

function cpo_java_lt_int_intexpr(cp::JavaCPOModel, expr_a::Integer, expr_b)
    return jcall(cp.cp, "lt", cp.constraint, (jint, cp.intexpr), expr_a, expr_b)
end

function cpo_java_lt_intexpr_int(cp::JavaCPOModel, expr_a, expr_b::Integer)
    return jcall(cp.cp, "lt", cp.constraint, (cp.intexpr, jint), expr_a, expr_b)
end

function cpo_java_neq(cp::JavaCPOModel, constr_a, constr_b)
    return jcall(cp.cp, "neq", cp.constraint, (cp.constraint, cp.constraint), constr_a, constr_b)
end

function cpo_java_neq_int_intexpr(cp::JavaCPOModel, expr_a::Integer, expr_b)
    return jcall(cp.cp, "neq", cp.constraint, (jint, cp.intexpr), expr_a, expr_b)
end

function cpo_java_neq_intexpr_int(cp::JavaCPOModel, expr_a, expr_b::Integer)
    return jcall(cp.cp, "neq", cp.constraint, (cp.intexpr, jint), expr_a, expr_b)
end

function cpo_java_nooverlap(cp::JavaCPOModel, seq)
    return jcall(cp.cp, "noOverlap", cp.nooverlap, (cp.intervalsequencevar,), seq)
end

function cpo_java_nooverlap(cp::JavaCPOModel, vars, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "noOverlap", cp.nooverlap, (cp.intervalvararray,), vars)
    else
        return jcall(cp.cp, "noOverlap", cp.nooverlap, (cp.intervalvararray, JString), vars, name)
    end
end

function cpo_java_pack(cp::JavaCPOModel, expr_load, expr_where, weight::Vector{<:Integer})
    return jcall(cp.cp, "pack", cp.constraint, (cp.intexprarray, cp.intexprarray, Vector{jint}), expr_load, expr_where, weight)
end

function cpo_java_pack(cp::JavaCPOModel, expr_load, expr_where, weight::Vector{<:Integer}, used)
    return jcall(cp.cp, "pack", cp.constraint, (cp.intexprarray, cp.intexprarray, Vector{jint}, cp.intexpr), expr_load, expr_where, weight, used)
end

# TODO: IloTransitionDistance and missing noOverlap

## Objective

function cpo_java_maximize(cp::JavaCPOModel, expr)
    return jcall(cp.cp, "maximize", cp.objective, (cp.intexpr,), expr)
end

function cpo_java_maximize_multicriterion(cp::JavaCPOModel, expr)
    return jcall(cp.cp, "maximize", cp.objective, (cp.multicriterionexpr,), expr)
end

function cpo_java_minimize(cp::JavaCPOModel, expr)
    return jcall(cp.cp, "minimize", cp.objective, (cp.intexpr,), expr)
end

function cpo_java_minimize_multicriterion(cp::JavaCPOModel, expr)
    return jcall(cp.cp, "minimize", cp.objective, (cp.multicriterionexpr,), expr)
end

## Query solution and state
# TODO: getAll*

function cpo_java_getallkpinames(cp::JavaCPOModel)
    return jcall(cp.cp, "getAllKPINames", Vector{JString})
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

#  TODO: getSegmentEnd, getSegmentStart, getSegmentValue

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

function cpo_java_getvalue_intexpr(cp::JavaCPOModel, expr)
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

function cpo_java_isfixed_intervalsequencevar(cp::JavaCPOModel, var)
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

## Miscellaneous
# TODO: dumpModel
# TODO: exportModel

function cpo_java_getbuildid(cp::JavaCPOModel)
    return jcall(cp.cp, "getBuildID", JString)
end

# TODO: getConflict
# TODO: getIloCumulFunctionExpr
# TODO: importModel
