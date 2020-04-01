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
    numvar
    numvararray

    intexpr
    intexprarray
    numexpr
    numexprarray
    inttupleset

    constraint
    alternative_constraint
end

function cpo_java_model()
    # Import the required symbols and store them in the model for future use.
    jcp = @jimport ilog.cp.IloCP
    intvar = @jimport ilog.concert.IloIntVar
    intervalvar = @jimport ilog.concert.IloIntervalVar
    numvar = @jimport ilog.concert.IloNumVar

    intexpr = @jimport ilog.concert.IloIntExpr
    numexpr = @jimport ilog.concert.IloNumExpr
    inttupleset = @jimport ilog.concert.IloIntTupleSet

    constraint = @jimport ilog.concert.IloConstraint
    alternative_constraint = @jimport ilog.concert.IloAlternative

    # Actually build the model.
    model = jcp(())

    # Return the right data structure
    return JavaCPOModel(model,
                        intvar, Vector{intvar}, intervalvar, numvar, Vector{numvar},
                        intexpr, Vector{intexpr}, numexpr, Vector{numexpr}, inttupleset,
                        constraint)
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
# TODO: arrays of interval objects
# intervalSequenceVar

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

# TODO: element

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

function cpo_java_forbiddenassignments_expr(cp::JavaCPOModel, expr, values::Vector{<:Integer})
    return jcall(cp.cp, "forbiddenAssignments", cp.constraint, (cp.intvararray, Vector{jint}), vars, values)
end

function cpo_java_forbiddenassignments_vars(cp::JavaCPOModel, vars, values::Vector{<:Integer})
    # TODO: IloIntTupleSet
    return jcall(cp.cp, "forbiddenAssignments", cp.constraint, (cp.intvararray, cp.inttupleset), vars, values)
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

# TODO: element

## Miscellaneous
# TODO: dumpModel
