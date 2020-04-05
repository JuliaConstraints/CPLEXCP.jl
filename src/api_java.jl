## Type definitions
# These correspond to Java types (which are directly returned by the functions
# in this binder). In particular, the inheritance defined in Java is not brought
# back to Julia. For instance, if a function accepts an IloIntExpr, one can also
# pass an IloIntVar as argument.
const IloCP = JavaObject{Symbol("ilog.concert.IloCP")}

const IloIntVar = JavaObject{Symbol("ilog.concert.IloIntVar")}
const IloIntervalVar = JavaObject{Symbol("ilog.concert.IloIntervalVar")}
const IloIntervalSequenceVar = JavaObject{Symbol("ilog.concert.IloIntervalSequenceVar")}
const IloNumVar = JavaObject{Symbol("ilog.concert.IloNumVar")}

const IloIntExpr = JavaObject{Symbol("ilog.concert.IloIntExpr")}
const IloNumExpr = JavaObject{Symbol("ilog.concert.IloNumExpr")}
const IloIntTupleSet = JavaObject{Symbol("ilog.concert.IloIntTupleSet")}
const IloNumToNumSegmentFunction = JavaObject{Symbol("ilog.concert.IloNumToNumSegmentFunction")}
const IloNumToNumStepFunction = JavaObject{Symbol("ilog.concert.IloNumToNumStepFunction")}
const IloCumulFunctionExpr = JavaObject{Symbol("ilog.concert.IloCumulFunctionExpr")}
const IloTransitionDistance = JavaObject{Symbol("ilog.concert.IloTransitionDistance")}
const IloStateFunction = JavaObject{Symbol("ilog.concert.IloStateFunction")}

const IloConstraint = JavaObject{Symbol("ilog.concert.IloConstraint")}
const IloAlternative = JavaObject{Symbol("ilog.concert.IloAlternative")}
const IloIsomorphism = JavaObject{Symbol("ilog.concert.IloIsomorphism")}
const IloNoOverlap = JavaObject{Symbol("ilog.concert.IloNoOverlap")}
const IloRange = JavaObject{Symbol("ilog.concert.IloRange")}
const IloSpan = JavaObject{Symbol("ilog.concert.IloSpan")}
const IloSynchronize = JavaObject{Symbol("ilog.concert.IloSynchronize")}

const IloObjective = JavaObject{Symbol("ilog.concert.IloObjective")}
const IloMultiCriterionExpr = JavaObject{Symbol("ilog.concert.IloMultiCriterionExpr")}
const IloSolution = JavaObject{Symbol("ilog.concert.IloSolution")}
const IloAddable = JavaObject{Symbol("ilog.concert.IloAddable")}

const Callback = JavaObject{Symbol("ilog.cp.IloCP\$Callback")}
const ConflictStatus = JavaObject{Symbol("ilog.concert.IloCP\$ConflictStatus")}

## TODO: missing methods from IloModeler?

"""
    cpo_java_init()

Initialises the JVM to be able to use CPLEX CP Optimizer.
"""
function cpo_java_init()
    JavaCall.addClassPath(libcplexcpojava)
    JavaCall.init()
end

struct JavaCPOModel
    cp

    conflictstatus_possible
    conflictstatus_member
    conflictstatus_excluded
end

function cpo_java_model()
    # Getting access to the values is not possible before the JVM is started, i.e. before cpo_java_init() is called.
    # It is thus not possible to use const for these values, unfortunately. TODO: Or just hard-code the values?
    conflictstatus = @jimport ilog.cp.IloCP$ConflictStatus
    conflictstatus_possible = jfield(conflictstatus, "ConflictPossibleMember", conflictstatus)
    conflictstatus_member = jfield(conflictstatus, "ConflictMember", conflictstatus)
    conflictstatus_excluded = jfield(conflictstatus, "ConflictExcluded", conflictstatus)

    # Actually build the model.
    model = jcp(())

    # Return the right data structure
    return JavaCPOModel(model, conflictstatus_possible, conflictstatus_member, conflictstatus_excluded)
end

function cpo_java_release(cp::JavaCPOModel)
    jcall(cp.cp, "end", Nothing, ())
end

## Variable creation

# Integer variables
function cpo_java_intvar(cp::JavaCPOModel, lb::T, ub::T, name::String="") where {T <: Integer}
    return jcall(cp.cp, "intVar", IloIntVar, (jint, jint, JString), lb, ub, name)
end

function cpo_java_intvar(cp::JavaCPOModel, values::Vector{T}, name::String="") where {T <: Integer}
    if length(name) == 0
        return jcall(cp.cp, "intVar", IloIntVar, (Vector{jint},), values)
    else
        return jcall(cp.cp, "intVar", IloIntVar, (Vector{jint}, JString), values, name)
    end
end

function cpo_java_intvararray(cp::JavaCPOModel, n::T, lb::T, ub::T, name::String="") where {T <: Integer}
    return jcall(cp.cp, "intVarArray", Vector{IloIntVar}, (jint, jint, jint, JString), n, lb, ub, name)
end

function cpo_java_intvararray(cp::JavaCPOModel, n::T, values::Vector{T}, name::String="") where {T <: Integer}
    return jcall(cp.cp, "intVarArray", Vector{IloIntVar}, (jint, Vector{jint}, JString), n, values, name)
end

# Numerical variables
function cpo_java_numvar(cp::JavaCPOModel, lb::T, ub::T, name::String="") where {T <: Real}
    return jcall(cp.cp, "numVarArray", Vector{IloNumVar}, (jint, jdouble, jdouble, JString), 1, lb, ub, name)[1]
end

function cpo_java_numvararray(cp::JavaCPOModel, n::Int, lb::T, ub::T, name::String="") where {T <: Real}
    return jcall(cp.cp, "numVarArray", Vector{IloNumVar}, (jint, jdouble, jdouble, JString), n, lb, ub, name)
end

# Interval variables
function cpo_java_intervalvar(cp::JavaCPOModel, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "intervalVar", IloIntervalVar, ())
    else
        return jcall(cp.cp, "intervalVar", IloIntervalVar, (JString,), name)
    end
end

function cpo_java_intervalvar(cp::JavaCPOModel, size::Integer, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "intervalVar", IloIntervalVar, (jint,), size)
    else
        return jcall(cp.cp, "intervalVar", IloIntervalVar, (jint, JString), size, name)
    end
end

function cpo_java_intervalvar(cp::JavaCPOModel, size_lb::Integer, size_ub::Integer)
    return jcall(cp.cp, "intervalVar", IloIntervalVar, (jint, jint), size_lb, size_ub)
end

function cpo_java_intervalvar(cp::JavaCPOModel, size_lb::Integer, size_ub::Integer, opt::Bool, intensity, granularity::Integer)
    return jcall(cp.cp, "intervalVar", IloIntervalVar, (jint, jint, jboolean, IloNumToNumStepFunction, jint), size_lb, size_ub, opt, intensity, granularity)
end

# Sequence-of-intervals variables
function cpo_java_intervalsequencevar(cp::JavaCPOModel, intervalvararray::Vector{IloIntervalVar}, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "intervalSequenceVar", IloIntervalSequenceVar, (Vector{IloIntervalVar},), intervalvararray)
    else
        return jcall(cp.cp, "intervalSequenceVar", IloIntervalSequenceVar, (Vector{IloIntervalVar}, JString), intervalvararray, name)
    end
end

function cpo_java_intervalsequencevar(cp::JavaCPOModel, intervalvararray::Vector{IloIntervalVar}, types::Vector{T}, name::String="") where {T <: Integer}
    if length(name) == 0
        return jcall(cp.cp, "intervalSequenceVar", IloIntervalSequenceVar, (Vector{IloIntervalVar}, Vector{jint}), intervalvararray, types)
    else
        return jcall(cp.cp, "intervalSequenceVar", IloIntervalSequenceVar, (Vector{IloIntervalVar}, Vector{jint}, JString), intervalvararray, types, name)
    end
end

## Expression creation

function cpo_java_abs(cp::JavaCPOModel, var)
    return jcall(cp.cp, "abs", IloIntExpr, (IloIntExpr,), var)
end

function cpo_java_constant(cp::JavaCPOModel, value::Integer)
    return jcall(cp.cp, "constant", IloIntExpr, (jint,), value)
end

function cpo_java_constant(cp::JavaCPOModel, value::Real)
    return jcall(cp.cp, "constant", IloNumExpr, (jdouble,), value)
end

function cpo_java_count(cp::JavaCPOModel, exprs::Vector, value::Integer)
    return jcall(cp.cp, "count", IloIntExpr, (Vector{IloIntExpr}, jint), exprs, value)
end

function cpo_java_countdifferent(cp::JavaCPOModel, exprs::Vector)
    return jcall(cp.cp, "countDifferent", IloIntExpr, (Vector{IloIntExpr},), exprs,)
end

function cpo_java_div(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "div", IloIntExpr, (IloIntExpr, IloIntExpr), expr_a, expr_b)
end

function cpo_java_div(cp::JavaCPOModel, int_a::Integer, expr_b)
    return jcall(cp.cp, "div", IloIntExpr, (jint, IloIntExpr), int_a, expr_b)
end

function cpo_java_div(cp::JavaCPOModel, expr_a, int_b::Integer)
    return jcall(cp.cp, "div", IloIntExpr, (IloIntExpr, jint), expr_a, int_b)
end

function cpo_java_element(cp::JavaCPOModel, values::Vector{T}, expr_index) where {T <: Integer}
    return jcall(cp.cp, "element", IloIntExpr, (Vector{jint}, IloIntExpr), values, expr_index)
end

function cpo_java_element(cp::JavaCPOModel, exprs::Vector, expr_index)
    return jcall(cp.cp, "element", IloIntExpr, (Vector{IloIntExpr}, IloIntExpr), exprs, expr_index)
end

function cpo_java_element(cp::JavaCPOModel, values::Vector{T}, expr_index) where {T <: Real}
    return jcall(cp.cp, "element", IloIntExpr, (Vector{jdouble}, IloIntExpr), values, expr_index)
end

function cpo_java_element(cp::JavaCPOModel, a::IloIntervalVar, f::IloNumToNumSegmentFunction)
    return jcall(cp.cp, "endEval", IloNumExpr, (IloIntervalVar, IloNumToNumSegmentFunction), a, f)
end

function cpo_java_element(cp::JavaCPOModel, a::IloIntervalVar, f::IloNumToNumSegmentFunction, absval::Real)
    return jcall(cp.cp, "endEval", IloNumExpr, (IloIntervalVar, IloNumToNumSegmentFunction, jdouble), a, f, absval)
end

function cpo_java_endof(cp::JavaCPOModel, var::IloIntervalVar)
    return jcall(cp.cp, "endOf", IloIntExpr, (IloIntervalVar,), var)
end

function cpo_java_endof(cp::JavaCPOModel, var::IloIntervalVar, absval::Integer)
    return jcall(cp.cp, "endOf", IloIntExpr, (IloIntervalVar, jint), var, absval)
end

function cpo_java_enfofnext(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, lastval::Integer)
    return jcall(cp.cp, "endOfNext", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint), var_seq, var_interval, lastval)
end

function cpo_java_enfofnext(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, lastval::Integer, absval::Integer)
    return jcall(cp.cp, "endOfNext", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint, jint), var_seq, var_interval, lastval, absval)
end

function cpo_java_enfofprevious(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, firstval::Integer)
    return jcall(cp.cp, "endOfPrevious", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint), var_seq, var_interval, firstval)
end

function cpo_java_enfofprevious(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, firstval::Integer, absval::Integer)
    return jcall(cp.cp, "endOfPrevious", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint, jint), var_seq, var_interval, firstval, absval)
end

function cpo_java_exponent(cp::JavaCPOModel, expr)
    return jcall(cp.cp, "exponent", IloNumExpr, (IloNumExpr,), expr)
end

function cpo_java_heightatend(cp::JavaCPOModel, a::IloIntervalVar, f::IloCumulFunctionExpr)
    return jcall(cp.cp, "heightAtEnd", IloIntExpr, (IloIntervalVar, IloCumulFunctionExpr), a, f)
end

function cpo_java_heightatend(cp::JavaCPOModel, a::IloIntervalVar, f::IloCumulFunctionExpr, absval::Integer)
    return jcall(cp.cp, "heightAtEnd", IloIntExpr, (IloIntervalVar, IloCumulFunctionExpr, jint), a, f, absval)
end

function cpo_java_heightatstart(cp::JavaCPOModel, a::IloIntervalVar, f::IloCumulFunctionExpr)
    return jcall(cp.cp, "heightAtStart", IloIntExpr, (IloIntervalVar, IloCumulFunctionExpr), a, f)
end

function cpo_java_heightatstart(cp::JavaCPOModel, a::IloIntervalVar, f::IloCumulFunctionExpr, absval::Integer)
    return jcall(cp.cp, "heightAtStart", IloIntExpr, (IloIntervalVar, IloCumulFunctionExpr, jint), a, f, absval)
end

function cpo_java_intexpr(cp::JavaCPOModel, constr)
    return jcall(cp.cp, "intExpr", IloIntExpr, (IloConstraint,), constr)
end

function cpo_java_intexprarray(cp::JavaCPOModel, n::Integer)
    return jcall(cp.cp, "intExprArray", Vector{IloIntExpr}, (jint,), n)
end

function cpo_java_lengthof(cp::JavaCPOModel, var::IloIntervalVar)
    return jcall(cp.cp, "lengthOf", IloIntExpr, (IloIntervalVar,), var)
end

function cpo_java_lengthof(cp::JavaCPOModel, var::IloIntervalVar, absval::Integer)
    return jcall(cp.cp, "lengthOf", IloIntExpr, (IloIntervalVar, jint), var, absval)
end

function cpo_java_lengthofnext(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, lastval::Integer)
    return jcall(cp.cp, "lengthOfNext", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint), var_seq, var_interval, lastval)
end

function cpo_java_lengthofnext(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, lastval::Integer, absval::Integer)
    return jcall(cp.cp, "lengthOfNext", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint, jint), var_seq, var_interval, lastval, absval)
end

function cpo_java_lengthofprevious(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, firstval::Integer)
    return jcall(cp.cp, "lengthOfPrevious", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint), var_seq, var_interval, lastval)
end

function cpo_java_lengthofprevious(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, firstval::Integer, absval::Integer)
    return jcall(cp.cp, "lengthOfPrevious", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint, jint), var_seq, var_interval, lastval, absval)
end

function cpo_java_log(cp::JavaCPOModel, expr)
    return jcall(cp.cp, "log", IloNumExpr, (IloNumExpr,), expr)
end

function cpo_java_max_int(cp::JavaCPOModel, exprs::Vector) # TODO: cannot use Julia method dispatch due to missing type for expressions (int/num).
    return jcall(cp.cp, "max", IloIntExpr, (Vector{IloIntExpr},), exprs)
end

function cpo_java_max_num(cp::JavaCPOModel, exprs::Vector)
    return jcall(cp.cp, "max", IloNumExpr, (Vector{IloNumExpr},), exprs)
end

function cpo_java_min_int(cp::JavaCPOModel, exprs::Vector) # TODO: cannot use Julia method dispatch due to missing type for expressions (int/num).
    return jcall(cp.cp, "min", IloIntExpr, (Vector{IloIntExpr},), exprs)
end

function cpo_java_min_num(cp::JavaCPOModel, exprs::Vector)
    return jcall(cp.cp, "min", IloNumExpr, (Vector{IloNumExpr},), exprs)
end

function cpo_java_modulo(cp::JavaCPOModel, expr, r::Integer)
    return jcall(cp.cp, "modulo", IloIntExpr, (IloIntExpr, jint), expr, r)
end

function cpo_java_numexprarray(cp::JavaCPOModel, n::Integer)
    return jcall(cp.cp, "numExprArray", IloIntExpr, (jint,), n)
end

function cpo_java_numvararray(cp::JavaCPOModel, n::Integer)
    return jcall(cp.cp, "numVarArray", IloIntExpr, (jint,), n)
end

function cpo_java_overlaplength(cp::JavaCPOModel, var_a::IloIntervalVar, var_b::IloIntervalVar)
    return jcall(cp.cp, "overlapLength", IloIntExpr, (IloIntervalVar, IloIntervalVar), var_a, var_b)
end

function cpo_java_overlaplength(cp::JavaCPOModel, var_a::IloIntervalVar, var_b::IloIntervalVar, absval::Integer)
    return jcall(cp.cp, "overlapLength", IloIntExpr, (IloIntervalVar, IloIntervalVar, jint), var_a, var_b, absval)
end

function cpo_java_overlaplength(cp::JavaCPOModel, var::IloIntervalVar, start::Integer, end_::Integer)
    return jcall(cp.cp, "overlapLength", IloIntExpr, (IloIntervalVar, jint, jint), var, start, end_)
end

function cpo_java_overlaplength(cp::JavaCPOModel, var::IloIntervalVar, start::Integer, end_::Integer, absval::Integer)
    return jcall(cp.cp, "overlapLength", IloIntExpr, (IloIntervalVar, jint, jint, jint), var, start, end_, absval)
end

function cpo_java_piecewiselinear(cp::JavaCPOModel, var::IloIntervalVar, point::Vector{T}, slope::Vector{T}, a::Real, fa::Real) where {T <: Real}
    return jcall(cp.cp, "piecewiseLinear", IloNumExpr, (IloNumExpr, Vector{jdouble}, Vector{jdouble}, jdouble, jdouble), var, point, slope, a, fa)
end

function cpo_java_piecewiselinear(cp::JavaCPOModel, var::IloIntervalVar, firstslope::Real, point::Vector{T}, value::Vector{T}, lastslope::Real) where {T <: Real}
    return jcall(cp.cp, "piecewiseLinear", IloNumExpr, (IloNumExpr, jdouble, Vector{jdouble}, Vector{jdouble}, jdouble), var, firstslope, point, value, lastslope)
end

function cpo_java_power(cp::JavaCPOModel, expr_a::Real, expr_b)
    return jcall(cp.cp, "power", IloNumExpr, (jdouble, IloNumExpr), expr_a, expr_b)
end

function cpo_java_power(cp::JavaCPOModel, expr_a, expr_b::Real)
    return jcall(cp.cp, "power", IloNumExpr, (IloNumExpr, jdouble), expr_a, expr_b)
end

function cpo_java_power(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "power", IloNumExpr, (IloNumExpr, IloNumExpr), expr_a, expr_b)
end

function cpo_java_prod(cp::JavaCPOModel, values::Vector{T}, exprs) where {T <: Integer}
    return jcall(cp.cp, "prod", IloIntExpr, (Vector{jint}, Vector{IloIntExpr}), values, exprs)
end

function cpo_java_prod(cp::JavaCPOModel, exprs::Vector, values::Vector{T}) where {T <: Integer}
    return jcall(cp.cp, "prod", IloIntExpr, (Vector{IloIntExpr}, Vector{jint}), exprs, values)
end

function cpo_java_prod(cp::JavaCPOModel, exprs_a::Vector, exprs_b::Vector)
    return jcall(cp.cp, "prod", IloIntExpr, (Vector{IloIntExpr}, Vector{IloIntExpr}), exprs_a, exprs_b)
end

function cpo_java_pulse(cp::JavaCPOModel, var::IloIntervalVar, v::Integer)
    return jcall(cp.cp, "pulse", IloCumulFunctionExpr, (IloIntervalVar, jint), var, v)
end

function cpo_java_pulse(cp::JavaCPOModel, var::IloIntervalVar, vmin::Integer, vmax::Integer)
    return jcall(cp.cp, "pulse", IloCumulFunctionExpr, (IloIntervalVar, jint, jint), var, vmin, vmax)
end

function cpo_java_pulse(cp::JavaCPOModel, start::Integer, end_::Integer, v::Integer)
    return jcall(cp.cp, "pulse", IloCumulFunctionExpr, (jint, jint, jint), start, end_, v)
end

function cpo_java_quot(cp::JavaCPOModel, expr_a::Real, expr_b)
    return jcall(cp.cp, "quot", IloNumExpr, (jdouble, IloNumExpr), expr_a, expr_b)
end

function cpo_java_quot(cp::JavaCPOModel, expr_a, expr_b::Real)
    return jcall(cp.cp, "quot", IloNumExpr, (IloNumExpr, jdouble), expr_a, expr_b)
end

function cpo_java_quot(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "quot", IloNumExpr, (IloNumExpr, IloNumExpr), expr_a, expr_b)
end

function cpo_java_sizeeval(cp::JavaCPOModel, a::IloIntervalVar, f::IloNumToNumSegmentFunction)
    return jcall(cp.cp, "sizeEval", IloNumExpr, (IloIntervalVar, IloNumToNumSegmentFunction), a, f)
end

function cpo_java_sizeeval(cp::JavaCPOModel, a::IloIntervalVar, f::IloNumToNumSegmentFunction, absval::Real)
    return jcall(cp.cp, "sizeEval", IloNumExpr, (IloIntervalVar, IloNumToNumSegmentFunction, jdouble), a, f, absval)
end

function cpo_java_sizeof(cp::JavaCPOModel, a::IloIntervalVar)
    return jcall(cp.cp, "sizeOf", IloNumExpr, (IloIntervalVar,), a)
end

function cpo_java_sizeof(cp::JavaCPOModel, a::IloIntervalVar, absval::Real)
    return jcall(cp.cp, "sizeOf", IloNumExpr, (IloIntervalVar, jdouble), a, absval)
end

function cpo_java_sizeofnext(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, lastval::Integer)
    return jcall(cp.cp, "sizeOfNext", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint), var_seq, var_interval, lastval)
end

function cpo_java_sizeofnext(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, lastval::Integer, absval::Real)
    return jcall(cp.cp, "sizeOfNext", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint, jdouble), var_seq, var_interval, lastval, absval)
end

function cpo_java_sizeofprevious(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, firstval::Integer)
    return jcall(cp.cp, "sizeOfPrevious", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint), var_seq, var_interval, firstval)
end

function cpo_java_sizeofprevious(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, firstval::Integer, absval::Real)
    return jcall(cp.cp, "sizeOfPrevious", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint, jdouble), var_seq, var_interval, firstval, absval)
end

function cpo_java_standarddeviation(cp::JavaCPOModel, exprs::Vector)
    return jcall(cp.cp, "standardDeviation", IloNumExpr, (Vector{IloIntExpr},), exprs)
end

function cpo_java_standarddeviation(cp::JavaCPOModel, exprs::Vector, mean_lb::Real, mean_ub::Real)
    return jcall(cp.cp, "standardDeviation", IloNumExpr, (Vector{IloIntExpr}, jdouble, jdouble), exprs, mean_lb, mean_ub)
end

function cpo_java_starteval(cp::JavaCPOModel, a::IloIntervalVar, f::IloNumToNumSegmentFunction)
    return jcall(cp.cp, "startEval", IloNumExpr, (IloIntervalVar, IloNumToNumSegmentFunction), a, f)
end

function cpo_java_starteval(cp::JavaCPOModel, a::IloIntervalVar, f::IloNumToNumSegmentFunction, absval::Real)
    return jcall(cp.cp, "startEval", IloNumExpr, (IloIntervalVar, IloNumToNumSegmentFunction, jdouble), a, f, absval)
end

function cpo_java_startof(cp::JavaCPOModel, a::IloIntervalVar)
    return jcall(cp.cp, "startOf", IloNumExpr, (IloIntervalVar,), a)
end

function cpo_java_startof(cp::JavaCPOModel, a::IloIntervalVar, absval::Real)
    return jcall(cp.cp, "startOf", IloNumExpr, (IloIntervalVar, jdouble), a, absval)
end

function cpo_java_startofnext(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, lastval)
    return jcall(cp.cp, "startOfNext", IloNumExpr, (IloIntervalSequenceVar, IloIntervalVar, jint), var_seq, var_interval, lastval)
end

function cpo_java_startofnext(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, lastval, absval::Real)
    return jcall(cp.cp, "startOfNext", IloNumExpr, (IloIntervalSequenceVar, IloIntervalVar, jint, jdouble), var_seq, var_interval, lastval, absval)
end

function cpo_java_startofprevious(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, firstval)
    return jcall(cp.cp, "startOfPrevious", IloNumExpr, (IloIntervalSequenceVar, IloIntervalVar, jint), var_seq, var_interval, firstval)
end

function cpo_java_startofprevious(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, firstval, absval::Real)
    return jcall(cp.cp, "startOfPrevious", IloNumExpr, (IloIntervalSequenceVar, IloIntervalVar, jint, jdouble), var_seq, var_interval, firstval, absval)
end

function cpo_java_sum_int(cp::JavaCPOModel, exprs) # TODO: cannot use Julia method dispatch due to missing type for expressions (int/num).
    return jcall(cp.cp, "sum", IloIntExpr, (Vector{IloIntExpr},), exprs)
end

function cpo_java_sum_num(cp::JavaCPOModel, exprs)
    return jcall(cp.cp, "sum", IloNumExpr, (Vector{IloNumExpr},), exprs)
end

function cpo_java_typeofnext(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, lastval::Integer)
    return jcall(cp.cp, "typeOfNext", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint), var_seq, var_interval, lastval)
end

function cpo_java_typeofnext(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, lastval::Integer, absval::Integer)
    return jcall(cp.cp, "typeOfNext", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint, jint), var_seq, var_interval, lastval, absval)
end

function cpo_java_typeofprevious(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, firstval::Integer)
    return jcall(cp.cp, "typeOfPrevious", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint), var_seq, var_interval, firstval)
end

function cpo_java_typeofprevious(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, firstval::Integer, absval::Integer)
    return jcall(cp.cp, "typeOfPrevious", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint, jint), var_seq, var_interval, firstval, absval)
end

## IloIntTupleSet: functions

function cpo_java_inttable(cp::JavaCPOModel, dimension::Integer)
    return jcall(cp.cp, "intTable", Vector{IloIntTupleSet}, (jint,), dimension)
end

function cpo_java_inttupleset_addtuple(cp::JavaCPOModel, its::IloIntTupleSet, tuple::Vector{T}) where {T <: Integer}
    return jcall(cp.cp, "addTuple", Nothing, (Vector{IloIntTupleSet}, Vector{jint}), its, tuple)
end

function cpo_java_inttupleset_getarity(cp::JavaCPOModel, its::IloIntTupleSet)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(its, "getArity", jint, ())
end

# Other functions in the interface, but they require the conversion between Java's int[] and C++ Concert's IloIntArray.
# This conversion does not seem to be exposed.

## IloNumToNumSegmentFunction: functions

function cpo_java_numtonumsegmentfunction(cp::JavaCPOModel)
    return jcall(cp.cp, "numToNumSegmentFunction", IloNumToNumSegmentFunction, ())
end

function cpo_java_numtonumsegmentfunction(cp::JavaCPOModel, x::Vector{Real}, v::Vector{Real})
    return jcall(cp.cp, "numToNumSegmentFunction", IloNumToNumSegmentFunction, (Vector{jdouble}, Vector{jdouble}), x, v)
end

function cpo_java_piecewiselinearfunction(cp::JavaCPOModel, point::Vector{T}, slope::Vector{T}, a::Real, fa::Real, name::String="") where {T <: Real}
    if length(name) == 0
        return jcall(cp.cp, "piecewiseLinearFunction", IloNumToNumSegmentFunction, (Vector{jdouble}, Vector{jdouble}, jdouble, jdouble), point, slope, a, fa)
    else
        return jcall(cp.cp, "piecewiseLinearFunction", IloNumToNumSegmentFunction, (Vector{jdouble}, Vector{jdouble}, jdouble, jdouble, JString), point, slope, a, fa, name)
    end
end

function cpo_java_numtonumsegmentfunction_add(cp::JavaCPOModel, n2nsf::IloNumToNumSegmentFunction, f::IloNumToNumSegmentFunction)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "add", Nothing, (IloNumToNumSegmentFunction,), f)
end

function cpo_java_numtonumsegmentfunction_addvalue(cp::JavaCPOModel, n2nsf::IloNumToNumSegmentFunction, x1::Real, x2::Real, v::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "addValue", Nothing, (jdouble, jdouble, jdouble), x1, x2, v)
end

function cpo_java_numtonumsegmentfunction_copy(cp::JavaCPOModel, n2nsf::IloNumToNumSegmentFunction)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "copy", IloNumToNumSegmentFunction, ())
end

function cpo_java_numtonumsegmentfunction_dilate(cp::JavaCPOModel, n2nsf::IloNumToNumSegmentFunction, k::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "dilate", Nothing, (jdouble,), k)
end

function cpo_java_numtonumsegmentfunction_getarea(cp::JavaCPOModel, n2nsf::IloNumToNumSegmentFunction, x1::Real, x2::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "getArea", jdouble, (jdouble, jdouble), x1, x2)
end

function cpo_java_numtonumsegmentfunction_getdefinitionintervalmax(cp::JavaCPOModel, n2nsf::IloNumToNumSegmentFunction)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "getDefinitionIntervalMax", jdouble, ())
end

function cpo_java_numtonumsegmentfunction_getdefinitionintervalmin(cp::JavaCPOModel, n2nsf::IloNumToNumSegmentFunction)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "getDefinitionIntervalMin", jdouble, ())
end

function cpo_java_numtonumsegmentfunction_getmax(cp::JavaCPOModel, n2nsf::IloNumToNumSegmentFunction, x1::Real, x2::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "getMax", jdouble, (jdouble, jdouble), x1, x2)
end

function cpo_java_numtonumsegmentfunction_getmin(cp::JavaCPOModel, n2nsf::IloNumToNumSegmentFunction, x1::Real, x2::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "getMin", jdouble, (jdouble, jdouble), x1, x2)
end

function cpo_java_numtonumsegmentfunction_getvalue(cp::JavaCPOModel, n2nsf::IloNumToNumSegmentFunction, x::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "getValue", jdouble, (jdouble,), x)
end

function cpo_java_numtonumsegmentfunction_prod(cp::JavaCPOModel, n2nsf::IloNumToNumSegmentFunction, k::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "prod", Nothing, (jdouble,), k)
end

function cpo_java_numtonumsegmentfunction_setmax(cp::JavaCPOModel, n2nsf::IloNumToNumSegmentFunction, x1::Real, x2::Real, v::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "setMax", Nothing, (jdouble, jdouble, jdouble, jdouble), x1, x2, v)
end

function cpo_java_numtonumsegmentfunction_setmax(cp::JavaCPOModel, n2nsf::IloNumToNumSegmentFunction, x1::Real, v1::Real, x2::Real, v2::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "setMax", Nothing, (jdouble, jdouble, jdouble, jdouble), x1, v1, x2, v2)
end

function cpo_java_numtonumsegmentfunction_setmax(cp::JavaCPOModel, n2nsf::IloNumToNumSegmentFunction, f::IloNumToNumSegmentFunction)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "setMax", Nothing, (IloNumToNumSegmentFunction,), f)
end

function cpo_java_numtonumsegmentfunction_setmin(cp::JavaCPOModel, n2nsf::IloNumToNumSegmentFunction, x1::Real, x2::Real, v::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "setMin", Nothing, (jdouble, jdouble, jdouble, jdouble), x1, x2, v)
end

function cpo_java_numtonumsegmentfunction_setmin(cp::JavaCPOModel, n2nsf::IloNumToNumSegmentFunction, x1::Real, v1::Real, x2::Real, v2::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "setMin", Nothing, (jdouble, jdouble, jdouble, jdouble), x1, v1, x2, v2)
end

function cpo_java_numtonumsegmentfunction_setmin(cp::JavaCPOModel, n2nsf::IloNumToNumSegmentFunction, f::IloNumToNumSegmentFunction)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "setMin", Nothing, (IloNumToNumSegmentFunction,), f)
end

function cpo_java_numtonumsegmentfunction_setperiodic(cp::JavaCPOModel, n2nsf::IloNumToNumSegmentFunction, f::IloNumToNumSegmentFunction, x0::Real, n::Real, dval::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "setPeriodic", Nothing, (IloNumToNumSegmentFunction, jdouble, jdouble, jdouble), f, x0, n, dval)
end

function cpo_java_numtonumsegmentfunction_setperiodicvalue(cp::JavaCPOModel, n2nsf::IloNumToNumSegmentFunction, x1::Real, x2::Real, f::IloNumToNumSegmentFunction, offset)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "setPeriodicValue", Nothing, (jdouble, jdouble, IloNumToNumSegmentFunction, jdouble), x1, x2, f, offset)
end

# setPoints not done, because there is no access to IloNumArray in Java.

function cpo_java_numtonumsegmentfunction_setslope(cp::JavaCPOModel, n2nsf::IloNumToNumSegmentFunction, x1::Real, x2::Real, v::Real, slope::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "setSlope", Nothing, (jdouble, jdouble, jdouble, jdouble), x1, x2, v, slope)
end

function cpo_java_numtonumsegmentfunction_setvalue(cp::JavaCPOModel, n2nsf::IloNumToNumSegmentFunction, x1::Real, x2::Real, v::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "setValue", Nothing, (jdouble, jdouble, jdouble), x1, x2, v)
end

function cpo_java_numtonumsegmentfunction_shift(cp::JavaCPOModel, n2nsf::IloNumToNumSegmentFunction, dx::Real, dval::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "shift", Nothing, (jdouble, jdouble), dx, dval)
end

function cpo_java_numtonumsegmentfunction_sub(cp::JavaCPOModel, n2nsf::IloNumToNumSegmentFunction, f::IloNumToNumSegmentFunction)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "sub", Nothing, (IloNumToNumSegmentFunction,), f)
end

# TODO: what about IloNumToNumSegmentFunctionCursor?

## IloNumToNumStepFunction: functions

function cpo_java_numtonumstepfunction(cp::JavaCPOModel)
    return jcall(cp.cp, "numToNumStepFunction", IloNumToNumStepFunction, ())
end

function cpo_java_numtonumstepfunction_add(cp::JavaCPOModel, n2nsf::IloNumToNumStepFunction, f::IloNumToNumStepFunction)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "add", Nothing, (IloNumToNumStepFunction,), f)
end

function cpo_java_numtonumstepfunction_addvalue(cp::JavaCPOModel, n2nsf::IloNumToNumStepFunction, x1::Real, x2::Real, v::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "addValue", Nothing, (jdouble, jdouble, jdouble), x1, x2, v)
end

function cpo_java_numtonumstepfunction_copy(cp::JavaCPOModel, n2nsf::IloNumToNumStepFunction)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "copy", IloNumToNumStepFunction, ())
end

function cpo_java_numtonumstepfunction_dilate(cp::JavaCPOModel, n2nsf::IloNumToNumStepFunction, k::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "dilate", Nothing, (jdouble,), k)
end

function cpo_java_numtonumstepfunction_getarea(cp::JavaCPOModel, n2nsf::IloNumToNumStepFunction, x1::Real, x2::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "getArea", jdouble, (jdouble, jdouble), x1, x2)
end

function cpo_java_numtonumstepfunction_getdefinitionintervalmax(cp::JavaCPOModel, n2nsf::IloNumToNumStepFunction)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "getDefinitionIntervalMax", jdouble, ())
end

function cpo_java_numtonumstepfunction_getdefinitionintervalmin(cp::JavaCPOModel, n2nsf::IloNumToNumStepFunction)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "getDefinitionIntervalMin", jdouble, ())
end

function cpo_java_numtonumstepfunction_getmax(cp::JavaCPOModel, n2nsf::IloNumToNumStepFunction, x1::Real, x2::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "getMax", jdouble, (jdouble, jdouble), x1, x2)
end

function cpo_java_numtonumstepfunction_getmin(cp::JavaCPOModel, n2nsf::IloNumToNumStepFunction, x1::Real, x2::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "getMin", jdouble, (jdouble, jdouble), x1, x2)
end

function cpo_java_numtonumstepfunction_getvalue(cp::JavaCPOModel, n2nsf::IloNumToNumStepFunction, x::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "getValue", jdouble, (jdouble,), x)
end

function cpo_java_numtonumstepfunction_prod(cp::JavaCPOModel, n2nsf::IloNumToNumStepFunction, k::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "prod", Nothing, (jdouble,), k)
end

function cpo_java_numtonumstepfunction_setmax(cp::JavaCPOModel, n2nsf::IloNumToNumStepFunction, x1::Real, x2::Real, v::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "setMax", Nothing, (jdouble, jdouble, jdouble, jdouble), x1, x2, v)
end

function cpo_java_numtonumstepfunction_setmax(cp::JavaCPOModel, n2nsf::IloNumToNumStepFunction, x1::Real, v1::Real, x2::Real, v2::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "setMax", Nothing, (jdouble, jdouble, jdouble, jdouble), x1, v1, x2, v2)
end

function cpo_java_numtonumstepfunction_setmax(cp::JavaCPOModel, n2nsf::IloNumToNumStepFunction, f)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "setMax", Nothing, (IloNumToNumStepFunction,), f)
end

function cpo_java_numtonumstepfunction_setmin(cp::JavaCPOModel, n2nsf::IloNumToNumStepFunction, x1::Real, x2::Real, v::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "setMin", Nothing, (jdouble, jdouble, jdouble, jdouble), x1, x2, v)
end

function cpo_java_numtonumstepfunction_setmin(cp::JavaCPOModel, n2nsf::IloNumToNumStepFunction, x1::Real, v1::Real, x2::Real, v2::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "setMin", Nothing, (jdouble, jdouble, jdouble, jdouble), x1, v1, x2, v2)
end

function cpo_java_numtonumstepfunction_setmin(cp::JavaCPOModel, n2nsf::IloNumToNumStepFunction, f::IloNumToNumStepFunction)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "setMin", Nothing, (IloNumToNumStepFunction,), f)
end

function cpo_java_numtonumstepfunction_setperiodic(cp::JavaCPOModel, n2nsf::IloNumToNumStepFunction, f::IloNumToNumStepFunction, x0::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "setPeriodic", Nothing, (IloNumToNumStepFunction, jdouble), f, x0)
end

function cpo_java_numtonumstepfunction_setperiodic(cp::JavaCPOModel, n2nsf::IloNumToNumStepFunction, f::IloNumToNumStepFunction, x0::Real, n::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "setPeriodic", Nothing, (IloNumToNumStepFunction, jdouble, jdouble), f, x0, n)
end

function cpo_java_numtonumstepfunction_setperiodicvalue(cp::JavaCPOModel, n2nsf::IloNumToNumStepFunction, x1::Real, x2::Real, f::IloNumToNumStepFunction)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "setPeriodicValue", Nothing, (jdouble, jdouble, IloNumToNumStepFunction), x1, x2, f)
end

function cpo_java_numtonumstepfunction_setperiodicvalue(cp::JavaCPOModel, n2nsf::IloNumToNumStepFunction, x1::Real, x2::Real, f::IloNumToNumStepFunction, offset)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "setPeriodicValue", Nothing, (jdouble, jdouble, IloNumToNumStepFunction, jdouble), x1, x2, f, offset)
end

# setSteps not done, because there is no access to IloNumArray in Java.

function cpo_java_numtonumstepfunction_setvalue(cp::JavaCPOModel, n2nsf::IloNumToNumStepFunction, x1::Real, x2::Real, v::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "setValue", Nothing, (jdouble, jdouble, jdouble), x1, x2, v)
end

function cpo_java_numtonumstepfunction_shift(cp::JavaCPOModel, n2nsf::IloNumToNumStepFunction, dx::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "shift", Nothing, (jdouble, jdouble), dx)
end

function cpo_java_numtonumstepfunction_shift(cp::JavaCPOModel, n2nsf::IloNumToNumStepFunction, dx::Real, dval::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "shift", Nothing, (jdouble, jdouble), dx, dval)
end

function cpo_java_numtonumstepfunction_sub(cp::JavaCPOModel, n2nsf::IloNumToNumStepFunction, f::IloNumToNumStepFunction)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(n2nsf, "sub", Nothing, (IloNumToNumStepFunction,), f)
end

# TODO: what about IloNumToNumStepFunctionCursor?

## IloStateFunction: functions (they are all directly implemented in IloCP)

function cpo_java_statefunction(cp::JavaCPOModel, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "stateFunction", IloStateFunction, ())
    else
        return jcall(cp.cp, "stateFunction", IloStateFunction, (JString,), name)
    end
end

function cpo_java_statefunction(cp::JavaCPOModel, t::IloTransitionDistance, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "stateFunction", IloStateFunction, (IloTransitionDistance,), t)
    else
        return jcall(cp.cp, "stateFunction", IloStateFunction, (IloTransitionDistance, JString), t, name)
    end
end

## IloCumulFunctionExpr: functions (they are all directly implemented in IloCP)

function cpo_java_cumulfunctionexpr(cp::JavaCPOModel, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "cumulFunctionExpr", IloCumulFunctionExpr, ())
    else
        return jcall(cp.cp, "cumulFunctionExpr", IloCumulFunctionExpr, (JString,), name)
    end
end

function cpo_java_diff_int(cp::JavaCPOModel, e1::Integer, e2) # TODO: cannot use Julia method dispatch due to missing type for expressions/variables (int/num).
    return jcall(cp.cp, "diff", IloIntExpr, (jint, IloIntExpr), f1, f2)
end

function cpo_java_diff_int(cp::JavaCPOModel, e1, e2::Integer)
    return jcall(cp.cp, "diff", IloIntExpr, (IloIntExpr, jint), e1, e2)
end

function cpo_java_diff_int(cp::JavaCPOModel, e1, e2)
    return jcall(cp.cp, "diff", IloIntExpr, (IloIntExpr, IloIntExpr), e1, e2)
end

function cpo_java_diff_num(cp::JavaCPOModel, e1::Real, e2)
    return jcall(cp.cp, "diff", IloNumExpr, (jdouble, IloNumExpr), e1, e2)
end

function cpo_java_diff_num(cp::JavaCPOModel, e1, e2::Real)
    return jcall(cp.cp, "diff", IloNumExpr, (jdouble, jdouble), e1, e2)
end

function cpo_java_diff_num(cp::JavaCPOModel, e1, e2)
    return jcall(cp.cp, "diff", IloNumExpr, (IloNumExpr, IloNumExpr), e1, e2)
end

function cpo_java_diff_cumulfunctionexpr(cp::JavaCPOModel, f1::IloCumulFunctionExpr, f2::IloCumulFunctionExpr)
    return jcall(cp.cp, "diff", IloCumulFunctionExpr, (IloCumulFunctionExpr, IloCumulFunctionExpr), f1, f2)
end

function cpo_java_getnumberofsegments(cp::JavaCPOModel, f::IloCumulFunctionExpr)
    return jcall(cp.cp, "getNumberOfSegments", jint, (IloCumulFunctionExpr,), f)
end

function cpo_java_getsegmentstart(cp::JavaCPOModel, f::IloCumulFunctionExpr, i::Integer)
    return jcall(cp.cp, "getSegmentStart", jint, (IloCumulFunctionExpr, jint), f, i)
end

function cpo_java_getsegmentend(cp::JavaCPOModel, f::IloCumulFunctionExpr, i::Integer)
    return jcall(cp.cp, "getSegmentEnd", jint, (IloCumulFunctionExpr, jint), f, i)
end

function cpo_java_getsegmentvalue(cp::JavaCPOModel, f::IloCumulFunctionExpr, i::Integer)
    return jcall(cp.cp, "getSegmentValue", jint, (IloCumulFunctionExpr, jint), f, i)
end

function cpo_java_getvalue_cumulfunctionexpr(cp::JavaCPOModel, f::IloCumulFunctionExpr, i::Integer) # TODO: cannot use Julia method dispatch due to missing type for expressions/variables (int/num).
    return jcall(cp.cp, "getSegmentValue", jint, (IloCumulFunctionExpr, jint), f, i)
end

function cpo_java_step(cp::JavaCPOModel, t::Integer, v::Integer)
    return jcall(cp.cp, "step", IloCumulFunctionExpr, (jint, jint), t, v)
end

function cpo_java_stepatend(cp::JavaCPOModel, a::IloIntervalVar, v::Integer)
    return jcall(cp.cp, "stepAtEnd", IloCumulFunctionExpr, (IloIntervalVar, jint), a, v)
end

function cpo_java_stepatend(cp::JavaCPOModel, a::IloIntervalVar, vmin::Integer, vmax::Integer)
    return jcall(cp.cp, "stepAtEnd", IloCumulFunctionExpr, (IloIntervalVar, jint, jint), a, vmin, vmax)
end

function cpo_java_stepatstart(cp::JavaCPOModel, a::IloIntervalVar, v::Integer)
    return jcall(cp.cp, "stepAtStart", IloCumulFunctionExpr, (IloIntervalVar, jint), a, v)
end

function cpo_java_stepatstart(cp::JavaCPOModel, a::IloIntervalVar, vmin::Integer, vmax::Integer)
    return jcall(cp.cp, "stepAtStart", IloCumulFunctionExpr, (IloIntervalVar, jint, jint), a, vmin, vmax)
end

function cpo_java_sum_cumulfunctionexpr(cp::JavaCPOModel, f1::IloIntervalVar, f2::IloIntervalVar) # TODO: cannot use Julia method dispatch due to missing type for expressions (int/num vs. cumul).
    return jcall(cp.cp, "sum", IloCumulFunctionExpr, (IloCumulFunctionExpr, cumulfunctionexpr), f1, f2)
end

## IloTransitionDistance: functions

function cpo_java_transitiondistance(cp::JavaCPOModel, i::Integer, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "transitionDistance", IloTransitionDistance, (jint,), i)
    else
        return jcall(cp.cp, "transitionDistance", IloTransitionDistance, (jint, JString), i, name)
    end
end

function cpo_java_transitiondistance(cp::JavaCPOModel, dtable::Matrix{T}, name::String="") where {T <: Integer}
    if length(name) == 0
        return jcall(cp.cp, "transitionDistance", IloTransitionDistance, (Vector{Vector{jint}},), dtable)
    else
        return jcall(cp.cp, "transitionDistance", IloTransitionDistance, (Vector{Vector{jint}}, JString), dtable, name)
    end
end

function cpo_java_transitiondistance_getsize(cp::JavaCPOModel, td::IloTransitionDistance)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(td, "getSize", jint, ())
end

function cpo_java_transitiondistance_getvalue(cp::JavaCPOModel, td::IloTransitionDistance, fromstate::Integer, tostate::Integer)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(td, "getValue", jint, (jint, jint), fromstate, tostate)
end

function cpo_java_transitiondistance_setvalue(cp::JavaCPOModel, td::IloTransitionDistance, fromstate::Integer, tostate::Integer, value::Integer)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(td, "setValue", Nothing, (jint, jint, jint), fromstate, tostate, value)
end

## Constraint creation

function cpo_java_add(cp::JavaCPOModel, addable)
    return jcall(cp.cp, "add", IloAddable, (IloAddable,), addable)
end

function cpo_java_alldiff(cp::JavaCPOModel, exprs::Vector)
    return jcall(cp.cp, "allDiff", IloConstraint, (Vector{IloIntExpr},), exprs)
end

function cpo_java_allmindistance(cp::JavaCPOModel, vars::Vector)
    return jcall(cp.cp, "allMinDistance", IloConstraint, (Vector{IloIntVar},), vars)
end

function cpo_java_allowedassignments_expr(cp::JavaCPOModel, expr, values::Vector{T}) where {T <: Integer} # TODO: cannot use Julia method dispatch due to missing type for expressions/variables.
    return jcall(cp.cp, "allowedAssignments", IloConstraint, (Vector{IloIntVar}, Vector{jint}), vars, values)
end

function cpo_java_allowedassignments_vars(cp::JavaCPOModel, vars::Vector, values::Vector)
    return jcall(cp.cp, "allowedAssignments", IloConstraint, (Vector{IloIntVar}, Vector{IloIntTupleSet}), vars, values)
end

function cpo_java_alternative(cp::JavaCPOModel, interval_a::IloIntervalVar, intervals_b::Vector{IloIntervalVar}, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "alternative", IloAlternative, (IloIntervalVar, Vector{IloIntervalVar}), interval_a, intervals_b)
    else
        return jcall(cp.cp, "alternative", IloAlternative, (IloIntervalVar, Vector{IloIntervalVar}, JString), interval_a, intervals_b, name)
    end
end

function cpo_java_alternative(cp::JavaCPOModel, interval_a::IloIntervalVar, intervals_b::Vector{IloIntervalVar}, value::Int, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "alternative", IloAlternative, (IloIntervalVar, Vector{IloIntervalVar}, jint), interval_a, intervals_b, value)
    else
        return jcall(cp.cp, "alternative", IloAlternative, (IloIntervalVar, Vector{IloIntervalVar}, jint, JString), interval_a, intervals_b, value, name)
    end
end

function cpo_java_alternative(cp::JavaCPOModel, interval_a::IloIntervalVar, intervals_b::Vector{IloIntervalVar}, expr, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "alternative", IloAlternative, (IloIntervalVar, Vector{IloIntervalVar}, IloIntExpr), interval_a, intervals_b, expr)
    else
        return jcall(cp.cp, "alternative", IloAlternative, (IloIntervalVar, Vector{IloIntervalVar}, IloIntExpr, JString), interval_a, intervals_b, expr, name)
    end
end

function cpo_java_alwaysconstant(cp::JavaCPOModel, f::IloStateFunction, a::IloIntervalVar)
    return jcall(cp.cp, "alwaysConstant", IloConstraint, (IloStateFunction, IloIntervalVar), f, a)
end

function cpo_java_alwaysconstant(cp::JavaCPOModel, f::IloStateFunction, a::IloIntervalVar, startalign::Bool, endalign::Bool)
    return jcall(cp.cp, "alwaysConstant", IloConstraint, (IloStateFunction, IloIntervalVar, jboolean, jboolean), f, a, startalign, endalign)
end

function cpo_java_alwaysconstant(cp::JavaCPOModel, f::IloStateFunction, start::Integer, end_::Integer, startalign::Bool, endalign::Bool)
    return jcall(cp.cp, "alwaysConstant", IloConstraint, (IloStateFunction, jint, jint), f, start, end_)
end

function cpo_java_alwaysconstant(cp::JavaCPOModel, f::IloStateFunction, start::Integer, end_::Integer)
    return jcall(cp.cp, "alwaysConstant", IloConstraint, (IloStateFunction, jint, jint, jboolean, jboolean), f, start, end_, startalign, endalign)
end

function cpo_java_alwaysequal_cumul(cp::JavaCPOModel, f::IloStateFunction, a::IloIntervalVar, v::Int) # TODO: cannot use Julia method dispatch due to missing type for expressions/functions.
    return jcall(cp.cp, "alwaysEqual", IloConstraint, (IloCumulFunctionExpr, IloIntervalVar, jint), f, a, v)
end

function cpo_java_alwaysequal_cumul(cp::JavaCPOModel, f::IloStateFunction, start::Integer, end_::Integer, v::Int)
    return jcall(cp.cp, "alwaysEqual", IloConstraint, (IloCumulFunctionExpr, jint, jint, jint), f, start, end_, v)
end

function cpo_java_alwaysequal_state(cp::JavaCPOModel, f::IloStateFunction, a::IloIntervalVar, v::Int)
    return jcall(cp.cp, "alwaysEqual", IloConstraint, (IloStateFunction, IloIntervalVar, jint), f, a, v)
end

function cpo_java_alwaysequal_state(cp::JavaCPOModel, f::IloStateFunction, a::IloIntervalVar, v::Int, startalign::Bool, endalign::Bool)
    return jcall(cp.cp, "alwaysEqual", IloConstraint, (IloStateFunction, IloIntervalVar, jint, jboolean, jboolean), f, a, v, startalign, endalign)
end

function cpo_java_alwaysequal_state(cp::JavaCPOModel, f::IloStateFunction, start::Integer, end_::Integer, v::Int, startalign::Bool, endalign::Bool)
    return jcall(cp.cp, "alwaysEqual", IloConstraint, (IloStateFunction, jint, jint, jint), f, start, end_, v)
end

function cpo_java_alwaysequal_state(cp::JavaCPOModel, f::IloStateFunction, start::Integer, end_::Integer, v::Int)
    return jcall(cp.cp, "alwaysEqual", IloConstraint, (IloStateFunction, jint, jint, jint, jboolean, jboolean), f, start, end_, v, startalign, endalign)
end

function cpo_java_alwaysin_cumul(cp::JavaCPOModel, f::IloCumulFunctionExpr, a::IloIntervalVar, vmin::Int, vmax::Int) # TODO: cannot use Julia method dispatch due to missing type for expressions/functions.
    return jcall(cp.cp, "alwaysIn", IloConstraint, (IloCumulFunctionExpr, IloIntervalVar, jint, jint), f, a, vmin, vmax)
end

function cpo_java_alwaysin_cumul(cp::JavaCPOModel, f::IloCumulFunctionExpr, start::Integer, end_::Integer, vmin::Int, vmax::Int)
    return jcall(cp.cp, "alwaysIn", IloConstraint, (IloCumulFunctionExpr, jint, jint, jint, jint), f, start, end_, vmin, vmax)
end

function cpo_java_alwaysin_state(cp::JavaCPOModel, f::IloStateFunction, a::IloIntervalVar, vmin::Int, vmax::Int)
    return jcall(cp.cp, "alwaysIn", IloConstraint, (IloStateFunction, IloIntervalVar, jint, jint), f, a, vmin, vmax)
end

function cpo_java_alwaysin_state(cp::JavaCPOModel, f::IloStateFunction, start::Integer, end_::Integer, vmin::Int, vmax::Int)
    return jcall(cp.cp, "alwaysIn", IloConstraint, (IloStateFunction, jint, jint, jint, jint), f, start, end_, vmin, vmax)
end

function cpo_java_alwaysnostate(cp::JavaCPOModel, f::IloStateFunction, a::IloIntervalVar)
    return jcall(cp.cp, "alwaysNoState", IloConstraint, (IloStateFunction, IloIntervalVar, jint), f, a)
end

function cpo_java_alwaysnostate(cp::JavaCPOModel, f::IloStateFunction, start::Integer, end_::Integer)
    return jcall(cp.cp, "alwaysNoState", IloConstraint, (IloStateFunction, jint, jint), f, start, end_)
end

function cpo_java_before(cp::JavaCPOModel, seq::IloIntervalSequenceVar, pred::IloIntervalVar, succ::IloIntervalVar)
    return jcall(cp.cp, "before", IloConstraint, (IloIntervalSequenceVar, IloIntervalVar, IloIntervalVar), seq, pred, succ)
end

function cpo_java_distribute(cp::JavaCPOModel, exprs_cards::Vector, exprs_vars::Vector)
    return jcall(cp.cp, "distribute", IloConstraint, (Vector{IloIntExpr}, Vector{IloIntExpr}), exprs_cards, exprs_vars)
end

function cpo_java_distribute(cp::JavaCPOModel, exprs_cards::Vector, values::Vector{T}, exprs_vars::Vector) where {T <: Integer}
    return jcall(cp.cp, "distribute", IloConstraint, (Vector{IloIntExpr}, Vector{jint}, Vector{IloIntExpr}), exprs_cards, values, exprs_vars)
end

function cpo_java_element_int(cp::JavaCPOModel, expr_var, expr_index, values::Vector{T}) where {T <: Integer} # TODO: cannot use Julia method dispatch due to missing type for expressions (int/num).
    return jcall(cp.cp, "element", IloConstraint, (IloIntExpr, IloIntExpr, Vector{jint}), expr_var, expr_index, values)
end

function cpo_java_element_num(cp::JavaCPOModel, expr_var, expr_index, values::Vector{T}) where {T <: Real}
    return jcall(cp.cp, "element", IloConstraint, (IloIntExpr, IloIntExpr, Vector{jdouble}), expr_var, expr_index, values)
end

function cpo_java_endatend(cp::JavaCPOModel, expr_a::IloIntervalVar, expr_b::IloIntervalVar)
    return jcall(cp.cp, "endAtEnd", IloConstraint, (IloIntervalVar, IloIntervalVar), expr_a, expr_b)
end

function cpo_java_endatend(cp::JavaCPOModel, expr_a::IloIntervalVar, expr_b::IloIntervalVar, expr_z::Integer)
    return jcall(cp.cp, "endAtEnd", IloConstraint, (IloIntervalVar, IloIntervalVar, jint), expr_a, expr_b, expr_z)
end

function cpo_java_endatend(cp::JavaCPOModel, expr_a::IloIntervalVar, expr_b::IloIntervalVar, expr_z)
    return jcall(cp.cp, "endAtEnd", IloConstraint, (IloIntervalVar, IloIntervalVar, IloIntExpr), expr_a, expr_b, expr_z)
end

function cpo_java_endatstart(cp::JavaCPOModel, expr_a::IloIntervalVar, expr_b::IloIntervalVar)
    return jcall(cp.cp, "endAtStart", IloConstraint, (IloIntervalVar, IloIntervalVar), expr_a, expr_b)
end

function cpo_java_endatstart(cp::JavaCPOModel, expr_a::IloIntervalVar, expr_b::IloIntervalVar, expr_z::Integer)
    return jcall(cp.cp, "endAtStart", IloConstraint, (IloIntervalVar, IloIntervalVar, jint), expr_a, expr_b, expr_z)
end

function cpo_java_endatstart(cp::JavaCPOModel, expr_a::IloIntervalVar, expr_b::IloIntervalVar, expr_z)
    return jcall(cp.cp, "endAtStart", IloConstraint, (IloIntervalVar, IloIntervalVar, IloIntExpr), expr_a, expr_b, expr_z)
end

function cpo_java_endbeforeend(cp::JavaCPOModel, expr_a::IloIntervalVar, expr_b::IloIntervalVar)
    return jcall(cp.cp, "endBeforeEnd", IloConstraint, (IloIntervalVar, IloIntervalVar), expr_a, expr_b)
end

function cpo_java_endbeforeend(cp::JavaCPOModel, expr_a::IloIntervalVar, expr_b::IloIntervalVar, expr_z::Integer)
    return jcall(cp.cp, "endBeforeEnd", IloConstraint, (IloIntervalVar, IloIntervalVar, jint), expr_a, expr_b, expr_z)
end

function cpo_java_endbeforeend(cp::JavaCPOModel, expr_a::IloIntervalVar, expr_b::IloIntervalVar, expr_z)
    return jcall(cp.cp, "endBeforeEnd", IloConstraint, (IloIntervalVar, IloIntervalVar, IloIntExpr), expr_a, expr_b, expr_z)
end

function cpo_java_endbeforestart(cp::JavaCPOModel, expr_a::IloIntervalVar, expr_b::IloIntervalVar)
    return jcall(cp.cp, "endBeforeStart", IloConstraint, (IloIntervalVar, IloIntervalVar), expr_a, expr_b)
end

function cpo_java_endbeforestart(cp::JavaCPOModel, expr_a::IloIntervalVar, expr_b::IloIntervalVar, expr_z::Integer)
    return jcall(cp.cp, "endBeforeStart", IloConstraint, (IloIntervalVar, IloIntervalVar, jint), expr_a, expr_b, expr_z)
end

function cpo_java_endbeforestart(cp::JavaCPOModel, expr_a::IloIntervalVar, expr_b::IloIntervalVar, expr_z)
    return jcall(cp.cp, "endBeforeStart", IloConstraint, (IloIntervalVar, IloIntervalVar, IloIntExpr), expr_a, expr_b, expr_z)
end

function cpo_java_eq(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "eq", IloConstraint, (IloIntExpr, IloIntExpr), expr_a, expr_b)
end

function cpo_java_eq(cp::JavaCPOModel, expr_a::Integer, expr_b)
    return jcall(cp.cp, "eq", IloConstraint, (jint, IloIntExpr), expr_a, expr_b)
end

function cpo_java_eq(cp::JavaCPOModel, expr_a, expr_b::Integer)
    return jcall(cp.cp, "eq", IloConstraint, (IloIntExpr, jint), expr_a, expr_b)
end

function cpo_java_equiv(cp::JavaCPOModel, constr_a, constr_b)
    return jcall(cp.cp, "eq", IloConstraint, (IloConstraint, IloConstraint), constr_a, constr_b)
end

function cpo_java_falseconstraint(cp::JavaCPOModel)
    return jcall(cp.cp, "falseConstraint", IloConstraint, ())
end

function cpo_java_first(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar)
    return jcall(cp.cp, "first", IloConstraint, (IloIntervalSequenceVar, IloIntervalVar), var_seq, var_interval)
end

function cpo_java_forbiddenassignments_expr(cp::JavaCPOModel, exprs::Vector, values::Vector{T}) where {T <: Integer} # TODO: cannot use Julia method dispatch due to missing type for expressions/variables.
    return jcall(cp.cp, "forbiddenAssignments", IloConstraint, (Vector{IloIntVar}, Vector{jint}), exprs, values)
end

function cpo_java_forbiddenassignments_vars(cp::JavaCPOModel, vars::Vector, values::Vector{IloIntTupleSet})
    return jcall(cp.cp, "forbiddenAssignments", IloConstraint, (Vector{IloIntVar}, Vector{IloIntTupleSet}), vars, values)
end

function cpo_java_forbidend(cp::JavaCPOModel, a::IloIntervalVar, f::IloNumToNumStepFunction)
    return jcall(cp.cp, "forbidEnd", IloConstraint, (IloIntervalVar, IloNumToNumStepFunction), a, f)
end

function cpo_java_forbidextent(cp::JavaCPOModel, a::IloIntervalVar, f::IloNumToNumStepFunction)
    return jcall(cp.cp, "forbidExtent", IloConstraint, (IloIntervalVar, IloNumToNumStepFunction), a, f)
end

function cpo_java_forbidstart(cp::JavaCPOModel, a::IloIntervalVar, f::IloNumToNumStepFunction)
    return jcall(cp.cp, "forbidStart", IloConstraint, (IloIntervalVar, IloNumToNumStepFunction), a, f)
end

function cpo_java_ge_cumul_intexpr(cp::JavaCPOModel, f::IloCumulFunctionExpr, vmin) # TODO: cannot use Julia method dispatch due to missing type for variables.
    return jcall(cp.cp, "ge", IloConstraint, (IloCumulFunctionExpr, IloIntExpr), f, vmin)
end

function cpo_java_ge_cumul_int(cp::JavaCPOModel, f::IloCumulFunctionExpr, vmin::Integer)
    return jcall(cp.cp, "ge", IloConstraint, (IloCumulFunctionExpr, jint), f, vmin)
end

function cpo_java_ge_intexpr_cumul(cp::JavaCPOModel, vmin, f::IloCumulFunctionExpr)
    return jcall(cp.cp, "ge", IloConstraint, (IloIntExpr, IloCumulFunctionExpr), vmin, f)
end

function cpo_java_ge_int_cumul(cp::JavaCPOModel, vmin::Integer, f::IloCumulFunctionExpr)
    return jcall(cp.cp, "ge", IloConstraint, (jint, IloCumulFunctionExpr), vmin, f)
end

function cpo_java_ge(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "ge", IloConstraint, (IloIntExpr, IloIntExpr), expr_a, expr_b)
end

function cpo_java_ge(cp::JavaCPOModel, expr_a::Integer, expr_b)
    return jcall(cp.cp, "ge", IloConstraint, (jint, IloIntExpr), expr_a, expr_b)
end

function cpo_java_ge(cp::JavaCPOModel, expr_a, expr_b::Integer)
    return jcall(cp.cp, "ge", IloConstraint, (IloIntExpr, jint), expr_a, expr_b)
end

function cpo_java_gt(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "gt", IloConstraint, (IloIntExpr, IloIntExpr), expr_a, expr_b)
end

function cpo_java_gt(cp::JavaCPOModel, expr_a::Integer, expr_b)
    return jcall(cp.cp, "gt", IloConstraint, (jint, IloIntExpr), expr_a, expr_b)
end

function cpo_java_gt(cp::JavaCPOModel, expr_a, expr_b::Integer)
    return jcall(cp.cp, "gt", IloConstraint, (IloIntExpr, jint), expr_a, expr_b)
end

function cpo_java_ifthenelse(cp::JavaCPOModel, constr_a, constr_b, constr_c)
    return jcall(cp.cp, "ifThenElse", IloConstraint, (IloConstraint, IloConstraint, IloConstraint), constr_a, constr_b, constr_c)
end

function cpo_java_imply(cp::JavaCPOModel, constr_a, constr_b)
    return jcall(cp.cp, "imply", IloConstraint, (IloConstraint, IloConstraint), constr_a, constr_b)
end

function cpo_java_inverse(cp::JavaCPOModel, constrs_a::Vector, constrs_b::Vector)
    return jcall(cp.cp, "inverse", IloConstraint, (Vector{IloConstraint}, Vector{IloConstraint}), constrs_a, constrs_b)
end

function cpo_java_isomorphism(cp::JavaCPOModel, vars_a::Vector{IloIntervalVar}, vars_b::Vector{IloIntervalVar}, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "isomorphism", IloIsomorphism, (Vector{IloIntervalVar}, Vector{IloIntervalVar}), vars_a, vars_b)
    else
        return jcall(cp.cp, "isomorphism", IloIsomorphism, (Vector{IloIntervalVar}, Vector{IloIntervalVar}, JString), vars_a, vars_b, name)
    end
end

function cpo_java_isomorphism(cp::JavaCPOModel, vars_a::Vector{IloIntervalVar}, vars_b::Vector{IloIntervalVar}, map::Vector, absval::Integer, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "isomorphism", IloIsomorphism, (Vector{IloIntervalVar}, Vector{IloIntervalVar}, Vector{IloIntExpr}, jint), vars_a, vars_b, map, absval)
    else
        return jcall(cp.cp, "isomorphism", IloIsomorphism, (Vector{IloIntervalVar}, Vector{IloIntervalVar}, Vector{IloIntExpr}, jint, JString), vars_a, vars_b, map, absval, name)
    end
end

function cpo_java_last(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar)
    return jcall(cp.cp, "last", IloConstraint, (IloIntervalSequenceVar, IloIntervalVar), var_seq, var_interval)
end

function cpo_java_le(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "le", IloConstraint, (IloIntExpr, IloIntExpr), expr_a, expr_b)
end

function cpo_java_le(cp::JavaCPOModel, expr_a::Integer, expr_b)
    return jcall(cp.cp, "le", IloConstraint, (jint, IloIntExpr), expr_a, expr_b)
end

function cpo_java_le(cp::JavaCPOModel, expr_a, expr_b::Integer)
    return jcall(cp.cp, "le", IloConstraint, (IloIntExpr, jint), expr_a, expr_b)
end

function cpo_java_lexicographic(cp::JavaCPOModel, exprs_a::Vector, exprs_b::Vector)
    return jcall(cp.cp, "lexicographic", IloConstraint, (Vector{IloIntExpr}, Vector{IloIntExpr}), exprs_a, exprs_b)
end

function cpo_java_lt(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "lt", IloConstraint, (IloIntExpr, IloIntExpr), expr_a, expr_b)
end

function cpo_java_lt(cp::JavaCPOModel, expr_a::Integer, expr_b)
    return jcall(cp.cp, "lt", IloConstraint, (jint, IloIntExpr), expr_a, expr_b)
end

function cpo_java_lt(cp::JavaCPOModel, expr_a, expr_b::Integer)
    return jcall(cp.cp, "lt", IloConstraint, (IloIntExpr, jint), expr_a, expr_b)
end

function cpo_java_neq_constraint(cp::JavaCPOModel, constr_a, constr_b)
    return jcall(cp.cp, "neq", IloConstraint, (IloConstraint, IloConstraint), constr_a, constr_b)
end

function cpo_java_neq_intexpr(cp::JavaCPOModel, expr_a, expr_b)
    return jcall(cp.cp, "neq", IloConstraint, (IloIntExpr, IloIntExpr), expr_a, expr_b)
end

function cpo_java_neq_intexpr(cp::JavaCPOModel, expr_a::Integer, expr_b)
    return jcall(cp.cp, "neq", IloConstraint, (jint, IloIntExpr), expr_a, expr_b)
end

function cpo_java_neq_intexpr(cp::JavaCPOModel, expr_a, expr_b::Integer)
    return jcall(cp.cp, "neq", IloConstraint, (IloIntExpr, jint), expr_a, expr_b)
end

function cpo_java_nooverlap_seq(cp::JavaCPOModel, seq::IloIntervalSequenceVar) # TODO: cannot use Julia method dispatch due to missing type for variables.
    return jcall(cp.cp, "noOverlap", IloNoOverlap, (IloIntervalSequenceVar,), seq)
end

function cpo_java_nooverlap_seq(cp::JavaCPOModel, seq::IloIntervalSequenceVar, tdist::IloTransitionDistance, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "noOverlap", IloNoOverlap, (IloIntervalSequenceVar, IloTransitionDistance), seq, tdist)
    else
        return jcall(cp.cp, "noOverlap", IloNoOverlap, (IloIntervalSequenceVar, IloTransitionDistance, JString), seq, tdist, name)
    end
end

function cpo_java_nooverlap_seq(cp::JavaCPOModel, seq::IloIntervalSequenceVar, tdist::IloTransitionDistance, direct::Bool, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "noOverlap", IloNoOverlap, (IloIntervalSequenceVar, IloTransitionDistance, jboolean), seq, tdist, direct)
    else
        return jcall(cp.cp, "noOverlap", IloNoOverlap, (IloIntervalSequenceVar, IloTransitionDistance, jboolean, JString), seq, tdist, direct, name)
    end
end

function cpo_java_nooverlap_vars(cp::JavaCPOModel, vars::Vector{IloIntervalVar}, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "noOverlap", IloNoOverlap, (Vector{IloIntervalVar},), vars)
    else
        return jcall(cp.cp, "noOverlap", IloNoOverlap, (Vector{IloIntervalVar}, JString), vars, name)
    end
end

function cpo_java_pack(cp::JavaCPOModel, expr_load::Vector, expr_where::Vector, weight::Vector{T}) where {T <: Integer}
    return jcall(cp.cp, "pack", IloConstraint, (Vector{IloIntExpr}, Vector{IloIntExpr}, Vector{jint}), expr_load, expr_where, weight)
end

function cpo_java_pack(cp::JavaCPOModel, expr_load::Vector, expr_where::Vector, weight::Vector{T}, used) where {T <: Integer}
    return jcall(cp.cp, "pack", IloConstraint, (Vector{IloIntExpr}, Vector{IloIntExpr}, Vector{jint}, IloIntExpr), expr_load, expr_where, weight, used)
end

function cpo_java_presenceof(cp::JavaCPOModel, var::IloIntervalVar)
    return jcall(cp.cp, "presenceOf", IloConstraint, (IloIntervalVar,), var)
end

function cpo_java_previous(cp::JavaCPOModel, seq::IloIntervalSequenceVar, prev::IloIntervalVar, next::IloIntervalVar)
    return jcall(cp.cp, "previous", IloConstraint, (IloIntervalSequenceVar, IloIntervalVar, IloIntervalVar), seq, prev, next)
end

function cpo_java_range(cp::JavaCPOModel, expr, b::Real)
    return jcall(cp.cp, "range", IloConstraint, (IloNumExpr, jdouble), expr, b)
end

function cpo_java_samecommonsubsequence(cp::JavaCPOModel, seq_1::IloIntervalSequenceVar, seq_2::IloIntervalSequenceVar, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "sameCommonSubsequence", IloConstraint, (IloIntervalSequenceVar, IloIntervalSequenceVar), seq_1, seq_2)
    else
        return jcall(cp.cp, "sameCommonSubsequence", IloConstraint, (IloIntervalSequenceVar, IloIntervalSequenceVar, JString), seq_1, seq_2, name)
    end
end

function cpo_java_samecommonsubsequence(cp::JavaCPOModel, seq_1::IloIntervalSequenceVar, seq_2::IloIntervalSequenceVar, a_1::Vector{IloIntervalVar}, a_2::Vector{IloIntervalVar}, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "sameCommonSubsequence", IloConstraint, (IloIntervalSequenceVar, IloIntervalSequenceVar, Vector{IloIntervalVar}, Vector{IloIntervalVar}), seq_1, seq_2, a_1, a_2)
    else
        return jcall(cp.cp, "sameCommonSubsequence", IloConstraint, (IloIntervalSequenceVar, IloIntervalSequenceVar, Vector{IloIntervalVar}, Vector{IloIntervalVar}, JString), seq_1, seq_2, a_1, a_2, name)
    end
end

function cpo_java_samesubsequence(cp::JavaCPOModel, seq_1::IloIntervalSequenceVar, seq_2::IloIntervalSequenceVar, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "sameSubsequence", IloConstraint, (IloIntervalSequenceVar, IloIntervalSequenceVar), seq_1, seq_2)
    else
        return jcall(cp.cp, "sameSubsequence", IloConstraint, (IloIntervalSequenceVar, IloIntervalSequenceVar, JString), seq_1, seq_2, name)
    end
end

function cpo_java_samesubsequence(cp::JavaCPOModel, seq_1::IloIntervalSequenceVar, seq_2::IloIntervalSequenceVar, a_1::Vector{IloIntervalVar}, a_2::Vector{IloIntervalVar}, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "sameSubsequence", IloConstraint, (IloIntervalSequenceVar, IloIntervalSequenceVar, Vector{IloIntervalVar}, Vector{IloIntervalVar}), seq_1, seq_2, a_1, a_2)
    else
        return jcall(cp.cp, "sameSubsequence", IloConstraint, (IloIntervalSequenceVar, IloIntervalSequenceVar, Vector{IloIntervalVar}, Vector{IloIntervalVar}, JString), seq_1, seq_2, a_1, a_2, name)
    end
end

function cpo_java_sequence(cp::JavaCPOModel, nbmin::T, nbmax::T, seqwidth::T, vars::Vector, values::Vector{T}, card::Vector) where {T <: Integer}
    return jcall(cp.cp, "sequence", IloConstraint, (jint, jint, jint, Vector{IloIntVar}, Vector{jint}, Vector{IloIntVar}), nbmin, nbmax, seqwidth, vars, values, card)
end

function cpo_java_span(cp::JavaCPOModel, a::IloIntervalVar, bs::Vector, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "span", IloSpan, (IloIntervalVar, Vector{IloIntervalVar}), a, bs)
    else
        return jcall(cp.cp, "span", IloSpan, (IloIntervalVar, Vector{IloIntervalVar}, JString), a, bs, name)
    end
end

function cpo_java_startatend(cp::JavaCPOModel, a::IloIntervalVar, b::IloIntervalVar)
    return jcall(cp.cp, "startAtEnd", IloConstraint, (IloIntervalVar, IloIntervalVar), a, b)
end

function cpo_java_startatend(cp::JavaCPOModel, a::IloIntervalVar, b::IloIntervalVar, z::Integer)
    return jcall(cp.cp, "startAtEnd", IloConstraint, (IloIntervalVar, IloIntervalVar, jint), a, b, z)
end

function cpo_java_startatend(cp::JavaCPOModel, a::IloIntervalVar, b::IloIntervalVar, z)
    return jcall(cp.cp, "startAtEnd", IloConstraint, (IloIntervalVar, IloIntervalVar, IloIntExpr), a, b, z)
end

function cpo_java_startatstart(cp::JavaCPOModel, a::IloIntervalVar, b::IloIntervalVar)
    return jcall(cp.cp, "startAtStart", IloConstraint, (IloIntervalVar, IloIntervalVar), a, b)
end

function cpo_java_startatstart(cp::JavaCPOModel, a::IloIntervalVar, b::IloIntervalVar, z::Integer)
    return jcall(cp.cp, "startAtStart", IloConstraint, (IloIntervalVar, IloIntervalVar, jint), a, b, z)
end

function cpo_java_startatstart(cp::JavaCPOModel, a::IloIntervalVar, b::IloIntervalVar, z)
    return jcall(cp.cp, "startAtStart", IloConstraint, (IloIntervalVar, IloIntervalVar, IloIntExpr), a, b, z)
end

function cpo_java_startbeforeend(cp::JavaCPOModel, a::IloIntervalVar, b::IloIntervalVar)
    return jcall(cp.cp, "startBeforeEnd", IloConstraint, (IloIntervalVar, IloIntervalVar), a, b)
end

function cpo_java_startbeforeend(cp::JavaCPOModel, a::IloIntervalVar, b::IloIntervalVar, z::Integer)
    return jcall(cp.cp, "startBeforeEnd", IloConstraint, (IloIntervalVar, IloIntervalVar, jint), a, b, z)
end

function cpo_java_startbeforeend(cp::JavaCPOModel, a::IloIntervalVar, b::IloIntervalVar, z)
    return jcall(cp.cp, "startBeforeEnd", IloConstraint, (IloIntervalVar, IloIntervalVar, IloIntExpr), a, b, z)
end

function cpo_java_startbeforestart(cp::JavaCPOModel, a::IloIntervalVar, b::IloIntervalVar)
    return jcall(cp.cp, "startBeforeStart", IloConstraint, (IloIntervalVar, IloIntervalVar), a, b)
end

function cpo_java_startbeforestart(cp::JavaCPOModel, a::IloIntervalVar, b::IloIntervalVar, z::Integer)
    return jcall(cp.cp, "startBeforeStart", IloConstraint, (IloIntervalVar, IloIntervalVar, jint), a, b, z)
end

function cpo_java_startbeforestart(cp::JavaCPOModel, a::IloIntervalVar, b::IloIntervalVar, z)
    return jcall(cp.cp, "startBeforeStart", IloConstraint, (IloIntervalVar, IloIntervalVar, IloIntExpr), a, b, z)
end

function cpo_java_strong(cp::JavaCPOModel, vars::Vector)
    return jcall(cp.cp, "strong", IloConstraint, (Vector{IloIntVar},), vars)
end

function cpo_java_subcircuit(cp::JavaCPOModel, vars::Vector)
    return jcall(cp.cp, "subCircuit", IloConstraint, (Vector{IloIntVar},), vars)
end

function cpo_java_synchronize(cp::JavaCPOModel, a::IloIntervalVar, bs::Vector{IloIntervalVar}, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "synchronize", IloSynchronize, (IloIntervalVar, Vector{IloIntervalVar}), a, bs)
    else
        return jcall(cp.cp, "synchronize", IloSynchronize, (IloIntervalVar, Vector{IloIntervalVar}, JString), a, bs, name)
    end
end

function cpo_java_trueconstraint(cp::JavaCPOModel)
    return jcall(cp.cp, "trueConstraint", IloConstraint, ())
end

## Objective

function cpo_java_maximize(cp::JavaCPOModel, expr)
    return jcall(cp.cp, "maximize", IloObjective, (IloIntExpr,), expr)
end

function cpo_java_maximize_multicriterion(cp::JavaCPOModel, expr::IloMultiCriterionExpr) # TODO: cannot use Julia method dispatch due to missing type for expressions.
    return jcall(cp.cp, "maximize", IloObjective, (IloMultiCriterionExpr,), expr)
end

function cpo_java_minimize(cp::JavaCPOModel, expr)
    return jcall(cp.cp, "minimize", IloObjective, (IloIntExpr,), expr)
end

function cpo_java_minimize_multicriterion(cp::JavaCPOModel, expr) # TODO: cannot use Julia method dispatch due to missing type for expressions.
    return jcall(cp.cp, "minimize", IloObjective, (IloMultiCriterionExpr,), expr)
end

function cpo_java_staticlex(cp::JavaCPOModel, criteria::Vector, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "staticLex", IloMultiCriterionExpr, (Vector{IloNumExpr},), criteria)
    else
        return jcall(cp.cp, "staticLex", IloMultiCriterionExpr, (Vector{IloNumExpr}, JString), criteria, name)
    end
    # Other staticLex don't need to be mapped, just facility functions in Java for short arrays.
end

## Query solution and state

function cpo_java_getallconstrainedilocumulfunctionexprs(cp::JavaCPOModel)
    return jcall(cp.cp, "getAllConstrainedIloCumulFunctionExprs", IloCumulFunctionExprarray, ())
end

function cpo_java_getallilointervalsequencevars(cp::JavaCPOModel)
    return jcall(cp.cp, "getAllIloIntervalSequenceVars", Vector{IloIntervalSequenceVar}, ())
end

function cpo_java_getallintervalvars(cp::JavaCPOModel)
    return jcall(cp.cp, "getAllIloIntervalVars", Vector{IloIntervalVar}, ())
end

function cpo_java_getallintvars(cp::JavaCPOModel)
    return jcall(cp.cp, "getAllIloIntVars", Vector{IloIntVar}, ())
end

function cpo_java_getallstatefunctions(cp::JavaCPOModel)
    return jcall(cp.cp, "getAllIloStateFunctions", IloStateFunctionarray, ())
end

function cpo_java_getallkpinames(cp::JavaCPOModel)
    return jcall(cp.cp, "getAllKPINames", Vector{JString}, ())
end

function cpo_java_getdomain(cp::JavaCPOModel, var::IloIntervalVar)
    return jcall(cp.cp, "getDomain", JString, (IloIntervalVar,), var)
end

function cpo_java_getdomainsize(cp::JavaCPOModel, var)
    return jcall(cp.cp, "getDomainSize", jint, (IloNumVar,), var)
end

function cpo_java_getend(cp::JavaCPOModel, var::IloIntervalVar)
    return jcall(cp.cp, "getEnd", jint, (IloIntervalVar,), var)
end

function cpo_java_getendmax(cp::JavaCPOModel, var::IloIntervalVar)
    return jcall(cp.cp, "getEndMax", jint, (IloIntervalVar,), var)
end

function cpo_java_getendmin(cp::JavaCPOModel, var::IloIntervalVar)
    return jcall(cp.cp, "getEndMin", jint, (IloIntervalVar,), var)
end

function cpo_java_getfirst(cp::JavaCPOModel, var::IloIntervalSequenceVar)
    return jcall(cp.cp, "getFirst", IloIntervalVar, (IloIntervalSequenceVar,), var)
end

function cpo_java_getincumbentvalue(cp::JavaCPOModel, expr)
    return jcall(cp.cp, "getIncumbentValue", jdouble, (IloNumExpr,), expr)
end

function cpo_java_getintvalue(cp::JavaCPOModel, expr)
    return jcall(cp.cp, "getInt", jint, (IloIntExpr,), expr)
end

function cpo_java_getlast(cp::JavaCPOModel, var::IloIntervalSequenceVar)
    return jcall(cp.cp, "getLast", IloIntervalVar, (IloIntervalSequenceVar,), var)
end

function cpo_java_getlength(cp::JavaCPOModel, var::IloIntervalVar)
    return jcall(cp.cp, "getLength", jint, (IloIntervalVar,), var)
end

function cpo_java_getlengthmax(cp::JavaCPOModel, var::IloIntervalVar)
    return jcall(cp.cp, "getLengthMax", jint, (IloIntervalVar,), var)
end

function cpo_java_getlengthmin(cp::JavaCPOModel, var::IloIntervalVar)
    return jcall(cp.cp, "getLengthMin", jint, (IloIntervalVar,), var)
end

function cpo_java_getmax(cp::JavaCPOModel, var)
    return jcall(cp.cp, "getMax", jdouble, (IloNumVar,), var)
end

function cpo_java_getmin(cp::JavaCPOModel, var)
    return jcall(cp.cp, "getMin", jdouble, (IloNumVar,), var)
end

function cpo_java_getnext(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar)
    return jcall(cp.cp, "getNext", IloIntervalVar, (IloIntervalSequenceVar, IloIntervalVar), var_seq, var_interval)
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

function cpo_java_getprev(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar)
    return jcall(cp.cp, "getPrev", IloIntervalVar, (IloIntervalSequenceVar, IloIntervalVar), var_seq, var_interval)
end

function cpo_java_getsize(cp::JavaCPOModel, var::IloIntervalVar)
    return jcall(cp.cp, "getSize", jint, (IloIntervalVar,), var)
end

function cpo_java_getsizemax(cp::JavaCPOModel, var::IloIntervalVar)
    return jcall(cp.cp, "getSizeMax", jint, (IloIntervalVar,), var)
end

function cpo_java_getsizemin(cp::JavaCPOModel, var::IloIntervalVar)
    return jcall(cp.cp, "getSizeMin", jint, (IloIntervalVar,), var)
end

function cpo_java_getstart(cp::JavaCPOModel, var::IloIntervalVar)
    return jcall(cp.cp, "getStart", jint, (IloIntervalVar,), var)
end

function cpo_java_getstartmax(cp::JavaCPOModel, var::IloIntervalVar)
    return jcall(cp.cp, "getStartMax", jint, (IloIntervalVar,), var)
end

function cpo_java_getstartmin(cp::JavaCPOModel, var::IloIntervalVar)
    return jcall(cp.cp, "getStartMin", jint, (IloIntervalVar,), var)
end

function cpo_java_getvalue_intexpr(cp::JavaCPOModel, expr) # TODO: cannot use Julia method dispatch due to missing type for expressions/variables (int/num).
    return jcall(cp.cp, "getValue", jdouble, (IloIntExpr,), expr)
end

function cpo_java_getvalue_intvar(cp::JavaCPOModel, expr)
    return jcall(cp.cp, "getValue", jdouble, (IloIntVar,), expr)
end

function cpo_java_getvalue_numexpr(cp::JavaCPOModel, expr)
    return jcall(cp.cp, "getValue", jdouble, (IloNumExpr,), expr)
end

function cpo_java_getversion(cp::JavaCPOModel)
    return jcall(cp.cp, "getValue", JString, ())
end

function cpo_java_hasobjective(cp::JavaCPOModel)
    return jcall(cp.cp, "hasObjective", jboolean, ())
end

function cpo_java_isabsent(cp::JavaCPOModel, var::IloIntervalVar)
    return jcall(cp.cp, "isAbsent", jboolean, (IloIntervalVar), var)
end

function cpo_java_isfixed_intervalsequencevar(cp::JavaCPOModel, var::IloIntervalSequenceVar) # TODO: cannot use Julia method dispatch due to missing type for variables.
    return jcall(cp.cp, "isFixed", jboolean, (IloIntervalSequenceVar), var)
end

function cpo_java_isfixed_intervalvar(cp::JavaCPOModel, var::IloIntervalVar)
    return jcall(cp.cp, "isFixed", jboolean, (IloIntervalVar), var)
end

function cpo_java_isfixed_numvar(cp::JavaCPOModel, var)
    return jcall(cp.cp, "isFixed", jboolean, (IloNumVar,), var)
end

function cpo_java_isindomain(cp::JavaCPOModel, var, value::Integer)
    return jcall(cp.cp, "isInDomain", jboolean, (IloNumVar, jint), var, value)
end

function cpo_java_ispresent(cp::JavaCPOModel, var::IloIntervalVar)
    return jcall(cp.cp, "isPresent", jboolean, (IloIntervalVar,), var)
end

function cpo_java_next(cp::JavaCPOModel)
    return jcall(cp.cp, "next", jboolean, ())
end

function cpo_java_propagate(cp::JavaCPOModel)
    return jcall(cp.cp, "propagate", jboolean, ())
end

function cpo_java_propagate(cp::JavaCPOModel, constr)
    return jcall(cp.cp, "propagate", jboolean, (IloConstraint,), constr)
end

function cpo_java_refineconflict(cp::JavaCPOModel)
    return jcall(cp.cp, "refineConflict", jboolean, ())
end

function cpo_java_refineconflict(cp::JavaCPOModel, constrs::Vector)
    return jcall(cp.cp, "refineConflict", jboolean, (Vector{IloConstraint},), constrs)
end

function cpo_java_refineconflict(cp::JavaCPOModel, constrs::Vector, prefs::Vector{T}) where {T <: Real}
    return jcall(cp.cp, "refineConflict", jboolean, (Vector{IloConstraint}, Vector{jdouble}), constrs, prefs)
end

function cpo_java_restore(cp::JavaCPOModel, solution::IloSolution)
    return jcall(cp.cp, "restore", jboolean, (IloSolution,), solution)
end

function cpo_java_solve(cp::JavaCPOModel)
    # Bool due to https://github.com/JuliaInterop/JavaCall.jl/issues/110
    return Bool(jcall(cp.cp, "solve", jboolean, ()))
end

function cpo_java_startnewsearch(cp::JavaCPOModel)
    return jcall(cp.cp, "startNewSearch", Nothing, ())
end

function cpo_java_store(cp::JavaCPOModel, solution::IloSolution)
    return jcall(cp.cp, "store", Nothing, (IloSolution,), solution)
end

## Solution IloSolution.

function cpo_java_solution(cp::JavaCPOModel)
    return jcall(cp.cp, "solution", IloSolution, ())
end

function cpo_java_solution_add_intervalvar(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar) # TODO: cannot use Julia method dispatch due to missing type for variables (int/interval/num).
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "add", Nothing, (IloIntervalVar,), var)
end

function cpo_java_solution_add_intervalvararray(cp::JavaCPOModel, solution::IloSolution, vars::Vector{IloIntervalVar})
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "add", Nothing, (Vector{IloIntervalVar},), var)
end

function cpo_java_solution_add_intvar(cp::JavaCPOModel, solution::IloSolution, var)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "add", Nothing, (IloIntVar,), var)
end

function cpo_java_solution_add_intvararray(cp::JavaCPOModel, solutio::IloSolution, vars::Vector)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "add", Nothing, (Vector{IloIntVar},), vars)
end

function cpo_java_solution_add_numvar(cp::JavaCPOModel, solution::IloSolution, var)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "add", Nothing, (IloNumVar,), var)
end

function cpo_java_solution_contains_intervalvar(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar) # TODO: cannot use Julia method dispatch due to missing type for variables (int/interval/num).
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "contains", jboolean, (IloIntervalVar,), var)
end

function cpo_java_solution_contains_intvar(cp::JavaCPOModel, solution::IloSolution, var)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "contains", jboolean, (IloIntVar,), var)
end

function cpo_java_solution_end(cp::JavaCPOModel, solution::IloSolution)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "end", jboolean, ())
end

function cpo_java_solution_getend(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "getEnd", jint, (IloIntervalVar,), var)
end

function cpo_java_solution_getendmax(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "getEndMax", jint, (IloIntervalVar,), var)
end

function cpo_java_solution_getendmin(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "getEndMin", jint, (IloIntervalVar,), var)
end

function cpo_java_solution_getlength(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "getLength", jint, (IloIntervalVar,), var)
end

function cpo_java_solution_getlengthmax(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "getLengthMax", jint, (IloIntervalVar,), var)
end

function cpo_java_solution_getlengthmin(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
        # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "getLengthMin", jint, (IloIntervalVar,), var)
end

function cpo_java_solution_getmax_int(cp::JavaCPOModel, solution::IloSolution, var) # TODO: cannot use Julia method dispatch due to missing type for variables (int/interval/num).
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "getMax", jint, (IloIntVar,), var)
end

function cpo_java_solution_getmax_num(cp::JavaCPOModel, solution::IloSolution, var)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "getMax", jdouble, (IloNumVar,), var)
end

function cpo_java_solution_getmin_int(cp::JavaCPOModel, solution::IloSolution, var) # TODO: cannot use Julia method dispatch due to missing type for variables (int/interval/num).
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "getMin", jint, (IloIntVar,), var)
end

function cpo_java_solution_getmin_num(cp::JavaCPOModel, solution::IloSolution, var)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "getMin", jdouble, (IloNumVar,), var)
end

function cpo_java_solution_getsize(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "getSize", jint, (IloIntervalVar,), var)
end

function cpo_java_solution_getsizemax(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "getSizeMax", jint, (IloIntervalVar,), var)
end

function cpo_java_solution_getsizemin(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "getSizeMin", jint, (IloIntervalVar,), var)
end

function cpo_java_solution_getstart(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "getStart", jint, (IloIntervalVar,), var)
end

function cpo_java_solution_getstartmax(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "getStartMax", jint, (IloIntervalVar,), var)
end

function cpo_java_solution_getstartmin(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "getStartMin", jint, (IloIntervalVar,), var)
end

function cpo_java_solution_getvalue_int(cp::JavaCPOModel, solution::IloSolution, var) # TODO: cannot use Julia method dispatch due to missing type for variables (int/interval/num).
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "getValue", jint, (IloIntVar,), var)
end

function cpo_java_solution_getvalue_num(cp::JavaCPOModel, solution::IloSolution, var)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "getValue", jdouble, (IloNumVar,), var)
end

function cpo_java_solution_isabsent(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "isAbsent", jboolean, (IloIntervalVar,), var)
end

function cpo_java_solution_isfixed(cp::JavaCPOModel, solution::IloSolution, var)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "isFixed", jboolean, (IloIntVar,), var)
end

function cpo_java_solution_isindomain(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar, value::Integer)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "isInDomain", jboolean, (IloIntervalVar, jint), var, value)
end

function cpo_java_solution_ispresent(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "isPresent", jboolean, (IloIntervalVar,), var)
end

function cpo_java_solution_remove_intervalvar(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar) # TODO: cannot use Julia method dispatch due to missing type for variables (int/interval/num).
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "remove", Nothing, (IloIntervalVar,), var)
end

function cpo_java_solution_remove_intervalvararray(cp::JavaCPOModel, solution::IloSolution, vars::Vector{IloIntervalVar})
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "remove", Nothing, (Vector{IloIntervalVar},), vars)
end

function cpo_java_solution_remove_intvar(cp::JavaCPOModel, solution::IloSolution, var)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "remove", Nothing, (IloIntVar,), var)
end

function cpo_java_solution_remove_intvararray(cp::JavaCPOModel, solution::IloSolution, vars::Vector)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "remove", Nothing, (Vector{IloIntVar},), vars)
end

function cpo_java_solution_setabsent(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setAbsent", void, (IloIntervalVar,), var)
end

function cpo_java_solution_setdomain(cp::JavaCPOModel, solution::IloSolution, var, vmin::Integer, vmax::Integer)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setDomain", jboolean, (IloIntVar, jint, jint), var, vmin, vmax)
end

function cpo_java_solution_setend(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar, v::Integer)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setEnd", void, (IloIntervalVar, jint), var, v)
end

function cpo_java_solution_setendmax(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar, v::Integer)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setEndMax", void, (IloIntervalVar, jint), var, v)
end

function cpo_java_solution_setendmin(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar, v::Integer)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setEndMin", void, (IloIntervalVar, jint), var, v)
end

function cpo_java_solution_setlength(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar, v::Integer)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setLength", void, (IloIntervalVar, jint), var, v)
end

function cpo_java_solution_setlengthmax(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar, v::Integer)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setLengthMax", void, (IloIntervalVar, jint), var, v)
end

function cpo_java_solution_setlengthmin(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar, v::Integer)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setLengthMin", void, (IloIntervalVar, jint), var, v)
end

function cpo_java_solution_setmax_int(cp::JavaCPOModel, solution::IloSolution, var, v::Integer) # TODO: cannot use Julia method dispatch due to missing type for variables (int/interval/num).
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setMax", jboolean, (IloIntVar, jint), var, v)
end

function cpo_java_solution_setmax_num(cp::JavaCPOModel, solution::IloSolution, var, v::Integer)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setMax", jboolean, (IloNumVar, jint), var, v)
end

function cpo_java_solution_setmin_int(cp::JavaCPOModel, solution::IloSolution, var, v::Integer) # TODO: cannot use Julia method dispatch due to missing type for variables (int/interval/num).
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setMin", jboolean, (IloIntVar, jint), var, v)
end

function cpo_java_solution_setmin_num(cp::JavaCPOModel, solution::IloSolution, var, v::Integer)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setMin", jboolean, (IloNumVar, jint), var, v)
end

function cpo_java_solution_setoptional(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setOptional", void, (IloIntervalVar,), var)
end

function cpo_java_solution_setpresent(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setPresent", void, (IloIntervalVar,), var)
end

function cpo_java_solution_setsize(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar, v::Integer)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setSize", jboolean, (IloIntervalVar, jint), var, v)
end

function cpo_java_solution_setsizemax(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar, v::Integer)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setSizeMax", jboolean, (IloIntervalVar, jint), var, v)
end

function cpo_java_solution_setsizemin(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar, v::Integer)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setSizeMin", jboolean, (IloIntervalVar, jint), var, v)
end

function cpo_java_solution_setstart(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar, v::Integer)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setStart", jboolean, (IloIntervalVar, jint), var, v)
end

function cpo_java_solution_setstartmax(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar, v::Integer)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setStartMax", jboolean, (IloIntervalVar, jint), var, v)
end

function cpo_java_solution_setstartmin(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar, v::Integer)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setStartMin", jboolean, (IloIntervalVar, jint), var, v)
end

function cpo_java_solution_setvalue_int(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar, v::Integer) # TODO: cannot use Julia method dispatch due to missing type for variables (int/interval/num).
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setValue", jboolean, (IloIntVar, jint), var, v)
end

function cpo_java_solution_setvalue_num(cp::JavaCPOModel, solution::IloSolution, var, v::Integer)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setValue", jboolean, (IloNumVar, jint), var, v)
end

function cpo_java_solution_store(cp::JavaCPOModel, solution::IloSolution)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "store", jboolean, ())
end

## Miscellaneous

function cpo_java_dumpmodel(cp::JavaCPOModel, filename::String)
    return jcall(cp.cp, "dumpModel", Nothing, (JString,), filename)
    # TODO: OutputStream?
end

function cpo_java_exportmodel(cp::JavaCPOModel, filename::String)
    return jcall(cp.cp, "exportModel", Nothing, (JString,), filename)
    # TODO: OutputStream?
end

function cpo_java_getbuildid(cp::JavaCPOModel)
    return jcall(cp.cp, "getBuildID", JString)
end

function cpo_java_getconflict_constraint(cp::JavaCPOModel, constr) # TODO: cannot use Julia method dispatch due to missing type for expressions (interval/num) and constraints.
    return jcall(cp.cp, "getConflict", ConflictStatus, (IloConstraint,), constr)
end

function cpo_java_getconflict_intervalval(cp::JavaCPOModel, var::IloIntervalVar)
    return jcall(cp.cp, "getConflict", ConflictStatus, (IloIntervalVar,), constr)
end

function cpo_java_getconflict_numvar(cp::JavaCPOModel, var)
    return jcall(cp.cp, "getConflict", ConflictStatus, (IloNumVar,), constr)
end

function cpo_java_importmodel(cp::JavaCPOModel, filename::String)
    return jcall(cp.cp, "importModel", Nothing, (JString,), filename)
    # TODO: InputStream?
end

function cpo_java_printinformation(cp::JavaCPOModel)
    return jcall(cp.cp, "printInformation", Nothing, ())
    # TODO: OutputStream?
end

function cpo_java_remove(cp::JavaCPOModel, addable)
    return jcall(cp.cp, "remove", IloAddable, (IloAddable,), addable)
end

function cpo_java_removeallcallbacks(cp::JavaCPOModel)
    return jcall(cp.cp, "removeAllCallbacks", Nothing, ())
end

function cpo_java_removeallkpis(cp::JavaCPOModel)
    return jcall(cp.cp, "removeAllKPIs", Nothing, ())
end

function cpo_java_removecallback(cp::JavaCPOModel, cb)
    return jcall(cp.cp, "removeCallback", Nothing, (Callback,), cb)
end

function cpo_java_removekpi(cp::JavaCPOModel, kpi::String)
    return jcall(cp.cp, "removeKPI", Nothing, (JString,), kpi)
end

function cpo_java_runseeds(cp::JavaCPOModel, n::Integer)
    return jcall(cp.cp, "runSeeds", Nothing, (jint,), n)
end

# TODO: searchPhase
