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
const IloLinearIntExpr = JavaObject{Symbol("ilog.concert.IloLinearIntExpr")}
const IloNumExpr = JavaObject{Symbol("ilog.concert.IloNumExpr")}
const IloLinearNumExpr = JavaObject{Symbol("ilog.concert.IloLinearNumExpr")}
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
const IloAnd = JavaObject{Symbol("ilog.concert.IloAnd")}
const IloOr = JavaObject{Symbol("ilog.concert.IloOr")}

const IloObjective = JavaObject{Symbol("ilog.concert.IloObjective")}
const IloMultiCriterionExpr = JavaObject{Symbol("ilog.concert.IloMultiCriterionExpr")}
const IloSolution = JavaObject{Symbol("ilog.concert.IloSolution")}
const IloAddable = JavaObject{Symbol("ilog.concert.IloAddable")}

const Callback = JavaObject{Symbol("ilog.cp.IloCP\$Callback")}
const ConflictStatus = JavaObject{Symbol("ilog.concert.IloCP\$ConflictStatus")}

const IloDoubleInfo = JavaObject{Symbol("ilog.cp.IloCP\$DoubleInfo")} # No need to export
const IloIntInfo = JavaObject{Symbol("ilog.cp.IloCP\$IntInfo")} # No need to export
const IloDoubleParam = JavaObject{Symbol("ilog.cp.IloCP\$DoubleParam")} # No need to export
const IloIntParam = JavaObject{Symbol("ilog.cp.IloCP\$IntParam")} # No need to export
const IloParameterValues = JavaObject{Symbol("ilog.cp.IloCP\$ParameterValues")} # No need to export

# Unions of types to model Java type hierarchy.
const Constraint = Union{IloConstraint, IloAlternative, IloIsomorphism, IloNoOverlap, IloRange, IloSpan, IloSynchronize, IloAnd, IloOr}
const IntExpr = Union{IloIntVar, IloIntExpr, IloLinearIntExpr, Constraint}
const NumVar = Union{IloIntVar, IloNumVar}
const NumExpr = Union{IntExpr, IloNumVar, IloNumExpr, IloLinearNumExpr}
const Variable = Union{NumVar, IloIntervalVar, IloIntervalSequenceVar}
const Addable = Union{Constraint, IloObjective, IloMultiCriterionExpr, Variable, NumExpr}

const ConstraintArray = Union{Vector{Constraint}, Vector{T} where {T <: Constraint}}
const IntExprArray = Union{Vector{IntExpr}, Vector{T} where {T <: IntExpr}}
const NumVarArray = Union{Vector{NumVar}, Vector{T} where {T <: NumVar}}
const NumExprArray = Union{Vector{NumExpr}, Vector{T} where {T <: NumExpr}}
const AddableArray = Union{Vector{Addable}, Vector{T} where {T <: Addable}}

# Variable bounds. Name from C++ API, as they don't exist in Java.
# const JavaDouble = @jimport java.lang.Double
# const JavaInteger = @jimport java.lang.Integer
# const IloInfinity = jfield(JavaDouble, "MAX_VALUE", jdouble)
# const IloMaxInt = jfield(JavaInteger, "MAX_VALUE", jdouble)
# const IloMinInt = jfield(JavaInteger, "MIN_VALUE", jdouble)
const IloInfinity = typemax(Float64)
const IloMaxInt = typemax(Int32)
const IloMinInt = typemin(Int32)

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
    # TODO: move this to JavaCPOModel constructor?

    # Getting access to the values is not possible before the JVM is started, i.e. before cpo_java_init() is called.
    # It is thus not possible to use const for these values, unfortunately. TODO: Or just hard-code the values?
    conflictstatus = @jimport ilog.cp.IloCP$ConflictStatus
    conflictstatus_possible = jfield(conflictstatus, "ConflictPossibleMember", conflictstatus)
    conflictstatus_member = jfield(conflictstatus, "ConflictMember", conflictstatus)
    conflictstatus_excluded = jfield(conflictstatus, "ConflictExcluded", conflictstatus)

    # Actually build the model.
    jcp = @jimport ilog.cp.IloCP
    model = jcp(())

    # Return the right data structure
    return JavaCPOModel(model, conflictstatus_possible, conflictstatus_member, conflictstatus_excluded)
end

function cpo_java_release(cp::JavaCPOModel)
    jcall(cp.cp, "end", Nothing, ())
end

## Variable creation

# Boolean variables
function cpo_java_boolvar(cp::JavaCPOModel, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "boolVar", IloIntVar, ())
    else
        return jcall(cp.cp, "boolVar", IloIntVar, (JString,), name)
    end
end

function cpo_java_boolvararray(cp::JavaCPOModel, n::Int32, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "boolVarArray", Vector{IloIntVar}, (jint,), n)
    else
        return jcall(cp.cp, "boolVarArray", Vector{IloIntVar}, (jint, JString), n, name)
    end
end

# Integer variables
function cpo_java_intvar(cp::JavaCPOModel, lb::Int32, ub::Int32, name::String="")
    return jcall(cp.cp, "intVar", IloIntVar, (jint, jint, JString), lb, ub, name)
end

function cpo_java_intvar(cp::JavaCPOModel, values::Vector{Int32}, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "intVar", IloIntVar, (Vector{jint},), values)
    else
        return jcall(cp.cp, "intVar", IloIntVar, (Vector{jint}, JString), values, name)
    end
end

function cpo_java_intvararray(cp::JavaCPOModel, n::Int32)
    return jcall(cp.cp, "intVarArray", Vector{IloIntVar}, (jint,), n)
end

function cpo_java_intvararray(cp::JavaCPOModel, n::Int32, lb::Int32, ub::Int32, name::String="")
    # Java API doesn't allow no name.
    return jcall(cp.cp, "intVarArray", Vector{IloIntVar}, (jint, jint, jint, JString), n, lb, ub, name)
end

function cpo_java_intvararray(cp::JavaCPOModel, n::Int32, values::Vector{Int32}, name::String="")
    # Java API doesn't allow no name.
    return jcall(cp.cp, "intVarArray", Vector{IloIntVar}, (jint, Vector{jint}, JString), n, values, name)
end

# Numerical variables
function cpo_java_numvar(cp::JavaCPOModel, lb::T, ub::T, name::String="") where {T <: Real}
    if length(name) == 0
        return jcall(cp.cp, "numVar", IloNumVar, (jdouble, jdouble), lb, ub)
    else
        return jcall(cp.cp, "numVar", IloNumVar, (jdouble, jdouble, JString), lb, ub, name)
    end
    # TODO: IloNumVarType? Should not be required, as there is IntVar, BoolVar, NumVar.
end

function cpo_java_numvararray(cp::JavaCPOModel, n::Int32, lb::T, ub::T, name::String="") where {T <: Union{Float64, Int32}}
    if length(name) == 0
        return jcall(cp.cp, "numVarArray", Vector{IloNumVar}, (jint, jdouble, jdouble, JString), n, lb, ub, name)
    else
        return jcall(cp.cp, "numVarArray", Vector{IloNumVar}, (jint, jdouble, jdouble), n, lb, ub)
    end
end

# Interval variables
function cpo_java_intervalvar(cp::JavaCPOModel, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "intervalVar", IloIntervalVar, ())
    else
        return jcall(cp.cp, "intervalVar", IloIntervalVar, (JString,), name)
    end
end

function cpo_java_intervalvar(cp::JavaCPOModel, size::Int32, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "intervalVar", IloIntervalVar, (jint,), size)
    else
        return jcall(cp.cp, "intervalVar", IloIntervalVar, (jint, JString), size, name)
    end
end

function cpo_java_intervalvar(cp::JavaCPOModel, size_lb::Int32, size_ub::Int32)
    return jcall(cp.cp, "intervalVar", IloIntervalVar, (jint, jint), size_lb, size_ub)
end

function cpo_java_intervalvar(cp::JavaCPOModel, size_lb::Int32, size_ub::Int32, opt::Bool, intensity, granularity::Int32)
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

function cpo_java_intervalsequencevar(cp::JavaCPOModel, intervalvararray::Vector{IloIntervalVar}, types::Vector{Int32}, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "intervalSequenceVar", IloIntervalSequenceVar, (Vector{IloIntervalVar}, Vector{jint}), intervalvararray, types)
    else
        return jcall(cp.cp, "intervalSequenceVar", IloIntervalSequenceVar, (Vector{IloIntervalVar}, Vector{jint}, JString), intervalvararray, types, name)
    end
end

# Getters and setters
function cpo_java_numvar_getlb(cp::JavaCPOModel, var::NumVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(var, "getLB", jdouble, ())
end

function cpo_java_numvar_getub(cp::JavaCPOModel, var::NumVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(var, "getUB", jdouble, ())
end

function cpo_java_numvar_setlb(cp::JavaCPOModel, var::NumVar, value::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(var, "setLB", Nothing, (jdouble,), value)
end

function cpo_java_numvar_setub(cp::JavaCPOModel, var::NumVar, value::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(var, "setUB", Nothing, (jdouble,), value)
end

function cpo_java_intvar_getlb(cp::JavaCPOModel, var::IloIntVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return Int(jcall(var, "getLB", jdouble, ()))
end

function cpo_java_intvar_getub(cp::JavaCPOModel, var::IloIntVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return Int(jcall(var, "getUB", jdouble, ()))
end

function cpo_java_intvar_setlb(cp::JavaCPOModel, var::IloIntVar, value::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(var, "setLB", Nothing, (jdouble,), value)
end

function cpo_java_intvar_setub(cp::JavaCPOModel, var::IloIntVar, value::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(var, "setUB", Nothing, (jdouble,), value)
end

## Expression creation

function cpo_java_abs(cp::JavaCPOModel, var::IntExpr)
    return jcall(cp.cp, "abs", IloIntExpr, (IloIntExpr,), var)
end

function cpo_java_constant(cp::JavaCPOModel, value::Int)
    return jcall(cp.cp, "constant", IloIntExpr, (jint,), value)
end

function cpo_java_constant(cp::JavaCPOModel, value::Real)
    return jcall(cp.cp, "constant", IloNumExpr, (jdouble,), value)
end

function cpo_java_count(cp::JavaCPOModel, exprs::IntExprArray, value::Int32)
    return jcall(cp.cp, "count", IloIntExpr, (Vector{IloIntExpr}, jint), exprs, value)
end

function cpo_java_countdifferent(cp::JavaCPOModel, exprs::IntExprArray)
    return jcall(cp.cp, "countDifferent", IloIntExpr, (Vector{IloIntExpr},), exprs,)
end

function cpo_java_div(cp::JavaCPOModel, expr_a::IntExpr, expr_b::IntExpr)
    return jcall(cp.cp, "div", IloIntExpr, (IloIntExpr, IloIntExpr), expr_a, expr_b)
end

function cpo_java_div(cp::JavaCPOModel, int_a::Int32, expr_b::IntExpr)
    return jcall(cp.cp, "div", IloIntExpr, (jint, IloIntExpr), int_a, expr_b)
end

function cpo_java_div(cp::JavaCPOModel, expr_a::IntExpr, int_b::Int32)
    return jcall(cp.cp, "div", IloIntExpr, (IloIntExpr, jint), expr_a, int_b)
end

function cpo_java_element(cp::JavaCPOModel, values::Vector{Int32}, expr_index::IntExpr)
    return jcall(cp.cp, "element", IloIntExpr, (Vector{jint}, IloIntExpr), values, expr_index)
end

function cpo_java_element(cp::JavaCPOModel, exprs::IntExprArray, expr_index::IntExpr)
    return jcall(cp.cp, "element", IloIntExpr, (Vector{IloIntExpr}, IloIntExpr), exprs, expr_index)
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

function cpo_java_endof(cp::JavaCPOModel, var::IloIntervalVar, absval::Int32)
    return jcall(cp.cp, "endOf", IloIntExpr, (IloIntervalVar, jint), var, absval)
end

function cpo_java_enfofnext(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, lastval::Int32)
    return jcall(cp.cp, "endOfNext", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint), var_seq, var_interval, lastval)
end

function cpo_java_enfofnext(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, lastval::Int32, absval::Int32)
    return jcall(cp.cp, "endOfNext", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint, jint), var_seq, var_interval, lastval, absval)
end

function cpo_java_enfofprevious(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, firstval::Int32)
    return jcall(cp.cp, "endOfPrevious", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint), var_seq, var_interval, firstval)
end

function cpo_java_enfofprevious(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, firstval::Int32, absval::Int32)
    return jcall(cp.cp, "endOfPrevious", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint, jint), var_seq, var_interval, firstval, absval)
end

function cpo_java_exponent(cp::JavaCPOModel, expr::NumExpr)
    return jcall(cp.cp, "exponent", IloNumExpr, (IloNumExpr,), expr)
end

function cpo_java_heightatend(cp::JavaCPOModel, a::IloIntervalVar, f::IloCumulFunctionExpr)
    return jcall(cp.cp, "heightAtEnd", IloIntExpr, (IloIntervalVar, IloCumulFunctionExpr), a, f)
end

function cpo_java_heightatend(cp::JavaCPOModel, a::IloIntervalVar, f::IloCumulFunctionExpr, absval::Int32)
    return jcall(cp.cp, "heightAtEnd", IloIntExpr, (IloIntervalVar, IloCumulFunctionExpr, jint), a, f, absval)
end

function cpo_java_heightatstart(cp::JavaCPOModel, a::IloIntervalVar, f::IloCumulFunctionExpr)
    return jcall(cp.cp, "heightAtStart", IloIntExpr, (IloIntervalVar, IloCumulFunctionExpr), a, f)
end

function cpo_java_heightatstart(cp::JavaCPOModel, a::IloIntervalVar, f::IloCumulFunctionExpr, absval::Int32)
    return jcall(cp.cp, "heightAtStart", IloIntExpr, (IloIntervalVar, IloCumulFunctionExpr, jint), a, f, absval)
end

function cpo_java_intexpr(cp::JavaCPOModel, constr::Constraint)
    return jcall(cp.cp, "intExpr", IloIntExpr, (IloConstraint,), constr)
end

function cpo_java_intexprarray(cp::JavaCPOModel, n::Int32)
    return jcall(cp.cp, "intExprArray", Vector{IloIntExpr}, (jint,), n)
end

function cpo_java_lengthof(cp::JavaCPOModel, var::IloIntervalVar)
    return jcall(cp.cp, "lengthOf", IloIntExpr, (IloIntervalVar,), var)
end

function cpo_java_lengthof(cp::JavaCPOModel, var::IloIntervalVar, absval::Int32)
    return jcall(cp.cp, "lengthOf", IloIntExpr, (IloIntervalVar, jint), var, absval)
end

function cpo_java_lengthofnext(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, lastval::Int32)
    return jcall(cp.cp, "lengthOfNext", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint), var_seq, var_interval, lastval)
end

function cpo_java_lengthofnext(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, lastval::Int32, absval::Int32)
    return jcall(cp.cp, "lengthOfNext", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint, jint), var_seq, var_interval, lastval, absval)
end

function cpo_java_lengthofprevious(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, firstval::Int32)
    return jcall(cp.cp, "lengthOfPrevious", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint), var_seq, var_interval, lastval)
end

function cpo_java_lengthofprevious(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, firstval::Int32, absval::Int32)
    return jcall(cp.cp, "lengthOfPrevious", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint, jint), var_seq, var_interval, lastval, absval)
end

function cpo_java_log(cp::JavaCPOModel, expr::NumExpr)
    return jcall(cp.cp, "log", IloNumExpr, (IloNumExpr,), expr)
end

function cpo_java_max(cp::JavaCPOModel, val::Real, expr::NumExpr)
    return jcall(cp.cp, "max", IloNumExpr, (jdouble, IloNumExpr), val, expr)
end

function cpo_java_max(cp::JavaCPOModel, exprs::IntExprArray)
    return jcall(cp.cp, "max", IloIntExpr, (Vector{IloIntExpr},), exprs)
end

function cpo_java_max(cp::JavaCPOModel, expr_a::IntExpr, expr_b::IntExpr)
    return jcall(cp.cp, "max", IloIntExpr, (IloIntExpr, IloIntExpr), expr_a, expr_b)
end

function cpo_java_max(cp::JavaCPOModel, expr::IntExpr, val::Int32)
    return jcall(cp.cp, "max", IloIntExpr, (IloIntExpr, jint), expr, val)
end

function cpo_java_max(cp::JavaCPOModel, exprs::NumExprArray)
    return jcall(cp.cp, "max", IloNumExpr, (Vector{IloNumExpr},), exprs)
end

function cpo_java_max(cp::JavaCPOModel, expr::NumExpr, val::Real)
    return jcall(cp.cp, "max", IloNumExpr, (IloNumExpr, jdouble), expr, val)
end

function cpo_java_max(cp::JavaCPOModel, expr::NumExpr, val::NumExpr)
    return jcall(cp.cp, "max", IloNumExpr, (IloNumExpr, IloNumExpr), expr_a, expr_b)
end

function cpo_java_max(cp::JavaCPOModel, val::Int32, expr::IntExpr)
    return jcall(cp.cp, "max", IloIntExpr, (jint, IloIntExpr), val, expr)
end

function cpo_java_min(cp::JavaCPOModel, val::Real, expr::NumExpr)
    return jcall(cp.cp, "min", IloNumExpr, (jdouble, IloNumExpr), val, expr)
end

function cpo_java_min(cp::JavaCPOModel, exprs::IntExprArray)
    return jcall(cp.cp, "min", IloIntExpr, (Vector{IloIntExpr},), exprs)
end

function cpo_java_min(cp::JavaCPOModel, expr_a::IntExpr, expr_b::IntExpr)
    return jcall(cp.cp, "min", IloIntExpr, (IloIntExpr, IloIntExpr), expr_a, expr_b)
end

function cpo_java_min(cp::JavaCPOModel, expr::IntExpr, val::Int32)
    return jcall(cp.cp, "min", IloIntExpr, (IloIntExpr, jint), expr, val)
end

function cpo_java_min(cp::JavaCPOModel, exprs::NumExprArray)
    return jcall(cp.cp, "min", IloNumExpr, (Vector{IloNumExpr},), exprs)
end

function cpo_java_min(cp::JavaCPOModel, expr::NumExpr, val::Real)
    return jcall(cp.cp, "min", IloNumExpr, (IloNumExpr, jdouble), expr, val)
end

function cpo_java_min(cp::JavaCPOModel, expr::NumExpr, val::NumExpr)
    return jcall(cp.cp, "min", IloNumExpr, (IloNumExpr, IloNumExpr), expr_a, expr_b)
end

function cpo_java_min(cp::JavaCPOModel, val::Int32, expr::IntExpr)
    return jcall(cp.cp, "min", IloIntExpr, (jint, IloIntExpr), val, expr)
end

function cpo_java_modulo(cp::JavaCPOModel, expr, r::Int32)
    return jcall(cp.cp, "modulo", IloIntExpr, (IloIntExpr, jint), expr, r)
end

function cpo_java_negative(cp::JavaCPOModel, expr::IntExpr)
    return jcall(cp.cp, "negative", IloIntExpr, (IloIntExpr,), expr)
end

function cpo_java_negative(cp::JavaCPOModel, expr::NumExpr)
    return jcall(cp.cp, "negative", IloNumExpr, (IloNumExpr,), expr)
end

function cpo_java_numexprarray(cp::JavaCPOModel, n::Int32)
    return jcall(cp.cp, "numExprArray", IloIntExpr, (jint,), n)
end

function cpo_java_numvararray(cp::JavaCPOModel, n::Int32)
    return jcall(cp.cp, "numVarArray", IloIntExpr, (jint,), n)
end

function cpo_java_overlaplength(cp::JavaCPOModel, var_a::IloIntervalVar, var_b::IloIntervalVar)
    return jcall(cp.cp, "overlapLength", IloIntExpr, (IloIntervalVar, IloIntervalVar), var_a, var_b)
end

function cpo_java_overlaplength(cp::JavaCPOModel, var_a::IloIntervalVar, var_b::IloIntervalVar, absval::Int32)
    return jcall(cp.cp, "overlapLength", IloIntExpr, (IloIntervalVar, IloIntervalVar, jint), var_a, var_b, absval)
end

function cpo_java_overlaplength(cp::JavaCPOModel, var::IloIntervalVar, start::Int32, end_::Int32)
    return jcall(cp.cp, "overlapLength", IloIntExpr, (IloIntervalVar, jint, jint), var, start, end_)
end

function cpo_java_overlaplength(cp::JavaCPOModel, var::IloIntervalVar, start::Int32, end_::Int32, absval::Int32)
    return jcall(cp.cp, "overlapLength", IloIntExpr, (IloIntervalVar, jint, jint, jint), var, start, end_, absval)
end

function cpo_java_piecewiselinear(cp::JavaCPOModel, var::IloIntervalVar, point::Vector{T}, slope::Vector{T}, a::Real, fa::Real) where {T <: Real}
    return jcall(cp.cp, "piecewiseLinear", IloNumExpr, (IloNumExpr, Vector{jdouble}, Vector{jdouble}, jdouble, jdouble), var, point, slope, a, fa)
end

function cpo_java_piecewiselinear(cp::JavaCPOModel, var::IloIntervalVar, firstslope::Real, point::Vector{T}, value::Vector{T}, lastslope::Real) where {T <: Real}
    return jcall(cp.cp, "piecewiseLinear", IloNumExpr, (IloNumExpr, jdouble, Vector{jdouble}, Vector{jdouble}, jdouble), var, firstslope, point, value, lastslope)
end

function cpo_java_prod(cp::JavaCPOModel, expr_a::Real, expr_b::NumExpr)
    return jcall(cp.cp, "prod", IloNumExpr, (jdouble, IloNumExpr), expr_a, expr_b)
end

function cpo_java_prod(cp::JavaCPOModel, expr_a::Real, expr_b::NumVar, expr_c::NumVar)
    return jcall(cp.cp, "prod", IloNumExpr, (jdouble, IloNumVar, IloNumVar), expr_a, expr_b, expr_c)
end

function cpo_java_prod(cp::JavaCPOModel, expr_a::IntExpr, expr_b::IntExpr)
    return jcall(cp.cp, "prod", IloIntExpr, (IloIntExpr, IloIntExpr), expr_a, expr_b)
end

function cpo_java_prod(cp::JavaCPOModel, expr_a::IntExpr, expr_b::Int32)
    return jcall(cp.cp, "prod", IloIntExpr, (IloIntExpr, jint), expr_a, expr_b)
end

function cpo_java_prod(cp::JavaCPOModel, expr_a::NumExpr, expr_b::Real)
    return jcall(cp.cp, "prod", IloNumExpr, (IloNumExpr, jdouble), expr_a, expr_b)
end

function cpo_java_prod(cp::JavaCPOModel, expr_a::NumExpr, expr_b::NumExpr)
    return jcall(cp.cp, "prod", IloNumExpr, (IloNumExpr, IloNumExpr), expr_a, expr_b)
end

function cpo_java_prod(cp::JavaCPOModel, expr_a::NumVar, expr_b::Real, expr_c::NumVar)
    return jcall(cp.cp, "prod", IloNumExpr, (IloNumVar, jdouble, IloNumVar), expr_a, expr_b, expr_c)
end

function cpo_java_prod(cp::JavaCPOModel, expr_a::NumVar, expr_b::NumVar, expr_c::Real)
    return jcall(cp.cp, "prod", IloNumExpr, (IloNumVar, IloNumVar, jdouble), expr_a, expr_b, expr_c)
end

function cpo_java_prod(cp::JavaCPOModel, expr_a::Int32, expr_b::IntExpr)
    return jcall(cp.cp, "prod", IloIntExpr, (jint, IloIntExpr), expr_a, expr_b)
end

function cpo_java_power(cp::JavaCPOModel, expr_a::NumExpr, expr_b::Real)
    return jcall(cp.cp, "power", IloNumExpr, (IloNumExpr, jdouble), expr_a, expr_b)
end

function cpo_java_power(cp::JavaCPOModel, expr_a::NumExpr, expr_b::NumExpr)
    return jcall(cp.cp, "power", IloNumExpr, (IloNumExpr, IloNumExpr), expr_a, expr_b)
end

function cpo_java_prod(cp::JavaCPOModel, values::Vector{Int32}, exprs::IntExprArray)
    return jcall(cp.cp, "prod", IloIntExpr, (Vector{jint}, Vector{IloIntExpr}), values, exprs)
end

function cpo_java_prod(cp::JavaCPOModel, exprs::IntExprArray, values::Vector{Int32})
    return jcall(cp.cp, "prod", IloIntExpr, (Vector{IloIntExpr}, Vector{jint}), exprs, values)
end

function cpo_java_prod(cp::JavaCPOModel, exprs_a::IntExprArray, exprs_b::IntExprArray)
    return jcall(cp.cp, "prod", IloIntExpr, (Vector{IloIntExpr}, Vector{IloIntExpr}), exprs_a, exprs_b)
end

function cpo_java_pulse(cp::JavaCPOModel, var::IloIntervalVar, v::Int32)
    return jcall(cp.cp, "pulse", IloCumulFunctionExpr, (IloIntervalVar, jint), var, v)
end

function cpo_java_pulse(cp::JavaCPOModel, var::IloIntervalVar, vmin::Int32, vmax::Int32)
    return jcall(cp.cp, "pulse", IloCumulFunctionExpr, (IloIntervalVar, jint, jint), var, vmin, vmax)
end

function cpo_java_pulse(cp::JavaCPOModel, start::Int32, end_::Int32, v::Int32)
    return jcall(cp.cp, "pulse", IloCumulFunctionExpr, (jint, jint, jint), start, end_, v)
end

function cpo_java_quot(cp::JavaCPOModel, expr_a::Real, expr_b::NumExpr)
    return jcall(cp.cp, "quot", IloNumExpr, (jdouble, IloNumExpr), expr_a, expr_b)
end

function cpo_java_quot(cp::JavaCPOModel, expr_a::NumExpr, expr_b::Real)
    return jcall(cp.cp, "quot", IloNumExpr, (IloNumExpr, jdouble), expr_a, expr_b)
end

function cpo_java_quot(cp::JavaCPOModel, expr_a::NumExpr, expr_b::NumExpr)
    return jcall(cp.cp, "quot", IloNumExpr, (IloNumExpr, IloNumExpr), expr_a, expr_b)
end

function cpo_java_scalprod(cp::JavaCPOModel, coefs::Vector{T}, vars::NumVarArray) where {T <: Real}
    return jcall(cp.cp, "scalProd", IloLinearNumExpr, (Vector{jdouble}, Vector{IloNumVar}), coefs, vars)
end

function cpo_java_scalprod(cp::JavaCPOModel, coefs::Vector{T}, vars::NumVarArray, start::Int32, num::Int32) where {T <: Real}
    return jcall(cp.cp, "scalProd", IloLinearNumExpr, (Vector{jdouble}, Vector{IloNumVar}, jint, jint), coefs, vars, start, num)
end

function cpo_java_scalprod(cp::JavaCPOModel, coefs::Vector{IloIntVar}, vars::Vector{IloIntVar})
    return jcall(cp.cp, "scalProd", IloIntExpr, (Vector{IloIntVar}, Vector{IloIntVar}), coefs, vars)
end

function cpo_java_scalprod(cp::JavaCPOModel, coefs::Vector{IloIntVar}, vars::Vector{IloIntVar}, start::Int32, num::Int32)
    return jcall(cp.cp, "scalProd", IloIntExpr, (Vector{IloIntVar}, Vector{IloIntVar}, jint, jint), coefs, vars, start, num)
end

function cpo_java_scalprod(cp::JavaCPOModel, vars::Vector{IloIntVar}, coefs::Vector{Int32})
    return jcall(cp.cp, "scalProd", IloLinearIntExpr, (Vector{IloIntVar}, Vector{jint}), coefs, vars)
end

function cpo_java_scalprod(cp::JavaCPOModel, vars::Vector{IloIntVar}, coefs::Vector{Int32}, start::Int32, num::Int32)
    return jcall(cp.cp, "scalProd", IloLinearIntExpr, (Vector{IloIntVar}, Vector{jint}, jint, jint), coefs, vars, start, num)
end

function cpo_java_scalprod(cp::JavaCPOModel, vars::Vector{IloNumVar}, coefs::Vector{T}) where {T <: Real}
    return jcall(cp.cp, "scalProd", IloLinearNumExpr, (Vector{IloNumVar}, Vector{jdouble}), coefs, vars)
end

function cpo_java_scalprod(cp::JavaCPOModel, vars::Vector{IloNumVar}, coefs::Vector{T}, start::Int32, num::Int32) where {T <: Real}
    return jcall(cp.cp, "scalProd", IloLinearNumExpr, (Vector{IloNumVar}, Vector{jdouble}, jint, jint), coefs, vars, start, num)
end

function cpo_java_scalprod(cp::JavaCPOModel, coefs::Vector{IloNumVar}, vars::Vector{IloNumVar})
    return jcall(cp.cp, "scalProd", IloNumExpr, (Vector{IloNumVar}, Vector{IloNumVar}), coefs, vars)
end

function cpo_java_scalprod(cp::JavaCPOModel, coefs::Vector{IloNumVar}, vars::Vector{IloNumVar}, start::Int32, num::Int32)
    return jcall(cp.cp, "scalProd", IloNumExpr, (Vector{IloNumVar}, Vector{IloNumVar}, jint, jint), coefs, vars, start, num)
end

function cpo_java_scalprod(cp::JavaCPOModel, coefs::NumVarArray, vars::Vector{Int32})
    return jcall(cp.cp, "scalProd", IloLinearNumExpr, (Vector{IloNumVar}, Vector{jint}), coefs, vars)
end

function cpo_java_scalprod(cp::JavaCPOModel, coefs::Vector{Int32}, vars::Vector{IloIntVar})
    return jcall(cp.cp, "scalProd", IloLinearIntExpr, (Vector{jint}, Vector{IloIntVar}), coefs, vars)
end

function cpo_java_scalprod(cp::JavaCPOModel, coefs::Vector{Int32}, vars::Vector{IloIntVar}, start::Int32, num::Int32)
    return jcall(cp.cp, "scalProd", IloLinearIntExpr, (Vector{jint}, Vector{IloIntVar}, jint, jint), coefs, vars, start, num)
end

function cpo_java_scalprod(cp::JavaCPOModel, vars::Vector{Int32}, coefs::NumVarArray)
    return jcall(cp.cp, "scalProd", IloLinearNumExpr, (Vector{IloNumVar}, Vector{jint}), coefs, vars)
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

function cpo_java_sizeofnext(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, lastval::Int32)
    return jcall(cp.cp, "sizeOfNext", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint), var_seq, var_interval, lastval)
end

function cpo_java_sizeofnext(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, lastval::Int32, absval::Real)
    return jcall(cp.cp, "sizeOfNext", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint, jdouble), var_seq, var_interval, lastval, absval)
end

function cpo_java_sizeofprevious(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, firstval::Int32)
    return jcall(cp.cp, "sizeOfPrevious", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint), var_seq, var_interval, firstval)
end

function cpo_java_sizeofprevious(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, firstval::Int32, absval::Real)
    return jcall(cp.cp, "sizeOfPrevious", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint, jdouble), var_seq, var_interval, firstval, absval)
end

function cpo_java_square(cp::JavaCPOModel, e::IloIntExpr)
    return jcall(cp.cp, "square", IloIntExpr, (IloIntExpr,), e)
end

function cpo_java_square(cp::JavaCPOModel, e::IloNumExpr)
    return jcall(cp.cp, "square", IloNumExpr, (IloNumExpr,), e)
end

function cpo_java_standarddeviation(cp::JavaCPOModel, exprs::IntExprArray)
    return jcall(cp.cp, "standardDeviation", IloNumExpr, (Vector{IloIntExpr},), exprs)
end

function cpo_java_standarddeviation(cp::JavaCPOModel, exprs::IntExprArray, mean_lb::Real, mean_ub::Real)
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

function cpo_java_startofnext(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, lastval::Int32)
    return jcall(cp.cp, "startOfNext", IloNumExpr, (IloIntervalSequenceVar, IloIntervalVar, jint), var_seq, var_interval, lastval)
end

function cpo_java_startofnext(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, lastval::Int32, absval::Real)
    return jcall(cp.cp, "startOfNext", IloNumExpr, (IloIntervalSequenceVar, IloIntervalVar, jint, jdouble), var_seq, var_interval, lastval, absval)
end

function cpo_java_startofprevious(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, firstval::Int32)
    return jcall(cp.cp, "startOfPrevious", IloNumExpr, (IloIntervalSequenceVar, IloIntervalVar, jint), var_seq, var_interval, firstval)
end

function cpo_java_startofprevious(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, firstval::Int32, absval::Real)
    return jcall(cp.cp, "startOfPrevious", IloNumExpr, (IloIntervalSequenceVar, IloIntervalVar, jint, jdouble), var_seq, var_interval, firstval, absval)
end

function cpo_java_sum(cp::JavaCPOModel, v::Int32, expr::IntExpr)
    return jcall(cp.cp, "sum", IloIntExpr, (jint, IloIntExpr), v, expr)
end

function cpo_java_sum(cp::JavaCPOModel, exprs::IntExprArray)
    return jcall(cp.cp, "sum", IloIntExpr, (Vector{IloIntExpr},), exprs)
end

function cpo_java_sum(cp::JavaCPOModel, expr::IntExpr, v::Int32)
    return jcall(cp.cp, "sum", IloIntExpr, (IloIntExpr, jint), expr, v)
end

function cpo_java_sum(cp::JavaCPOModel, v::Real, expr::NumExpr)
    return jcall(cp.cp, "sum", IloNumExpr, (jdouble, IloNumExpr), v, expr)
end

function cpo_java_sum(cp::JavaCPOModel, v::NumExpr, expr::NumExpr)
    return jcall(cp.cp, "sum", IloNumExpr, (IloNumExpr, IloNumExpr), v, expr)
end

function cpo_java_sum(cp::JavaCPOModel, exprs::NumExprArray)
    return jcall(cp.cp, "sum", IloNumExpr, (Vector{IloNumExpr},), exprs)
end

function cpo_java_sum(cp::JavaCPOModel, expr::NumExpr, v::Real)
    return jcall(cp.cp, "sum", IloNumExpr, (IloNumExpr, jdouble), expr, v)
end

function cpo_java_typeofnext(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, lastval::Int32)
    return jcall(cp.cp, "typeOfNext", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint), var_seq, var_interval, lastval)
end

function cpo_java_typeofnext(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, lastval::Int32, absval::Int32)
    return jcall(cp.cp, "typeOfNext", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint, jint), var_seq, var_interval, lastval, absval)
end

function cpo_java_typeofprevious(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, firstval::Int32)
    return jcall(cp.cp, "typeOfPrevious", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint), var_seq, var_interval, firstval)
end

function cpo_java_typeofprevious(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar, firstval::Int32, absval::Int32)
    return jcall(cp.cp, "typeOfPrevious", IloIntExpr, (IloIntervalSequenceVar, IloIntervalVar, jint, jint), var_seq, var_interval, firstval, absval)
end

## IloLinearIntExpr: functions

function cpo_java_linearintexpr(cp::JavaCPOModel)
    return jcall(cp.cp, "linearIntExpr", IloLinearIntExpr, ())
end

function cpo_java_linearintexpr(cp::JavaCPOModel, val::Int32)
    return jcall(cp.cp, "linearIntExpr", IloLinearIntExpr, (jint,), val)
end

function cpo_java_linearintexpr_add(cp::JavaCPOModel, lis::IloLinearIntExpr, sc::IloLinearIntExpr)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(lis, "add", Nothing, (IloLinearIntExpr,), sc)
end

function cpo_java_linearintexpr_addterm(cp::JavaCPOModel, lis::IloLinearIntExpr, var::IloIntVar, coef::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(lis, "addTerm", Nothing, (IloIntVar, jint), var, coef)
end

function cpo_java_linearintexpr_addterm(cp::JavaCPOModel, lis::IloLinearIntExpr, coef::Int32, var::IloIntVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(lis, "addTerm", Nothing, (jint, IloIntVar), coef, var)
end

function cpo_java_linearintexpr_addterms(cp::JavaCPOModel, lis::IloLinearIntExpr, var::Vector{IloIntVar}, coef::Vector{Int32})
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(lis, "addTerms", Nothing, (Vector{IloIntVar}, Vector{jint}), var, coef)
end

function cpo_java_linearintexpr_addterms(cp::JavaCPOModel, lis::IloLinearIntExpr, var::Vector{IloIntVar}, coef::Vector{Int32}, start::Int32, num::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(lis, "addTerms", Nothing, (Vector{IloIntVar}, Vector{jint}, jint, jint), var, coef, start, num)
end

function cpo_java_linearintexpr_addterms(cp::JavaCPOModel, lis::IloLinearIntExpr, coef::Vector{Int32}, var::Vector{IloIntVar})
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(lis, "addTerms", Nothing, (Vector{jint}, Vector{IloIntVar}), coef, var)
end

function cpo_java_linearintexpr_addterms(cp::JavaCPOModel, lis::IloLinearIntExpr, coef::Vector{Int32}, var::Vector{IloIntVar}, start::Int32, num::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(lis, "addTerms", Nothing, (Vector{jint}, Vector{IloIntVar}, jint, jint), coef, var, start, num)
end

function cpo_java_linearintexpr_clear(cp::JavaCPOModel, lis::IloLinearIntExpr)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(lis, "clear", Nothing, ())
end

function cpo_java_linearintexpr_getconstant(cp::JavaCPOModel, lis::IloLinearIntExpr)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(lis, "getConstant", jint, ())
end

# TODO: linearIterator?

function cpo_java_linearintexpr_remove(cp::JavaCPOModel, lis::IloLinearIntExpr, var::IloIntVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(lis, "remove", Nothing, (IloIntVar,), var)
end

function cpo_java_linearintexpr_remove(cp::JavaCPOModel, lis::IloLinearIntExpr, var::Vector{IloIntVar})
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(lis, "remove", Nothing, (Vector{IloIntVar},), var)
end

function cpo_java_linearintexpr_remove(cp::JavaCPOModel, lis::IloLinearIntExpr, var::Vector{IloIntVar}, start::Int32, num::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(lis, "remove", Nothing, (Vector{IloIntVar}, jint, jint), var, start, num)
end

function cpo_java_linearintexpr_setconstant(cp::JavaCPOModel, lis::IloLinearIntExpr, val::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(lis, "setConstant", Nothing, (jint,), val)
end

## IloLinearNumExpr: functions

function cpo_java_linearnumexpr(cp::JavaCPOModel)
    return jcall(cp.cp, "linearNumExpr", IloLinearNumExpr, ())
end

function cpo_java_linearnumexpr(cp::JavaCPOModel, val::Real)
    return jcall(cp.cp, "linearNumExpr", IloLinearNumExpr, (jdouble,), val)
end

function cpo_java_linearnumexpr_add(cp::JavaCPOModel, lis::IloLinearNumExpr, sc::IloLinearNumExpr)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(lis, "add", Nothing, (IloLinearNumExpr,), sc)
end

function cpo_java_linearnumexpr_addterm(cp::JavaCPOModel, lis::IloLinearNumExpr, var::IloNumVar, coef::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(lis, "addTerm", Nothing, (IloNumVar, jdouble), var, coef)
end

function cpo_java_linearnumexpr_addterm(cp::JavaCPOModel, lis::IloLinearNumExpr, coef::Int32, var::IloNumVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(lis, "addTerm", Nothing, (jdouble, IloNumVar), coef, var)
end

function cpo_java_linearnumexpr_addterms(cp::JavaCPOModel, lis::IloLinearNumExpr, var::Vector{IloNumVar}, coef::Vector{T}) where {T <: Real}
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(lis, "addTerms", Nothing, (Vector{IloNumVar}, Vector{jdouble}), var, coef)
end

function cpo_java_linearnumexpr_addterms(cp::JavaCPOModel, lis::IloLinearNumExpr, var::Vector{IloNumVar}, coef::Vector{T}, start::Int32, num::Int32) where {T <: Real}
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(lis, "addTerms", Nothing, (Vector{IloNumVar}, Vector{jdouble}, jint, jint), var, coef, start, num)
end

function cpo_java_linearnumexpr_addterms(cp::JavaCPOModel, lis::IloLinearNumExpr, coef::Vector{T}, var::Vector{IloNumVar}) where {T <: Real}
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(lis, "addTerms", Nothing, (Vector{jdouble}, Vector{IloNumVar}), coef, var)
end

function cpo_java_linearnumexpr_addterms(cp::JavaCPOModel, lis::IloLinearNumExpr, coef::Vector{T}, var::Vector{IloNumVar}, start::Int32, num::Int32) where {T <: Real}
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(lis, "addTerms", Nothing, (Vector{jdouble}, Vector{IloNumVar}, jint, jint), coef, var, start, num)
end

function cpo_java_linearnumexpr_clear(cp::JavaCPOModel, lis::IloLinearNumExpr)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(lis, "clear", Nothing, ())
end

function cpo_java_linearnumexpr_getconstant(cp::JavaCPOModel, lis::IloLinearNumExpr)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(lis, "getConstant", jdouble, ())
end

# TODO: linearIterator?

function cpo_java_linearnumexpr_remove(cp::JavaCPOModel, lis::IloLinearNumExpr, var::IloNumVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(lis, "remove", Nothing, (IloNumVar,), var)
end

function cpo_java_linearnumexpr_remove(cp::JavaCPOModel, lis::IloLinearNumExpr, var::Vector{IloNumVar})
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(lis, "remove", Nothing, (Vector{IloNumVar},), var)
end

function cpo_java_linearnumexpr_remove(cp::JavaCPOModel, lis::IloLinearNumExpr, var::Vector{IloNumVar}, start::Int32, num::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(lis, "remove", Nothing, (Vector{IloNumVar}, jint, jint), var, start, num)
end

function cpo_java_linearnumexpr_setconstant(cp::JavaCPOModel, lis::IloLinearNumExpr, val::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(lis, "setConstant", Nothing, (jdouble,), val)
end

## IloIntTupleSet: functions

function cpo_java_inttable(cp::JavaCPOModel, dimension::Int32)
    return jcall(cp.cp, "intTable", IloIntTupleSet, (jint,), dimension)
end

function cpo_java_inttupleset_addtuple(cp::JavaCPOModel, its::IloIntTupleSet, tuple::Vector{Int32})
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(cp.cp, "addTuple", Nothing, (IloIntTupleSet, Vector{jint}), its, tuple)
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

function cpo_java_diff(cp::JavaCPOModel, e1::Int32, e2::IntExpr)
    return jcall(cp.cp, "diff", IloIntExpr, (jint, IloIntExpr), f1, f2)
end

function cpo_java_diff(cp::JavaCPOModel, e1::IntExpr, e2::Int32)
    return jcall(cp.cp, "diff", IloIntExpr, (IloIntExpr, jint), e1, e2)
end

function cpo_java_diff(cp::JavaCPOModel, e1::IntExpr, e2::IntExpr)
    return jcall(cp.cp, "diff", IloIntExpr, (IloIntExpr, IloIntExpr), e1, e2)
end

function cpo_java_diff(cp::JavaCPOModel, e1::Real, e2::NumExpr)
    return jcall(cp.cp, "diff", IloNumExpr, (jdouble, IloNumExpr), e1, e2)
end

function cpo_java_diff(cp::JavaCPOModel, e1::NumExpr, e2::Real)
    return jcall(cp.cp, "diff", IloNumExpr, (IloNumExpr, jdouble), e1, e2)
end

function cpo_java_diff(cp::JavaCPOModel, e1::NumExpr, e2::NumExpr)
    return jcall(cp.cp, "diff", IloNumExpr, (IloNumExpr, IloNumExpr), e1, e2)
end

function cpo_java_diff(cp::JavaCPOModel, f1::IloCumulFunctionExpr, f2::IloCumulFunctionExpr)
    return jcall(cp.cp, "diff", IloCumulFunctionExpr, (IloCumulFunctionExpr, IloCumulFunctionExpr), f1, f2)
end

function cpo_java_getnumberofsegments(cp::JavaCPOModel, f::IloCumulFunctionExpr)
    return jcall(cp.cp, "getNumberOfSegments", jint, (IloCumulFunctionExpr,), f)
end

function cpo_java_getsegmentstart(cp::JavaCPOModel, f::IloCumulFunctionExpr, i::Int32)
    return jcall(cp.cp, "getSegmentStart", jint, (IloCumulFunctionExpr, jint), f, i)
end

function cpo_java_getsegmentend(cp::JavaCPOModel, f::IloCumulFunctionExpr, i::Int32)
    return jcall(cp.cp, "getSegmentEnd", jint, (IloCumulFunctionExpr, jint), f, i)
end

function cpo_java_getsegmentvalue(cp::JavaCPOModel, f::IloCumulFunctionExpr, i::Int32)
    return jcall(cp.cp, "getSegmentValue", jint, (IloCumulFunctionExpr, jint), f, i)
end

function cpo_java_getvalue(cp::JavaCPOModel, f::IloCumulFunctionExpr, i::Int32)
    return jcall(cp.cp, "getSegmentValue", jint, (IloCumulFunctionExpr, jint), f, i)
end

function cpo_java_step(cp::JavaCPOModel, t::Int32, v::Int32)
    return jcall(cp.cp, "step", IloCumulFunctionExpr, (jint, jint), t, v)
end

function cpo_java_stepatend(cp::JavaCPOModel, a::IloIntervalVar, v::Int32)
    return jcall(cp.cp, "stepAtEnd", IloCumulFunctionExpr, (IloIntervalVar, jint), a, v)
end

function cpo_java_stepatend(cp::JavaCPOModel, a::IloIntervalVar, vmin::Int32, vmax::Int32)
    return jcall(cp.cp, "stepAtEnd", IloCumulFunctionExpr, (IloIntervalVar, jint, jint), a, vmin, vmax)
end

function cpo_java_stepatstart(cp::JavaCPOModel, a::IloIntervalVar, v::Int32)
    return jcall(cp.cp, "stepAtStart", IloCumulFunctionExpr, (IloIntervalVar, jint), a, v)
end

function cpo_java_stepatstart(cp::JavaCPOModel, a::IloIntervalVar, vmin::Int32, vmax::Int32)
    return jcall(cp.cp, "stepAtStart", IloCumulFunctionExpr, (IloIntervalVar, jint, jint), a, vmin, vmax)
end

function cpo_java_sum(cp::JavaCPOModel, f1::IloIntervalVar, f2::IloIntervalVar)
    return jcall(cp.cp, "sum", IloCumulFunctionExpr, (IloCumulFunctionExpr, cumulfunctionexpr), f1, f2)
end

## IloTransitionDistance: functions

function cpo_java_transitiondistance(cp::JavaCPOModel, i::Int32, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "transitionDistance", IloTransitionDistance, (jint,), i)
    else
        return jcall(cp.cp, "transitionDistance", IloTransitionDistance, (jint, JString), i, name)
    end
end

function cpo_java_transitiondistance(cp::JavaCPOModel, dtable::Matrix{Int32}, name::String="")
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

function cpo_java_transitiondistance_getvalue(cp::JavaCPOModel, td::IloTransitionDistance, fromstate::Int32, tostate::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(td, "getValue", jint, (jint, jint), fromstate, tostate)
end

function cpo_java_transitiondistance_setvalue(cp::JavaCPOModel, td::IloTransitionDistance, fromstate::Int32, tostate::Int32, value::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(td, "setValue", Nothing, (jint, jint, jint), fromstate, tostate, value)
end

## Constraint creation

function cpo_java_add(cp::JavaCPOModel, addable::Addable)
    return jcall(cp.cp, "add", IloAddable, (IloAddable,), addable)
end

function cpo_java_alldiff(cp::JavaCPOModel, exprs::IntExprArray)
    return jcall(cp.cp, "allDiff", IloConstraint, (Vector{IloIntExpr},), exprs)
end

function cpo_java_allmindistance(cp::JavaCPOModel, vars::Vector{IloIntExpr}, k::Int32)
    return jcall(cp.cp, "allMinDistance", IloConstraint, (Vector{IloIntExpr}, jint), vars, k)
end

function cpo_java_allowedassignments(cp::JavaCPOModel, expr::IntExpr, values::Vector{Int32})
    return jcall(cp.cp, "allowedAssignments", IloConstraint, (IloIntExpr, Vector{jint}), expr, values)
end

function cpo_java_allowedassignments(cp::JavaCPOModel, vars::Vector{IloIntVar}, values::IloIntTupleSet)
    return jcall(cp.cp, "allowedAssignments", IloConstraint, (Vector{IloIntVar}, IloIntTupleSet), vars, values)
end

function cpo_java_alternative(cp::JavaCPOModel, interval_a::IloIntervalVar, intervals_b::Vector{IloIntervalVar}, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "alternative", IloAlternative, (IloIntervalVar, Vector{IloIntervalVar}), interval_a, intervals_b)
    else
        return jcall(cp.cp, "alternative", IloAlternative, (IloIntervalVar, Vector{IloIntervalVar}, JString), interval_a, intervals_b, name)
    end
end

function cpo_java_alternative(cp::JavaCPOModel, interval_a::IloIntervalVar, intervals_b::Vector{IloIntervalVar}, value::Int32, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "alternative", IloAlternative, (IloIntervalVar, Vector{IloIntervalVar}, jint), interval_a, intervals_b, value)
    else
        return jcall(cp.cp, "alternative", IloAlternative, (IloIntervalVar, Vector{IloIntervalVar}, jint, JString), interval_a, intervals_b, value, name)
    end
end

function cpo_java_alternative(cp::JavaCPOModel, interval_a::IloIntervalVar, intervals_b::Vector{IloIntervalVar}, expr::IntExpr, name::String="")
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

function cpo_java_alwaysconstant(cp::JavaCPOModel, f::IloStateFunction, start::Int32, end_::Int32, startalign::Bool, endalign::Bool)
    return jcall(cp.cp, "alwaysConstant", IloConstraint, (IloStateFunction, jint, jint), f, start, end_)
end

function cpo_java_alwaysconstant(cp::JavaCPOModel, f::IloStateFunction, start::Int32, end_::Int32)
    return jcall(cp.cp, "alwaysConstant", IloConstraint, (IloStateFunction, jint, jint, jboolean, jboolean), f, start, end_, startalign, endalign)
end

function cpo_java_alwaysequal(cp::JavaCPOModel, f::IloStateFunction, a::IloIntervalVar, v::Int32, startalign::Bool, endalign::Bool)
    return jcall(cp.cp, "alwaysEqual", IloConstraint, (IloStateFunction, IloIntervalVar, jint, jboolean, jboolean), f, a, v, startalign, endalign)
end

function cpo_java_alwaysequal(cp::JavaCPOModel, f::IloStateFunction, start::Int32, end_::Int32, v::Int32, startalign::Bool, endalign::Bool)
    return jcall(cp.cp, "alwaysEqual", IloConstraint, (IloStateFunction, jint, jint, jint), f, start, end_, v)
end

function cpo_java_alwaysequal(cp::JavaCPOModel, f::IloStateFunction, start::Int32, end_::Int32, v::Int32)
    return jcall(cp.cp, "alwaysEqual", IloConstraint, (IloStateFunction, jint, jint, jint, jboolean, jboolean), f, start, end_, v, startalign, endalign)
end

function cpo_java_alwaysin(cp::JavaCPOModel, f::IloCumulFunctionExpr, a::IloIntervalVar, vmin::Int32, vmax::Int32)
    return jcall(cp.cp, "alwaysIn", IloConstraint, (IloCumulFunctionExpr, IloIntervalVar, jint, jint), f, a, vmin, vmax)
end

function cpo_java_alwaysin(cp::JavaCPOModel, f::IloCumulFunctionExpr, start::Int32, end_::Int32, vmin::Int32, vmax::Int32)
    return jcall(cp.cp, "alwaysIn", IloConstraint, (IloCumulFunctionExpr, jint, jint, jint, jint), f, start, end_, vmin, vmax)
end

function cpo_java_alwaysin(cp::JavaCPOModel, f::IloStateFunction, a::IloIntervalVar, vmin::Int32, vmax::Int32)
    return jcall(cp.cp, "alwaysIn", IloConstraint, (IloStateFunction, IloIntervalVar, jint, jint), f, a, vmin, vmax)
end

function cpo_java_alwaysin(cp::JavaCPOModel, f::IloStateFunction, start::Int32, end_::Int32, vmin::Int32, vmax::Int32)
    return jcall(cp.cp, "alwaysIn", IloConstraint, (IloStateFunction, jint, jint, jint, jint), f, start, end_, vmin, vmax)
end

function cpo_java_alwaysnostate(cp::JavaCPOModel, f::IloStateFunction, a::IloIntervalVar)
    return jcall(cp.cp, "alwaysNoState", IloConstraint, (IloStateFunction, IloIntervalVar, jint), f, a)
end

function cpo_java_alwaysnostate(cp::JavaCPOModel, f::IloStateFunction, start::Int32, end_::Int32)
    return jcall(cp.cp, "alwaysNoState", IloConstraint, (IloStateFunction, jint, jint), f, start, end_)
end

function cpo_java_and(cp::JavaCPOModel)
    return jcall(cp.cp, "and", IloAnd, ())
end

function cpo_java_and(cp::JavaCPOModel, constrs::ConstraintArray, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "and", IloAnd, (Vector{IloConstraint},), constrs)
    else
        return jcall(cp.cp, "and", IloAnd, (Vector{IloConstraint}, JString), constrs, name)
    end
end

function cpo_java_and(cp::JavaCPOModel, constrs::ConstraintArray, start::Int32, num::Int32, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "and", IloAnd, (Vector{IloConstraint}, jint, jint), constrs, start, num)
    else
        return jcall(cp.cp, "and", IloAnd, (Vector{IloConstraint}, jint, jint, JString), constrs, start, num, name)
    end
end

function cpo_java_and(cp::JavaCPOModel, constr_a::Constraint, constr_b::Constraint, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "and", IloAnd, (IloConstraint, IloConstraint, jint, jint), constr_a, constr_b)
    else
        return jcall(cp.cp, "and", IloAnd, (IloConstraint, IloConstraint, jint, jint, JString), constr_a, constr_b, name)
    end
end

function cpo_java_before(cp::JavaCPOModel, seq::IloIntervalSequenceVar, pred::IloIntervalVar, succ::IloIntervalVar)
    return jcall(cp.cp, "before", IloConstraint, (IloIntervalSequenceVar, IloIntervalVar, IloIntervalVar), seq, pred, succ)
end

function cpo_java_distribute(cp::JavaCPOModel, exprs_cards::IntExprArray, exprs_vars::IntExprArray)
    return jcall(cp.cp, "distribute", IloConstraint, (Vector{IloIntExpr}, Vector{IloIntExpr}), exprs_cards, exprs_vars)
end

function cpo_java_distribute(cp::JavaCPOModel, exprs_cards::IntExprArray, values::Vector{Int32}, exprs_vars::IntExprArray)
    return jcall(cp.cp, "distribute", IloConstraint, (Vector{IloIntExpr}, Vector{jint}, Vector{IloIntExpr}), exprs_cards, values, exprs_vars)
end

function cpo_java_element(cp::JavaCPOModel, expr_var::IntExpr, expr_index::IntExpr, values::Vector{Int32})
    return jcall(cp.cp, "element", IloConstraint, (IloIntExpr, IloIntExpr, Vector{jint}), expr_var, expr_index, values)
end

function cpo_java_element(cp::JavaCPOModel, expr_var::NumExpr, expr_index::IntExpr, values::Vector{T}) where {T <: Real}
    return jcall(cp.cp, "element", IloConstraint, (IloNumExpr, IloIntExpr, Vector{jdouble}), expr_var, expr_index, values)
end

function cpo_java_endatend(cp::JavaCPOModel, expr_a::IloIntervalVar, expr_b::IloIntervalVar)
    return jcall(cp.cp, "endAtEnd", IloConstraint, (IloIntervalVar, IloIntervalVar), expr_a, expr_b)
end

function cpo_java_endatend(cp::JavaCPOModel, expr_a::IloIntervalVar, expr_b::IloIntervalVar, expr_z::Int32)
    return jcall(cp.cp, "endAtEnd", IloConstraint, (IloIntervalVar, IloIntervalVar, jint), expr_a, expr_b, expr_z)
end

function cpo_java_endatend(cp::JavaCPOModel, expr_a::IloIntervalVar, expr_b::IloIntervalVar, expr_z)
    return jcall(cp.cp, "endAtEnd", IloConstraint, (IloIntervalVar, IloIntervalVar, IloIntExpr), expr_a, expr_b, expr_z)
end

function cpo_java_endatstart(cp::JavaCPOModel, expr_a::IloIntervalVar, expr_b::IloIntervalVar)
    return jcall(cp.cp, "endAtStart", IloConstraint, (IloIntervalVar, IloIntervalVar), expr_a, expr_b)
end

function cpo_java_endatstart(cp::JavaCPOModel, expr_a::IloIntervalVar, expr_b::IloIntervalVar, expr_z::Int32)
    return jcall(cp.cp, "endAtStart", IloConstraint, (IloIntervalVar, IloIntervalVar, jint), expr_a, expr_b, expr_z)
end

function cpo_java_endatstart(cp::JavaCPOModel, expr_a::IloIntervalVar, expr_b::IloIntervalVar, expr_z)
    return jcall(cp.cp, "endAtStart", IloConstraint, (IloIntervalVar, IloIntervalVar, IloIntExpr), expr_a, expr_b, expr_z)
end

function cpo_java_endbeforeend(cp::JavaCPOModel, expr_a::IloIntervalVar, expr_b::IloIntervalVar)
    return jcall(cp.cp, "endBeforeEnd", IloConstraint, (IloIntervalVar, IloIntervalVar), expr_a, expr_b)
end

function cpo_java_endbeforeend(cp::JavaCPOModel, expr_a::IloIntervalVar, expr_b::IloIntervalVar, expr_z::Int32)
    return jcall(cp.cp, "endBeforeEnd", IloConstraint, (IloIntervalVar, IloIntervalVar, jint), expr_a, expr_b, expr_z)
end

function cpo_java_endbeforeend(cp::JavaCPOModel, expr_a::IloIntervalVar, expr_b::IloIntervalVar, expr_z)
    return jcall(cp.cp, "endBeforeEnd", IloConstraint, (IloIntervalVar, IloIntervalVar, IloIntExpr), expr_a, expr_b, expr_z)
end

function cpo_java_endbeforestart(cp::JavaCPOModel, expr_a::IloIntervalVar, expr_b::IloIntervalVar)
    return jcall(cp.cp, "endBeforeStart", IloConstraint, (IloIntervalVar, IloIntervalVar), expr_a, expr_b)
end

function cpo_java_endbeforestart(cp::JavaCPOModel, expr_a::IloIntervalVar, expr_b::IloIntervalVar, expr_z::Int32)
    return jcall(cp.cp, "endBeforeStart", IloConstraint, (IloIntervalVar, IloIntervalVar, jint), expr_a, expr_b, expr_z)
end

function cpo_java_endbeforestart(cp::JavaCPOModel, expr_a::IloIntervalVar, expr_b::IloIntervalVar, expr_z)
    return jcall(cp.cp, "endBeforeStart", IloConstraint, (IloIntervalVar, IloIntervalVar, IloIntExpr), expr_a, expr_b, expr_z)
end

function cpo_java_eq(cp::JavaCPOModel, expr_a::Real, expr_b::NumExpr, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "eq", IloRange, (jdouble, IloNumExpr), expr_a, expr_b)
    else
        return jcall(cp.cp, "eq", IloRange, (jdouble, IloNumExpr, JString), expr_a, expr_b, name)
    end
end

function cpo_java_eq(cp::JavaCPOModel, expr_a::NumExpr, expr_b::Real, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "eq", IloRange, (IloNumExpr, jdouble), expr_a, expr_b)
    else
        return jcall(cp.cp, "eq", IloRange, (IloNumExpr, jdouble, JString), expr_a, expr_b, name)
    end
end

function cpo_java_eq(cp::JavaCPOModel, expr_a::NumExpr, expr_b::NumExpr, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "eq", IloConstraint, (IloNumExpr, IloNumExpr), expr_a, expr_b)
    else
        return jcall(cp.cp, "eq", IloConstraint, (IloNumExpr, IloNumExpr, JString), expr_a, expr_b, name)
    end
end

function cpo_java_eq(cp::JavaCPOModel, expr_a::IntExpr, expr_b::IntExpr)
    return jcall(cp.cp, "eq", IloConstraint, (IloIntExpr, IloIntExpr), expr_a, expr_b)
end

function cpo_java_eq(cp::JavaCPOModel, expr_a::Int32, expr_b::IntExpr)
    return jcall(cp.cp, "eq", IloConstraint, (jint, IloIntExpr), expr_a, expr_b)
end

function cpo_java_eq(cp::JavaCPOModel, expr_a::IntExpr, expr_b::Int32)
    return jcall(cp.cp, "eq", IloConstraint, (IloIntExpr, jint), expr_a, expr_b)
end

function cpo_java_equiv(cp::JavaCPOModel, constr_a::Constraint, constr_b::Constraint)
    return jcall(cp.cp, "equiv", IloConstraint, (IloConstraint, IloConstraint), constr_a, constr_b)
end

function cpo_java_falseconstraint(cp::JavaCPOModel)
    return jcall(cp.cp, "falseConstraint", IloConstraint, ())
end

function cpo_java_first(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar)
    return jcall(cp.cp, "first", IloConstraint, (IloIntervalSequenceVar, IloIntervalVar), var_seq, var_interval)
end

function cpo_java_forbiddenassignments(cp::JavaCPOModel, expr::IntExpr, values::Vector{Int32})
    return jcall(cp.cp, "forbiddenAssignments", IloConstraint, (IloIntExpr, Vector{jint}), expr, values)
end

function cpo_java_forbiddenassignments(cp::JavaCPOModel, exprs::Vector{IntExpr}, values::Vector{IloIntTupleSet})
    return jcall(cp.cp, "forbiddenAssignments", IloConstraint, (Vector{IloIntExpr}, Vector{IloIntTupleSet}), exprs, values)
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

function cpo_java_ge(cp::JavaCPOModel, f::IloCumulFunctionExpr, vmin::IntExpr)
    return jcall(cp.cp, "ge", IloConstraint, (IloCumulFunctionExpr, IloIntExpr), f, vmin)
end

function cpo_java_ge(cp::JavaCPOModel, f::IloCumulFunctionExpr, vmin::Int32)
    return jcall(cp.cp, "ge", IloConstraint, (IloCumulFunctionExpr, jint), f, vmin)
end

function cpo_java_ge(cp::JavaCPOModel, vmin::IntExpr, f::IloCumulFunctionExpr)
    return jcall(cp.cp, "ge", IloConstraint, (IloIntExpr, IloCumulFunctionExpr), vmin, f)
end

function cpo_java_ge(cp::JavaCPOModel, vmin::Int32, f::IloCumulFunctionExpr)
    return jcall(cp.cp, "ge", IloConstraint, (jint, IloCumulFunctionExpr), vmin, f)
end

function cpo_java_ge(cp::JavaCPOModel, expr_a::Real, expr_b::NumExpr, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "ge", IloRange, (jdouble, IloNumExpr), expr_a, expr_b)
    else
        return jcall(cp.cp, "ge", IloRange, (jdouble, IloNumExpr, JString), expr_a, expr_b, name)
    end
end

function cpo_java_ge(cp::JavaCPOModel, expr_a::NumExpr, expr_b::Real, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "ge", IloRange, (IloNumExpr, jdouble), expr_a, expr_b)
    else
        return jcall(cp.cp, "ge", IloRange, (IloNumExpr, jdouble, JString), expr_a, expr_b, name)
    end
end

function cpo_java_ge(cp::JavaCPOModel, expr_a::NumExpr, expr_b::NumExpr, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "ge", IloConstraint, (IloNumExpr, IloNumExpr), expr_a, expr_b)
    else
        return jcall(cp.cp, "ge", IloConstraint, (IloNumExpr, IloNumExpr, JString), expr_a, expr_b, name)
    end
end

function cpo_java_ge(cp::JavaCPOModel, expr_a::IntExpr, expr_b::IntExpr)
    return jcall(cp.cp, "ge", IloConstraint, (IloIntExpr, IloIntExpr), expr_a, expr_b)
end

function cpo_java_ge(cp::JavaCPOModel, expr_a::Int32, expr_b::IntExpr)
    return jcall(cp.cp, "ge", IloConstraint, (jint, IloIntExpr), expr_a, expr_b)
end

function cpo_java_ge(cp::JavaCPOModel, expr_a::IntExpr, expr_b::Int32)
    return jcall(cp.cp, "ge", IloConstraint, (IloIntExpr, jint), expr_a, expr_b)
end

function cpo_java_gt(cp::JavaCPOModel, expr_a::IntExpr, expr_b::IntExpr)
    return jcall(cp.cp, "gt", IloConstraint, (IloIntExpr, IloIntExpr), expr_a, expr_b)
end

function cpo_java_gt(cp::JavaCPOModel, expr_a::Int32, expr_b::IntExpr)
    return jcall(cp.cp, "gt", IloConstraint, (jint, IloIntExpr), expr_a, expr_b)
end

function cpo_java_gt(cp::JavaCPOModel, expr_a::IntExpr, expr_b::Int32)
    return jcall(cp.cp, "gt", IloConstraint, (IloIntExpr, jint), expr_a, expr_b)
end

function cpo_java_ifthen(cp::JavaCPOModel, constr_a::Constraint, constr_b::Constraint, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "ifThen", IloConstraint, (IloConstraint, IloConstraint), constr_a, constr_b)
    else
        return jcall(cp.cp, "ifThen", IloConstraint, (IloConstraint, IloConstraint, JString), constr_a, constr_b, name)
    end
end

function cpo_java_ifthenelse(cp::JavaCPOModel, constr_a::Constraint, constr_b::Constraint, constr_c::Constraint)
    return jcall(cp.cp, "ifThenElse", IloConstraint, (IloConstraint, IloConstraint, IloConstraint), constr_a, constr_b, constr_c)
end

function cpo_java_imply(cp::JavaCPOModel, constr_a::Constraint, constr_b::Constraint)
    return jcall(cp.cp, "imply", IloConstraint, (IloConstraint, IloConstraint), constr_a, constr_b)
end

function cpo_java_inverse(cp::JavaCPOModel, f::IntExprArray, invf::IntExprArray)
    return jcall(cp.cp, "inverse", IloConstraint, (Vector{IloIntExpr}, Vector{IloIntExpr}), f, invf)
end

function cpo_java_isomorphism(cp::JavaCPOModel, vars_a::Vector{IloIntervalVar}, vars_b::Vector{IloIntervalVar}, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "isomorphism", IloIsomorphism, (Vector{IloIntervalVar}, Vector{IloIntervalVar}), vars_a, vars_b)
    else
        return jcall(cp.cp, "isomorphism", IloIsomorphism, (Vector{IloIntervalVar}, Vector{IloIntervalVar}, JString), vars_a, vars_b, name)
    end
end

function cpo_java_isomorphism(cp::JavaCPOModel, vars_a::Vector{IloIntervalVar}, vars_b::Vector{IloIntervalVar}, map::IntExprArray, absval::Int32, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "isomorphism", IloIsomorphism, (Vector{IloIntervalVar}, Vector{IloIntervalVar}, Vector{IloIntExpr}, jint), vars_a, vars_b, map, absval)
    else
        return jcall(cp.cp, "isomorphism", IloIsomorphism, (Vector{IloIntervalVar}, Vector{IloIntervalVar}, Vector{IloIntExpr}, jint, JString), vars_a, vars_b, map, absval, name)
    end
end

function cpo_java_last(cp::JavaCPOModel, var_seq::IloIntervalSequenceVar, var_interval::IloIntervalVar)
    return jcall(cp.cp, "last", IloConstraint, (IloIntervalSequenceVar, IloIntervalVar), var_seq, var_interval)
end

function cpo_java_le(cp::JavaCPOModel, expr_a::Real, expr_b::NumExpr, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "le", IloRange, (jdouble, IloNumExpr), expr_a, expr_b)
    else
        return jcall(cp.cp, "le", IloRange, (jdouble, IloNumExpr, JString), expr_a, expr_b, name)
    end
end

function cpo_java_le(cp::JavaCPOModel, expr_a::NumExpr, expr_b::Real, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "le", IloRange, (IloNumExpr, jdouble), expr_a, expr_b)
    else
        return jcall(cp.cp, "le", IloRange, (IloNumExpr, jdouble, JString), expr_a, expr_b, name)
    end
end

function cpo_java_le(cp::JavaCPOModel, expr_a::NumExpr, expr_b::NumExpr, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "le", IloConstraint, (IloNumExpr, IloNumExpr), expr_a, expr_b)
    else
        return jcall(cp.cp, "le", IloConstraint, (IloNumExpr, IloNumExpr, JString), expr_a, expr_b, name)
    end
end

function cpo_java_le(cp::JavaCPOModel, expr_a::IntExpr, expr_b::IntExpr)
    return jcall(cp.cp, "le", IloConstraint, (IloIntExpr, IloIntExpr), expr_a, expr_b)
end

function cpo_java_le(cp::JavaCPOModel, expr_a::Int32, expr_b::IntExpr)
    return jcall(cp.cp, "le", IloConstraint, (jint, IloIntExpr), expr_a, expr_b)
end

function cpo_java_le(cp::JavaCPOModel, expr_a::IntExpr, expr_b::Int32)
    return jcall(cp.cp, "le", IloConstraint, (IloIntExpr, jint), expr_a, expr_b)
end

function cpo_java_lexicographic(cp::JavaCPOModel, exprs_a::IntExprArray, exprs_b::IntExprArray)
    return jcall(cp.cp, "lexicographic", IloConstraint, (Vector{IloIntExpr}, Vector{IloIntExpr}), exprs_a, exprs_b)
end

function cpo_java_strictlexicographic(cp::JavaCPOModel, exprs_a::IntExprArray, exprs_b::IntExprArray)
    return jcall(cp.cp, "strictLexicographic", IloConstraint, (Vector{IloIntExpr}, Vector{IloIntExpr}), exprs_a, exprs_b)
end

function cpo_java_lt(cp::JavaCPOModel, expr_a::IntExpr, expr_b::IntExpr)
    return jcall(cp.cp, "lt", IloConstraint, (IloIntExpr, IloIntExpr), expr_a, expr_b)
end

function cpo_java_lt(cp::JavaCPOModel, expr_a::Int32, expr_b::IntExpr)
    return jcall(cp.cp, "lt", IloConstraint, (jint, IloIntExpr), expr_a, expr_b)
end

function cpo_java_lt(cp::JavaCPOModel, expr_a::IntExpr, expr_b::Int32)
    return jcall(cp.cp, "lt", IloConstraint, (IloIntExpr, jint), expr_a, expr_b)
end

function cpo_java_neq(cp::JavaCPOModel, constr_a::Constraint, constr_b::Constraint)
    return jcall(cp.cp, "neq", IloConstraint, (IloConstraint, IloConstraint), constr_a, constr_b)
end

function cpo_java_neq(cp::JavaCPOModel, expr_a::IntExpr, expr_b::IntExpr)
    return jcall(cp.cp, "neq", IloConstraint, (IloIntExpr, IloIntExpr), expr_a, expr_b)
end

function cpo_java_neq(cp::JavaCPOModel, expr_a::Int32, expr_b::IntExpr)
    return jcall(cp.cp, "neq", IloConstraint, (jint, IloIntExpr), expr_a, expr_b)
end

function cpo_java_neq(cp::JavaCPOModel, expr_a::IntExpr, expr_b::Int32)
    return jcall(cp.cp, "neq", IloConstraint, (IloIntExpr, jint), expr_a, expr_b)
end

function cpo_java_not(cp::JavaCPOModel, constr::Constraint, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "not", IloConstraint, (IloConstraint), constr)
    else
        return jcall(cp.cp, "not", IloConstraint, (IloConstraint, JString), constr, name)
    end
end

function cpo_java_nooverlap(cp::JavaCPOModel, seq::IloIntervalSequenceVar)
    return jcall(cp.cp, "noOverlap", IloNoOverlap, (IloIntervalSequenceVar,), seq)
end

function cpo_java_nooverlap(cp::JavaCPOModel, seq::IloIntervalSequenceVar, tdist::IloTransitionDistance, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "noOverlap", IloNoOverlap, (IloIntervalSequenceVar, IloTransitionDistance), seq, tdist)
    else
        return jcall(cp.cp, "noOverlap", IloNoOverlap, (IloIntervalSequenceVar, IloTransitionDistance, JString), seq, tdist, name)
    end
end

function cpo_java_nooverlap(cp::JavaCPOModel, seq::IloIntervalSequenceVar, tdist::IloTransitionDistance, direct::Bool, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "noOverlap", IloNoOverlap, (IloIntervalSequenceVar, IloTransitionDistance, jboolean), seq, tdist, direct)
    else
        return jcall(cp.cp, "noOverlap", IloNoOverlap, (IloIntervalSequenceVar, IloTransitionDistance, jboolean, JString), seq, tdist, direct, name)
    end
end

function cpo_java_nooverlap(cp::JavaCPOModel, vars::Vector{IloIntervalVar}, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "noOverlap", IloNoOverlap, (Vector{IloIntervalVar},), vars)
    else
        return jcall(cp.cp, "noOverlap", IloNoOverlap, (Vector{IloIntervalVar}, JString), vars, name)
    end
end

function cpo_java_or(cp::JavaCPOModel)
    return jcall(cp.cp, "or", IloOr, ())
end

function cpo_java_or(cp::JavaCPOModel, constrs::ConstraintArray, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "or", IloOr, (Vector{IloConstraint},), constrs)
    else
        return jcall(cp.cp, "or", IloOr, (Vector{IloConstraint}, JString), constrs, name)
    end
end

function cpo_java_or(cp::JavaCPOModel, constrs::ConstraintArray, start::Int32, num::Int32, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "or", IloOr, (Vector{IloConstraint}, jint, jint), constrs, start, num)
    else
        return jcall(cp.cp, "or", IloOr, (Vector{IloConstraint}, jint, jint, JString), constrs, start, num, name)
    end
end

function cpo_java_or(cp::JavaCPOModel, constr_a::Constraint, constr_b::Constraint, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "or", IloOr, (IloConstraint, IloConstraint, jint, jint), constr_a, constr_b)
    else
        return jcall(cp.cp, "or", IloOr, (IloConstraint, IloConstraint, jint, jint, JString), constr_a, constr_b, name)
    end
end

function cpo_java_pack(cp::JavaCPOModel, expr_load::IntExprArray, expr_where::IntExprArray, weight::Vector{Int32})
    return jcall(cp.cp, "pack", IloConstraint, (Vector{IloIntExpr}, Vector{IloIntExpr}, Vector{jint}), expr_load, expr_where, weight)
end

function cpo_java_pack(cp::JavaCPOModel, expr_load::IntExprArray, expr_where::IntExprArray, weight::Vector{Int32}, used::IntExpr)
    return jcall(cp.cp, "pack", IloConstraint, (Vector{IloIntExpr}, Vector{IloIntExpr}, Vector{jint}, IloIntExpr), expr_load, expr_where, weight, used)
end

function cpo_java_presenceof(cp::JavaCPOModel, var::IloIntervalVar)
    return jcall(cp.cp, "presenceOf", IloConstraint, (IloIntervalVar,), var)
end

function cpo_java_previous(cp::JavaCPOModel, seq::IloIntervalSequenceVar, prev::IloIntervalVar, next::IloIntervalVar)
    return jcall(cp.cp, "previous", IloConstraint, (IloIntervalSequenceVar, IloIntervalVar, IloIntervalVar), seq, prev, next)
end

function cpo_java_range(cp::JavaCPOModel, expr::NumExpr, b::Real)
    return jcall(cp.cp, "range", IloConstraint, (IloNumExpr, jdouble), expr, b)
end

function cpo_java_range(cp::JavaCPOModel, lb::Real, expr::NumExpr, ub::Real, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "range", IloRange, (jdouble, IloNumExpr, jdouble), lb, expr, ub)
    else
        return jcall(cp.cp, "range", IloRange, (jdouble, IloNumExpr, jdouble, JString), lb, expr, ub, name)
    end
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

function cpo_java_sequence(cp::JavaCPOModel, nbmin::Int32, nbmax::Int32, seqwidth::Int32, vars::Vector{IloIntVar}, values::Vector{Int32}, card::Vector{IloIntVar})
    return jcall(cp.cp, "sequence", IloConstraint, (jint, jint, jint, Vector{IloIntVar}, Vector{jint}, Vector{IloIntVar}), nbmin, nbmax, seqwidth, vars, values, card)
end

function cpo_java_span(cp::JavaCPOModel, a::IloIntervalVar, bs::Vector{IloIntervalVar}, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "span", IloSpan, (IloIntervalVar, Vector{IloIntervalVar}), a, bs)
    else
        return jcall(cp.cp, "span", IloSpan, (IloIntervalVar, Vector{IloIntervalVar}, JString), a, bs, name)
    end
end

function cpo_java_startatend(cp::JavaCPOModel, a::IloIntervalVar, b::IloIntervalVar)
    return jcall(cp.cp, "startAtEnd", IloConstraint, (IloIntervalVar, IloIntervalVar), a, b)
end

function cpo_java_startatend(cp::JavaCPOModel, a::IloIntervalVar, b::IloIntervalVar, z::Int32)
    return jcall(cp.cp, "startAtEnd", IloConstraint, (IloIntervalVar, IloIntervalVar, jint), a, b, z)
end

function cpo_java_startatend(cp::JavaCPOModel, a::IloIntervalVar, b::IloIntervalVar, z)
    return jcall(cp.cp, "startAtEnd", IloConstraint, (IloIntervalVar, IloIntervalVar, IloIntExpr), a, b, z)
end

function cpo_java_startatstart(cp::JavaCPOModel, a::IloIntervalVar, b::IloIntervalVar)
    return jcall(cp.cp, "startAtStart", IloConstraint, (IloIntervalVar, IloIntervalVar), a, b)
end

function cpo_java_startatstart(cp::JavaCPOModel, a::IloIntervalVar, b::IloIntervalVar, z::Int32)
    return jcall(cp.cp, "startAtStart", IloConstraint, (IloIntervalVar, IloIntervalVar, jint), a, b, z)
end

function cpo_java_startatstart(cp::JavaCPOModel, a::IloIntervalVar, b::IloIntervalVar, z)
    return jcall(cp.cp, "startAtStart", IloConstraint, (IloIntervalVar, IloIntervalVar, IloIntExpr), a, b, z)
end

function cpo_java_startbeforeend(cp::JavaCPOModel, a::IloIntervalVar, b::IloIntervalVar)
    return jcall(cp.cp, "startBeforeEnd", IloConstraint, (IloIntervalVar, IloIntervalVar), a, b)
end

function cpo_java_startbeforeend(cp::JavaCPOModel, a::IloIntervalVar, b::IloIntervalVar, z::Int32)
    return jcall(cp.cp, "startBeforeEnd", IloConstraint, (IloIntervalVar, IloIntervalVar, jint), a, b, z)
end

function cpo_java_startbeforeend(cp::JavaCPOModel, a::IloIntervalVar, b::IloIntervalVar, z)
    return jcall(cp.cp, "startBeforeEnd", IloConstraint, (IloIntervalVar, IloIntervalVar, IloIntExpr), a, b, z)
end

function cpo_java_startbeforestart(cp::JavaCPOModel, a::IloIntervalVar, b::IloIntervalVar)
    return jcall(cp.cp, "startBeforeStart", IloConstraint, (IloIntervalVar, IloIntervalVar), a, b)
end

function cpo_java_startbeforestart(cp::JavaCPOModel, a::IloIntervalVar, b::IloIntervalVar, z::Int32)
    return jcall(cp.cp, "startBeforeStart", IloConstraint, (IloIntervalVar, IloIntervalVar, jint), a, b, z)
end

function cpo_java_startbeforestart(cp::JavaCPOModel, a::IloIntervalVar, b::IloIntervalVar, z)
    return jcall(cp.cp, "startBeforeStart", IloConstraint, (IloIntervalVar, IloIntervalVar, IloIntExpr), a, b, z)
end

function cpo_java_strong(cp::JavaCPOModel, vars::Vector{IloIntVar})
    return jcall(cp.cp, "strong", IloConstraint, (Vector{IloIntVar},), vars)
end

function cpo_java_subcircuit(cp::JavaCPOModel, vars::Vector{IloIntVar})
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

## IloAdd.

function cpo_java_and_add(cp::JavaCPOModel, and::IloAnd, constr::Constraint)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(and, "add", IloConstraint, (IloConstraint,), constr)
end

function cpo_java_and_add(cp::JavaCPOModel, and::IloAnd, constr::ConstraintArray)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(and, "add", IloConstraint, (Vector{IloConstraint},), constr)
end

function cpo_java_and_add(cp::JavaCPOModel, and::IloAnd, constr::ConstraintArray, start::Int32, num::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(and, "add", IloConstraint, (Vector{IloConstraint}, jint, jint), constr, start, num)
end

function cpo_java_and_remove(cp::JavaCPOModel, and::IloAnd, constr::Constraint)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(and, "remove", IloConstraint, (IloConstraint,), constr)
end

function cpo_java_and_remove(cp::JavaCPOModel, and::IloAnd, constr::ConstraintArray)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(and, "remove", IloConstraint, (Vector{IloConstraint},), constr)
end

function cpo_java_and_remove(cp::JavaCPOModel, and::IloAnd, constr::ConstraintArray, start::Int32, num::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(and, "remove", IloConstraint, (Vector{IloConstraint}, jint, jint), constr, start, num)
end

## IloOr.

function cpo_java_or_add(cp::JavaCPOModel, or::IloOr, constr::Constraint)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(or, "add", IloConstraint, (IloConstraint,), constr)
end

function cpo_java_or_add(cp::JavaCPOModel, or::IloOr, constr::ConstraintArray)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(or, "add", IloConstraint, (Vector{IloConstraint},), constr)
end

function cpo_java_or_add(cp::JavaCPOModel, or::IloOr, constr::ConstraintArray, start::Int32, num::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(or, "add", IloConstraint, (Vector{IloConstraint}, jint, jint), constr, start, num)
end

function cpo_java_or_remove(cp::JavaCPOModel, or::IloOr, constr::Constraint)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(or, "remove", IloConstraint, (IloConstraint,), constr)
end

function cpo_java_or_remove(cp::JavaCPOModel, or::IloOr, constr::ConstraintArray)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(or, "remove", IloConstraint, (Vector{IloConstraint},), constr)
end

function cpo_java_or_remove(cp::JavaCPOModel, or::IloOr, constr::ConstraintArray, start::Int32, num::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(or, "remove", IloConstraint, (Vector{IloConstraint}, jint, jint), constr, start, num)
end

## IloConstraint

function cpo_java_addable_getname(cp::JavaCPOModel, addable::Addable)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(addable, "getName", JString, ())
end

function cpo_java_addable_setname(cp::JavaCPOModel, addable::Addable, name::String)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(addable, "setName", Nothing, (JString,), name)
end

## IloRange

function cpo_java_range_clearexpr(cp::JavaCPOModel, range::IloRange)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(range, "clearExpr", Nothing, ())
end

function cpo_java_range_getexpr(cp::JavaCPOModel, range::IloRange)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(range, "getExpr", IloNumExpr, ())
end

function cpo_java_range_getlb(cp::JavaCPOModel, range::IloRange)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(range, "getLB", jdouble, ())
end

function cpo_java_range_getub(cp::JavaCPOModel, range::IloRange)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(range, "getUB", jdouble, ())
end

function cpo_java_range_setbounds(cp::JavaCPOModel, range::IloRange, lb::Real, ub::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(range, "setBounds", Nothing, (jdouble, jdouble), lb, ub)
end

function cpo_java_range_setexpr(cp::JavaCPOModel, range::IloRange, expr::NumExpr)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(range, "setExpr", Nothing, (IloNumExpr,), expr)
end

function cpo_java_range_setlb(cp::JavaCPOModel, range::IloRange, lb::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(range, "setLB", Nothing, (jdouble,), lb)
end

function cpo_java_range_setub(cp::JavaCPOModel, range::IloRange, ub::Real)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(range, "setUB", Nothing, (jdouble,), ub)
end

## Objective

function cpo_java_maximize(cp::JavaCPOModel, expr::IntExpr)
    return jcall(cp.cp, "maximize", IloObjective, (IloIntExpr,), expr)
end

function cpo_java_maximize(cp::JavaCPOModel, expr::NumExpr, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "maximize", IloObjective, (IloNumExpr,), expr)
    else
        return jcall(cp.cp, "maximize", IloObjective, (IloNumExpr, JString), expr, name)
    end
end

function cpo_java_maximize(cp::JavaCPOModel, expr::IloMultiCriterionExpr)
    return jcall(cp.cp, "maximize", IloObjective, (IloMultiCriterionExpr,), expr)
end

function cpo_java_minimize(cp::JavaCPOModel, expr::IntExpr)
    return jcall(cp.cp, "minimize", IloObjective, (IloIntExpr,), expr)
end

function cpo_java_minimize(cp::JavaCPOModel, expr::NumExpr, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "minimize", IloObjective, (IloNumExpr,), expr)
    else
        return jcall(cp.cp, "minimize", IloObjective, (IloNumExpr, JString), expr, name)
    end
end

function cpo_java_minimize(cp::JavaCPOModel, expr::IloMultiCriterionExpr)
    return jcall(cp.cp, "minimize", IloObjective, (IloMultiCriterionExpr,), expr)
end

function cpo_java_staticlex(cp::JavaCPOModel, criteria::NumExprArray, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "staticLex", IloMultiCriterionExpr, (Vector{IloNumExpr},), criteria)
    else
        return jcall(cp.cp, "staticLex", IloMultiCriterionExpr, (Vector{IloNumExpr}, JString), criteria, name)
    end
    # Other staticLex don't need to be mapped, just facility functions in Java for short arrays.
end

## Creation and addition

function cpo_java_addeq(cp::JavaCPOModel, val::Real, expr::NumExpr, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "addEq", IloRange, (jdouble, IloNumExpr), val, expr)
    else
        return jcall(cp.cp, "addEq", IloRange, (jdouble, IloNumExpr, JString), val, expr, name)
    end
end

function cpo_java_addeq(cp::JavaCPOModel, expr::NumExpr, val::Real, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "addEq", IloRange, (IloNumExpr, jdouble), expr, val)
    else
        return jcall(cp.cp, "addEq", IloRange, (IloNumExpr, jdouble, JString), expr, val, name)
    end
end

function cpo_java_addeq(cp::JavaCPOModel, expr::NumExpr, val::NumExpr, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "addEq", IloConstraint, (IloNumExpr, IloNumExpr), expr, val)
    else
        return jcall(cp.cp, "addEq", IloConstraint, (IloNumExpr, IloNumExpr, JString), expr, val, name)
    end
end

function cpo_java_addge(cp::JavaCPOModel, val::Real, expr::NumExpr, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "addGe", IloRange, (jdouble, IloNumExpr), val, expr)
    else
        return jcall(cp.cp, "addGe", IloRange, (jdouble, IloNumExpr, JString), val, expr, name)
    end
end

function cpo_java_addge(cp::JavaCPOModel, expr::NumExpr, val::Real, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "addGe", IloRange, (IloNumExpr, jdouble), expr, val)
    else
        return jcall(cp.cp, "addGe", IloRange, (IloNumExpr, jdouble, JString), expr, val, name)
    end
end

function cpo_java_addge(cp::JavaCPOModel, expr::NumExpr, val::NumExpr, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "addGe", IloConstraint, (IloNumExpr, IloNumExpr), expr, val)
    else
        return jcall(cp.cp, "addGe", IloConstraint, (IloNumExpr, IloNumExpr, JString), expr, val, name)
    end
end

function cpo_java_addle(cp::JavaCPOModel, val::Real, expr::NumExpr, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "addLe", IloRange, (jdouble, IloNumExpr), val, expr)
    else
        return jcall(cp.cp, "addLe", IloRange, (jdouble, IloNumExpr, JString), val, expr, name)
    end
end

function cpo_java_addle(cp::JavaCPOModel, expr::NumExpr, val::Real, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "addLe", IloRange, (IloNumExpr, jdouble), expr, val)
    else
        return jcall(cp.cp, "addLe", IloRange, (IloNumExpr, jdouble, JString), expr, val, name)
    end
end

function cpo_java_addle(cp::JavaCPOModel, expr::NumExpr, val::NumExpr, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "addLe", IloConstraint, (IloNumExpr, IloNumExpr), expr, val)
    else
        return jcall(cp.cp, "addLe", IloConstraint, (IloNumExpr, IloNumExpr, JString), expr, val, name)
    end
end

function cpo_java_addmaximize(cp::JavaCPOModel, expr::NumExpr, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "addMaximize", IloObjective, (IloNumExpr,), expr)
    else
        return jcall(cp.cp, "addMaximize", IloObjective, (IloNumExpr, JString), expr, name)
    end
end

function cpo_java_addminimize(cp::JavaCPOModel, expr::NumExpr, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "addMinimize", IloObjective, (IloNumExpr,), expr)
    else
        return jcall(cp.cp, "addMinimize", IloObjective, (IloNumExpr, JString), expr, name)
    end
end

# TODO: addObjective? Redundant with the two previous ones.

function cpo_java_addrange(cp::JavaCPOModel, lb::Real, expr::NumExpr, ub::Real, name::String="")
    if length(name) == 0
        return jcall(cp.cp, "addRange", IloRange, (jdouble, IloNumExpr, jdouble), lb, expr, ub)
    else
        return jcall(cp.cp, "addRange", IloRange, (jdouble, IloNumExpr, jdouble, JString), lb, expr, ub, name)
    end
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

function cpo_java_getdomainsize(cp::JavaCPOModel, var::NumVar)
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

function cpo_java_getilocumulfunctionexpr(cp::JavaCPOModel, name::String)
    return jcall(cp.cp, "getIloCumulFunctionExpr", IloCumulFunctionExpr, (JString,), name)
end

function cpo_java_getilointervalsequencevar(cp::JavaCPOModel, name::String)
    return jcall(cp.cp, "getIloIntervalSequenceVar", IloIntervalSequenceVar, (JString,), name)
end

function cpo_java_getilointervalvar(cp::JavaCPOModel, name::String)
    return jcall(cp.cp, "getIloIntervalVar", IloIntervalVar, (JString,), name)
end

function cpo_java_getilointvar(cp::JavaCPOModel, name::String)
    return jcall(cp.cp, "getIloIntVar", IloIntVar, (JString,), name)
end

function cpo_java_getilostatefunction(cp::JavaCPOModel, name::String)
    return jcall(cp.cp, "getIloStateFunction", IloStateFunction, (JString,), name)
end

function cpo_java_getincumbentvalue(cp::JavaCPOModel, expr::NumExpr)
    return jcall(cp.cp, "getIncumbentValue", jdouble, (IloNumExpr,), expr)
end

function cpo_java_getintvalue(cp::JavaCPOModel, expr::IntExpr)
    return jcall(cp.cp, "getInt", jint, (IloIntExpr,), expr)
end

function cpo_java_getkpivalue(cp::JavaCPOModel, name::String)
    return jcall(cp.cp, "getKPIValue", jint, (JString,), name)
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

function cpo_java_getmax(cp::JavaCPOModel, var::NumVar)
    return jcall(cp.cp, "getMax", jdouble, (IloNumVar,), var)
end

function cpo_java_getmin(cp::JavaCPOModel, var::NumVar)
    return jcall(cp.cp, "getMin", jdouble, (IloNumVar,), var)
end

function cpo_java_getname(cp::JavaCPOModel)
    return jcall(cp.cp, "getName", JString, ())
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

function cpo_java_getvalue(cp::JavaCPOModel, expr::IntExpr)
    return jcall(cp.cp, "getValue", jdouble, (IloIntExpr,), expr)
end

function cpo_java_getvalue(cp::JavaCPOModel, expr::IloIntVar)
    return jcall(cp.cp, "getValue", jdouble, (IloIntVar,), expr)
end

function cpo_java_getvalue(cp::JavaCPOModel, expr::NumExpr)
    return jcall(cp.cp, "getValue", jdouble, (IloNumExpr,), expr)
end

function cpo_java_getvalue(cp::JavaCPOModel, name::String)
    return jcall(cp.cp, "getValue", jdouble, (JString,), name)
end

function cpo_java_getversion(cp::JavaCPOModel)
    return jcall(cp.cp, "getValue", JString, ())
end

function cpo_java_hasobjective(cp::JavaCPOModel)
    return Bool(jcall(cp.cp, "hasObjective", jboolean, ()))
end

function cpo_java_isabsent(cp::JavaCPOModel, var::IloIntervalVar)
    return Bool(jcall(cp.cp, "isAbsent", jboolean, (IloIntervalVar), var))
end

function cpo_java_isfixed(cp::JavaCPOModel, var::IloIntervalSequenceVar)
    return Bool(jcall(cp.cp, "isFixed", jboolean, (IloIntervalSequenceVar), var))
end

function cpo_java_isfixed(cp::JavaCPOModel, var::IloIntervalVar)
    return Bool(jcall(cp.cp, "isFixed", jboolean, (IloIntervalVar), var))
end

function cpo_java_isfixed(cp::JavaCPOModel, var::NumVar)
    return Bool(jcall(cp.cp, "isFixed", jboolean, (IloNumVar,), var))
end

function cpo_java_isindomain(cp::JavaCPOModel, var, value::Int32)
    return Bool(jcall(cp.cp, "isInDomain", jboolean, (IloNumVar, jint), var, value))
end

function cpo_java_ispresent(cp::JavaCPOModel, var::IloIntervalVar)
    return Bool(jcall(cp.cp, "isPresent", jboolean, (IloIntervalVar,), var))
end

function cpo_java_next(cp::JavaCPOModel)
    return Bool(jcall(cp.cp, "next", jboolean, ()))
end

function cpo_java_propagate(cp::JavaCPOModel)
    return Bool(jcall(cp.cp, "propagate", jboolean, ()))
end

function cpo_java_propagate(cp::JavaCPOModel, constr)
    return Bool(jcall(cp.cp, "propagate", jboolean, (IloConstraint,), constr))
end

function cpo_java_refineconflict(cp::JavaCPOModel)
    return Bool(jcall(cp.cp, "refineConflict", jboolean, ()))
end

function cpo_java_refineconflict(cp::JavaCPOModel, constrs::ConstraintArray)
    return Bool(jcall(cp.cp, "refineConflict", jboolean, (Vector{IloConstraint},), constrs))
end

function cpo_java_refineconflict(cp::JavaCPOModel, constrs::ConstraintArray, prefs::Vector{T}) where {T <: Real}
    return Bool(jcall(cp.cp, "refineConflict", jboolean, (Vector{IloConstraint}, Vector{jdouble}), constrs, prefs))
end

function cpo_java_restore(cp::JavaCPOModel, solution::IloSolution)
    return Bool(jcall(cp.cp, "restore", jboolean, (IloSolution,), solution))
end

function cpo_java_setname(cp::JavaCPOModel, name::String)
    return jcall(cp.cp, "setName", Nothing, (JString,), name)
end

function cpo_java_solve(cp::JavaCPOModel)
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

function cpo_java_solution_add(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "add", Nothing, (IloIntervalVar,), var)
end

function cpo_java_solution_add(cp::JavaCPOModel, solution::IloSolution, vars::Vector{IloIntervalVar})
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "add", Nothing, (Vector{IloIntervalVar},), var)
end

function cpo_java_solution_add(cp::JavaCPOModel, solution::IloSolution, var::IloIntVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "add", Nothing, (IloIntVar,), var)
end

function cpo_java_solution_add(cp::JavaCPOModel, solutio::IloSolution, vars::Vector{IloIntVar})
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "add", Nothing, (Vector{IloIntVar},), vars)
end

function cpo_java_solution_add(cp::JavaCPOModel, solution::IloSolution, var::IloNumVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "add", Nothing, (IloNumVar,), var)
end

function cpo_java_solution_contains(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return Bool(jcall(solution, "contains", jboolean, (IloIntervalVar,), var))
end

function cpo_java_solution_contains(cp::JavaCPOModel, solution::IloSolution, var::IloIntVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return Bool(jcall(solution, "contains", jboolean, (IloIntVar,), var))
end

function cpo_java_solution_end(cp::JavaCPOModel, solution::IloSolution)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "end", Nothing, ())
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

function cpo_java_solution_getmax(cp::JavaCPOModel, solution::IloSolution, var::IloIntVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "getMax", jint, (IloIntVar,), var)
end

function cpo_java_solution_getmax(cp::JavaCPOModel, solution::IloSolution, var::IloNumVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "getMax", jdouble, (IloNumVar,), var)
end

function cpo_java_solution_getmin(cp::JavaCPOModel, solution::IloSolution, var::IloIntVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "getMin", jint, (IloIntVar,), var)
end

function cpo_java_solution_getmin(cp::JavaCPOModel, solution::IloSolution, var::IloNumVar)
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

function cpo_java_solution_getvalue(cp::JavaCPOModel, solution::IloSolution, var::IloIntVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "getValue", jint, (IloIntVar,), var)
end

function cpo_java_solution_getvalue(cp::JavaCPOModel, solution::IloSolution, var::IloNumVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "getValue", jdouble, (IloNumVar,), var)
end

function cpo_java_solution_isabsent(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return Bool(jcall(solution, "isAbsent", jboolean, (IloIntervalVar,), var))
end

function cpo_java_solution_isfixed(cp::JavaCPOModel, solution::IloSolution, var::IloIntVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return Bool(jcall(solution, "isFixed", jboolean, (IloIntVar,), var))
end

function cpo_java_solution_isindomain(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar, value::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return Bool(jcall(solution, "isInDomain", jboolean, (IloIntervalVar, jint), var, value))
end

function cpo_java_solution_ispresent(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return Bool(jcall(solution, "isPresent", jboolean, (IloIntervalVar,), var))
end

function cpo_java_solution_remove(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "remove", Nothing, (IloIntervalVar,), var)
end

function cpo_java_solution_remove(cp::JavaCPOModel, solution::IloSolution, vars::Vector{IloIntervalVar})
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "remove", Nothing, (Vector{IloIntervalVar},), vars)
end

function cpo_java_solution_remove(cp::JavaCPOModel, solution::IloSolution, var::IloIntVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "remove", Nothing, (IloIntVar,), var)
end

function cpo_java_solution_remove(cp::JavaCPOModel, solution::IloSolution, vars::Vector{IloIntVar})
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "remove", Nothing, (Vector{IloIntVar},), vars)
end

function cpo_java_solution_setabsent(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setAbsent", void, (IloIntervalVar,), var)
end

function cpo_java_solution_setdomain(cp::JavaCPOModel, solution::IloSolution, var, vmin::Int32, vmax::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return Bool(jcall(solution, "setDomain", jboolean, (IloIntVar, jint, jint), var, vmin, vmax))
end

function cpo_java_solution_setend(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar, v::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setEnd", void, (IloIntervalVar, jint), var, v)
end

function cpo_java_solution_setendmax(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar, v::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setEndMax", void, (IloIntervalVar, jint), var, v)
end

function cpo_java_solution_setendmin(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar, v::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setEndMin", void, (IloIntervalVar, jint), var, v)
end

function cpo_java_solution_setlength(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar, v::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setLength", void, (IloIntervalVar, jint), var, v)
end

function cpo_java_solution_setlengthmax(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar, v::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setLengthMax", void, (IloIntervalVar, jint), var, v)
end

function cpo_java_solution_setlengthmin(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar, v::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setLengthMin", void, (IloIntervalVar, jint), var, v)
end

function cpo_java_solution_setmax(cp::JavaCPOModel, solution::IloSolution, var::IloIntVar, v::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setMax", Nothing, (IloIntVar, jint), var, v)
end

function cpo_java_solution_setmax(cp::JavaCPOModel, solution::IloSolution, var::IloNumVar, v::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setMax", Nothing, (IloNumVar, jint), var, v)
end

function cpo_java_solution_setmin(cp::JavaCPOModel, solution::IloSolution, var::IloIntVar, v::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setMin", Nothing, (IloIntVar, jint), var, v)
end

function cpo_java_solution_setmin(cp::JavaCPOModel, solution::IloSolution, var::IloNumVar, v::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setMin", Nothing, (IloNumVar, jint), var, v)
end

function cpo_java_solution_setoptional(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setOptional", void, (IloIntervalVar,), var)
end

function cpo_java_solution_setpresent(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setPresent", void, (IloIntervalVar,), var)
end

function cpo_java_solution_setsize(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar, v::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setSize", Nothing, (IloIntervalVar, jint), var, v)
end

function cpo_java_solution_setsizemax(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar, v::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setSizeMax", Nothing, (IloIntervalVar, jint), var, v)
end

function cpo_java_solution_setsizemin(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar, v::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setSizeMin", Nothing, (IloIntervalVar, jint), var, v)
end

function cpo_java_solution_setstart(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar, v::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setStart", Nothing, (IloIntervalVar, jint), var, v)
end

function cpo_java_solution_setstartmax(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar, v::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setStartMax", Nothing, (IloIntervalVar, jint), var, v)
end

function cpo_java_solution_setstartmin(cp::JavaCPOModel, solution::IloSolution, var::IloIntervalVar, v::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setStartMin", Nothing, (IloIntervalVar, jint), var, v)
end

function cpo_java_solution_setvalue(cp::JavaCPOModel, solution::IloSolution, var::IloIntVar, v::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setValue", Nothing, (IloIntVar, jint), var, v)
end

function cpo_java_solution_setvalue(cp::JavaCPOModel, solution::IloSolution, var::IloNumVar, v::Int32)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "setValue", Nothing, (IloNumVar, jint), var, v)
end

function cpo_java_solution_store(cp::JavaCPOModel, solution::IloSolution)
    # cp argument is useless, but kept to be consistent with the rest of the API.
    return jcall(solution, "store", Nothing, ())
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

function cpo_java_getconflict(cp::JavaCPOModel, constr::Constraint)
    return jcall(cp.cp, "getConflict", ConflictStatus, (IloConstraint,), constr)
end

function cpo_java_getconflict(cp::JavaCPOModel, var::IloIntervalVar)
    return jcall(cp.cp, "getConflict", ConflictStatus, (IloIntervalVar,), constr)
end

function cpo_java_getconflict(cp::JavaCPOModel, var::NumVar)
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

function cpo_java_remove(cp::JavaCPOModel, addable::Addable)
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

function cpo_java_runseeds(cp::JavaCPOModel, n::Int32)
    return jcall(cp.cp, "runSeeds", Nothing, (jint,), n)
end

# TODO: searchPhase

## Parameters
# This section deviates from the Java API, as binding all parameters would
# create a lot of functions. Moreover, these values must be loaded at runtime,
# i.e. in the __init__ function.
# Hence, take as input the string name of the parameter, as defined in IntParam,
# DoubleParam, or their info twins.

function cpo_java_getintinfo(cp::JavaCPOModel, name::String)
    param = jfield(IloIntInfo, name, IloIntInfo)
    return jcall(cp.cp, "getInfo", jint, (IloIntInfo,), param)
end

function cpo_java_getdoubleinfo(cp::JavaCPOModel, name::String)
    param = jfield(IloDoubleInfo, name, IloDoubleInfo)
    return jcall(cp.cp, "getInfo", jdouble, (IloDoubleInfo,), param)
end

function cpo_java_getdoubleparameter(cp::JavaCPOModel, name::String)
    param = jfield(IloDoubleParam, name, IloDoubleParam)
    return jcall(cp.cp, "getParameter", jdouble, (IloDoubleParam,), param)
end

function cpo_java_getdoubledefaultparameter(cp::JavaCPOModel, name::String)
    param = jfield(IloDoubleParam, name, IloDoubleParam)
    return jcall(cp.cp, "getParameterDefault", jdouble, (IloDoubleParam,), param)
end

function cpo_java_getintparameter(cp::JavaCPOModel, name::String)
    param = jfield(IloIntParam, name, IloIntParam)
    return jcall(cp.cp, "getParameter", jint, (IloIntParam,), param)
end

function cpo_java_getintdefaultparameter(cp::JavaCPOModel, name::String)
    param = jfield(IloIntParam, name, IloIntParam)
    return jcall(cp.cp, "getParameterDefault", jint, (IloIntParam,), param)
end

function cpo_java_setdoubleparameter(cp::JavaCPOModel, name::String, value::Real)
    param = jfield(IloDoubleParam, name, IloDoubleParam)
    return jcall(cp.cp, "setParameter", Nothing, (IloIntParam, jdouble), param, value)
end

function cpo_java_setintparameter(cp::JavaCPOModel, name::String, value::String)
    param = jfield(IloIntParam, name, IloIntParam)
    field = jfield(IloParameterValues, value, IloParameterValues)
    return jcall(cp.cp, "setParameter", Nothing, (IloIntParam, IloParameterValues), param, field)
end

function cpo_java_setintparameter(cp::JavaCPOModel, name::String, value::Int32)
    param = jfield(IloIntParam, name, IloIntParam)
    return jcall(cp.cp, "setParameter", Nothing, (IloIntParam, jint), param, value)
end

function cpo_java_isparamint(::JavaCPOModel, name::String)
    try
        # Try to get a member with the given name in `IntParam`.
        jfield(IloIntParam, name, IloIntParam)
        return true
    catch
        return false
    end
end

function cpo_java_isparamdouble(::JavaCPOModel, name::String)
    try
        # Try to get a member with the given name in `DoubleParam`.
        jfield(IloDoubleParam, name, IloDoubleParam)
        return true
    catch
        return false
    end
end
