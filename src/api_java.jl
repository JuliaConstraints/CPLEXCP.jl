"""
    cpo_java_init()

Initialises the JVM to be able to use CPLEX CP Optimizer.
"""
function cpo_java_init()
    JavaCall.addClassPath(libcplexcpojava)
    JavaCall.init()
end

mutable struct JavaCPOModel
    cp
end

function cpo_java_model()
    jcp = @jimport ilog.cp.IloCP
    return JavaCPOModel(jcp(()))
end

## Variable creation

# Integer variables
function cpo_java_intvar_bounded(cp::JavaCPOModel, lb::T, ub::T, name::String="") where {T <: Integer}
    jcall(cp.cp, "intVar", (jint, jint, JString), lb, ub, name)
end

function cpo_java_intvar_discrete(cp::JavaCPOModel, values::Vector{T}, name::String="") where {T <: Integer}
    jvalues = JavaCall.convert_arg(Vector{jint}, values)
    if length(name) == 0
        jcall(cp.cp, "intVar", (Vector{jint},), jvalues)
    else
        jcall(cp.cp, "intVar", (Vector{jint}, JString), jvalues, name)
    end
end

function cpo_java_intvararray_bounded(cp::JavaCPOModel, n::T, lb::T, ub::T, name::String="") where {T <: Integer}
    jcall(cp.cp, "intVarArray", (jint, jint, jint, JString), n, lb, ub, name)
end

function cpo_java_intvararray_discrete(cp::JavaCPOModel, n::T, values::Vector{T}, name::String="") where {T <: Integer}
    jvalues = JavaCall.convert_arg(Vector{jint}, values)
    jcall(cp.cp, "intVarArray", (jint, Vector{jint}, JString), n, jvalues, name)
end

# Numerical variables
function cpo_java_numvararray(cp::JavaCPOModel, n::Int, lb::T, ub::T, name::String="") where {T <: Real}
    jcall(cp.cp, "numVarArray", (jint, jdouble, jdouble, JString), n, lb, ub, name)
end

# Interval variables
function cpo_java_intervalvar(cp::JavaCPOModel, name::String="")
    if length(name) == 0
        jcall(cp.cp, "intervalVar", ())
    else
        jcall(cp.cp, "intervalVar", (JString), name)
    end
end

function cpo_java_intervalvar_fixedsize(cp::JavaCPOModel, size::Integer, name::String="")
    if length(name) == 0
        jcall(cp.cp, "intervalVar", (jint,), size)
    else
        jcall(cp.cp, "intervalVar", (jint, JString), size, name)
    end
end

function cpo_java_intervalvar_boundedsize(cp::JavaCPOModel, size_lb::Integer, size_ub::Integer)
    jcall(cp.cp, "intervalVar", (jint, jint), size_lb, size_ub)
end

# TODO: public IloIntervalVar intervalVar(int szmin, int szmax, boolean opt, IloNumToNumStepFunction intensity, int granularity)

# Sequence-of-intervals variables
# TODO: arrays of interval objects
# intervalSequenceVar
