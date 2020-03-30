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
end

function cpo_java_model()
    # Import the required symbols and store them in the model for future use.
    jcp = @jimport ilog.cp.IloCP
    intvar = @jimport ilog.concert.IloIntVar
    intervalvar = @jimport ilog.concert.IloIntervalVar
    numvar = @jimport ilog.concert.IloNumVar

    # Actually build the model.
    model = jcp(())

    # Return the right data structure
    return JavaCPOModel(model, intvar, Vector{intvar}, intervalvar, numvar, Vector{numvar})
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

# Sequence-of-intervals variables
# TODO: arrays of interval objects
# intervalSequenceVar
