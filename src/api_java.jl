"""
    cpo_java_init()

Initialises the JVM to be able to use CPLEX CP Optimizer.
"""
function cpo_java_init()
    JavaCall.addClassPath(libcplexcpojava)
    JavaCall.init()
end

function cpo_java_model()
    jcp = @jimport ilog.cp.IloCP
    return jcp(())
end
