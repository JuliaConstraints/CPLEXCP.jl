@testset "Java API" begin
    ## Model initialisation
    cpo_java_init()
    model = cpo_java_model()
    @test typeof(model) == JavaCPOModel

    # Types
    @test model.intvar == JavaCall.JavaObject{Symbol("ilog.concert.IloIntVar")}
    @test model.intervalvar == JavaCall.JavaObject{Symbol("ilog.concert.IloIntervalVar")}
    @test model.numvar == JavaCall.JavaObject{Symbol("ilog.concert.IloNumVar")}

    @test model.intvararray == Vector{model.intvar}
    @test model.numvararray == Vector{model.numvar}

    ## Variable creation
    # Integer variables
    @test typeof(cpo_java_intvar_bounded(model, 5, 10)) == model.intvar
    @test typeof(cpo_java_intvar_bounded(model, 10, 5)) == model.intvar # No check on CPLEXCP side
    @test typeof(cpo_java_intvar_bounded(model, 5, 10, "var")) == model.intvar

    @test typeof(cpo_java_intvar_discrete(model, [5, 10])) == model.intvar
    @test typeof(cpo_java_intvar_discrete(model, [5, 10], "var")) == model.intvar

    l = cpo_java_intvararray_bounded(model, 20, 5, 10)
    @test typeof(l) == model.intvararray
    @test eltype(l) == model.intvar
    @test length(l) == 20
    l = cpo_java_intvararray_discrete(model, 20, [5, 10])
    @test typeof(l) == model.intvararray
    @test eltype(l) == model.intvar
    @test length(l) == 20

    # Numerical variables
    @test typeof(cpo_java_numvar(model, 5.0, 10.0)) == model.numvar

    l = cpo_java_numvararray(model, 20, 5.0, 10.0)
    @test typeof(l) == model.numvararray
    @test eltype(l) == model.numvar
    @test length(l) == 20

    # Interval variables
    @test typeof(cpo_java_intervalvar(model)) == model.intervalvar
    @test typeof(cpo_java_intervalvar(model, "var")) == model.intervalvar
    @test typeof(cpo_java_intervalvar_fixedsize(model, 5)) == model.intervalvar
    @test typeof(cpo_java_intervalvar_fixedsize(model, 5, "var")) == model.intervalvar
    @test typeof(cpo_java_intervalvar_boundedsize(model, 5, 10)) == model.intervalvar

    # Sequence-of-intervals variables
end
