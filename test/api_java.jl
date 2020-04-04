@testset "Java API" begin
    @testset "Unit tests" begin
        @testset "Model initialisation" begin
            cpo_java_init()
            model = cpo_java_model()
            @test typeof(model) == JavaCPOModel

            # Types
            @test model.intvar == JavaCall.JavaObject{Symbol("ilog.concert.IloIntVar")}
            @test model.intervalvar == JavaCall.JavaObject{Symbol("ilog.concert.IloIntervalVar")}
            @test model.numvar == JavaCall.JavaObject{Symbol("ilog.concert.IloNumVar")}

            @test model.intexpr == JavaCall.JavaObject{Symbol("ilog.concert.IloIntExpr")}
            @test model.numexpr == JavaCall.JavaObject{Symbol("ilog.concert.IloNumExpr")}
            @test model.inttupleset == JavaCall.JavaObject{Symbol("ilog.concert.IloIntTupleSet")}

            @test model.constraint == JavaCall.JavaObject{Symbol("ilog.concert.IloConstraint")}
            @test model.alternative == JavaCall.JavaObject{Symbol("ilog.concert.IloAlternative")}

            @test model.intvararray == Vector{model.intvar}
            @test model.numvararray == Vector{model.numvar}
            @test model.intexprarray == Vector{model.intexpr}
            @test model.numexprarray == Vector{model.numexpr}
        end

        @testset "Variable creation" begin
            model = cpo_java_model()

            # Integer variables
            @test typeof(cpo_java_intvar(model, 5, 10)) == model.intvar
            @test typeof(cpo_java_intvar(model, 10, 5)) == model.intvar # No check on CPLEXCP side
            @test typeof(cpo_java_intvar(model, 5, 10, "var")) == model.intvar

            @test typeof(cpo_java_intvar(model, [5, 10])) == model.intvar
            @test typeof(cpo_java_intvar(model, [5, 10], "var")) == model.intvar

            l = cpo_java_intvararray(model, 20, 5, 10)
            @test typeof(l) == model.intvararray
            @test eltype(l) == model.intvar
            @test length(l) == 20
            l = cpo_java_intvararray(model, 20, [5, 10])
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
            @test typeof(cpo_java_intervalvar(model, 5)) == model.intervalvar
            @test typeof(cpo_java_intervalvar(model, 5, "var")) == model.intervalvar
            @test typeof(cpo_java_intervalvar(model, 5, 10)) == model.intervalvar

            # Sequence-of-intervals variables
            i1 = cpo_java_intervalvar(model)
            i2 = cpo_java_intervalvar(model)
            @test typeof(cpo_java_intervalsequencevar(model, [i1, i2])) == model.intervalsequencevar
        end
    end
end
