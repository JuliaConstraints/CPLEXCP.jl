@testset "Java API" begin
    @testset "Unit tests" begin
        @testset "Type hierarchy" begin
            for T in [IloConstraint, IloAlternative, IloIsomorphism, IloNoOverlap, IloRange, IloSpan, IloSynchronize]
                @test T <: Constraint
                @test Vector{T} <: ConstraintArray
                @test T <: Addable
                @test Vector{T} <: AddableArray

                @test T <: IntExpr
                @test Vector{T} <: IntExprArray
            end

            for T in [IloIntVar, IloIntExpr, Constraint]
                @test T <: IntExpr
                @test Vector{T} <: IntExprArray
            end

            for T in [IloIntVar, IloNumVar]
                @test T <: NumVar
                @test Vector{T} <: NumVarArray
            end

            for T in [IntExpr, IloNumVar, IloNumExpr]
                @test T <: NumExpr
                @test Vector{T} <: NumExprArray
            end
        end

        @testset "Model initialisation" begin
            cpo_java_init()
            model = cpo_java_model()
            @test typeof(model) == JavaCPOModel
        end

        @testset "Variable creation" begin
            model = cpo_java_model()

            # Integer variables
            @test typeof(cpo_java_intvar(model, 5, 10)) == IloIntVar
            @test typeof(cpo_java_intvar(model, 10, 5)) == IloIntVar # No check on CPLEXCP side
            @test typeof(cpo_java_intvar(model, 5, 10, "var")) == IloIntVar

            @test typeof(cpo_java_intvar(model, [5, 10])) == IloIntVar
            @test typeof(cpo_java_intvar(model, [5, 10], "var")) == IloIntVar

            l = cpo_java_intvararray(model, 20, 5, 10)
            @test typeof(l) == Vector{IloIntVar}
            @test eltype(l) == IloIntVar
            @test length(l) == 20
            l = cpo_java_intvararray(model, 20, [5, 10])
            @test typeof(l) == Vector{IloIntVar}
            @test eltype(l) == IloIntVar
            @test length(l) == 20

            # Numerical variables
            @test typeof(cpo_java_numvar(model, 5.0, 10.0)) == IloNumVar

            l = cpo_java_numvararray(model, 20, 5.0, 10.0)
            @test typeof(l) == Vector{IloNumVar}
            @test eltype(l) == IloNumVar
            @test length(l) == 20

            # Interval variables
            @test typeof(cpo_java_intervalvar(model)) == IloIntervalVar
            @test typeof(cpo_java_intervalvar(model, "var")) == IloIntervalVar
            @test typeof(cpo_java_intervalvar(model, 5)) == IloIntervalVar
            @test typeof(cpo_java_intervalvar(model, 5, "var")) == IloIntervalVar
            @test typeof(cpo_java_intervalvar(model, 5, 10)) == IloIntervalVar

            # Sequence-of-intervals variables
            i1 = cpo_java_intervalvar(model)
            i2 = cpo_java_intervalvar(model)
            @test typeof(cpo_java_intervalsequencevar(model, [i1, i2])) == IloIntervalSequenceVar
        end
    end

    @testset "Integration tests" begin
        # Based on the provided examples, translated from Java.
        # All that should be ported: https://perso.ensta-paris.fr/~diam/ro/online/cplex/cplex1271/CP_Optimizer/User_manual/topics/constraints_arithmetic_expr.html

        @testset "Color" begin
            model = cpo_java_model()
            belgium = cpo_java_intvar(model, 0, 3)
            denmark = cpo_java_intvar(model, 0, 3)
            france = cpo_java_intvar(model, 0, 3)
            germany = cpo_java_intvar(model, 0, 3)
            luxembourg = cpo_java_intvar(model, 0, 3)
            netherlands = cpo_java_intvar(model, 0, 3)

            cpo_java_add(model, cpo_java_neq(model, belgium, france))
            cpo_java_add(model, cpo_java_neq(model, belgium, germany))
            cpo_java_add(model, cpo_java_neq(model, belgium, netherlands))
            cpo_java_add(model, cpo_java_neq(model, belgium, luxembourg))
            cpo_java_add(model, cpo_java_neq(model, denmark, germany))
            cpo_java_add(model, cpo_java_neq(model, france, germany))
            cpo_java_add(model, cpo_java_neq(model, france, luxembourg))
            cpo_java_add(model, cpo_java_neq(model, germany, luxembourg))
            cpo_java_add(model, cpo_java_neq(model, germany, netherlands))

            status = cpo_java_solve(model)
            @test status

            @test cpo_java_getvalue(model, belgium) == cpo_java_getvalue(model, belgium)
            @test cpo_java_getvalue(model, denmark) == cpo_java_getvalue(model, denmark)
            @test cpo_java_getvalue(model, france) == cpo_java_getvalue(model, france)
            @test cpo_java_getvalue(model, germany) == cpo_java_getvalue(model, germany)
            @test cpo_java_getvalue(model, luxembourg) == cpo_java_getvalue(model, luxembourg)
            @test cpo_java_getvalue(model, netherlands) == cpo_java_getvalue(model, netherlands)

            @test cpo_java_getvalue(model, belgium) != cpo_java_getvalue(model, france)
            @test cpo_java_getvalue(model, belgium) != cpo_java_getvalue(model, germany)
            @test cpo_java_getvalue(model, belgium) != cpo_java_getvalue(model, netherlands)
            @test cpo_java_getvalue(model, belgium) != cpo_java_getvalue(model, luxembourg)
            @test cpo_java_getvalue(model, denmark) != cpo_java_getvalue(model, germany)
            @test cpo_java_getvalue(model, france) != cpo_java_getvalue(model, germany)
            @test cpo_java_getvalue(model, france) != cpo_java_getvalue(model, luxembourg)
            @test cpo_java_getvalue(model, germany) != cpo_java_getvalue(model, luxembourg)
            @test cpo_java_getvalue(model, germany) != cpo_java_getvalue(model, netherlands)
        end

        @testset "Alloc" begin
            # Model data.
            ncell = 25
            nfreq = 256
            nchannel = [8, 6, 6, 1, 4, 4, 8, 8, 8, 8, 4, 9, 8, 4, 4, 10, 8, 9, 8, 4, 5, 4, 8, 1, 1]
            dist = [
                16  1   1   0   0   0   0   0   1   1   1   1   1   2   2   1   1   0   0   0   2   2   1   1   1;
                1   16  2   0   0   0   0   0   2   2   1   1   1   2   2   1   1   0   0   0   0   0   0   0   0;
                1   2   16  0   0   0   0   0   2   2   1   1   1   2   2   1   1   0   0   0   0   0   0   0   0;
                0   0   0   16  2   2   0   0   0   0   0   0   0   0   0   0   0   1   1   1   0   0   0   1   1;
                0   0   0   2   16  2   0   0   0   0   0   0   0   0   0   0   0   1   1   1   0   0   0   1   1;
                0   0   0   2   2   16  0   0   0   0   0   0   0   0   0   0   0   1   1   1   0   0   0   1   1;
                0   0   0   0   0   0   16  2   0   0   1   1   1   0   0   1   1   1   1   2   0   0   0   1   1;
                0   0   0   0   0   0   2   16  0   0   1   1   1   0   0   1   1   1   1   2   0   0   0   1   1;
                1   2   2   0   0   0   0   0   16  2   2   2   2   2   2   1   1   1   1   1   1   1   0   1   1;
                1   2   2   0   0   0   0   0   2   16  2   2   2   2   2   1   1   1   1   1   1   1   0   1   1;
                1   1   1   0   0   0   1   1   2   2   16  2   2   2   2   2   2   1   1   2   1   1   0   1   1;
                1   1   1   0   0   0   1   1   2   2   2   16  2   2   2   2   2   1   1   2   1   1   0   1   1;
                1   1   1   0   0   0   1   1   2   2   2   2   16  2   2   2   2   1   1   2   1   1   0   1   1;
                2   2   2   0   0   0   0   0   2   2   2   2   2   16  2   1   1   1   1   1   1   1   1   1   1;
                2   2   2   0   0   0   0   0   2   2   2   2   2   2   16  1   1   1   1   1   1   1   1   1   1;
                1   1   1   0   0   0   1   1   1   1   2   2   2   1   1   16  2   2   2   1   2   2   1   2   2;
                1   1   1   0   0   0   1   1   1   1   2   2   2   1   1   2   16  2   2   1   2   2   1   2   2;
                0   0   0   1   1   1   1   1   1   1   1   1   1   1   1   2   2   16  2   2   1   1   0   2   2;
                0   0   0   1   1   1   1   1   1   1   1   1   1   1   1   2   2   2   16  2   1   1   0   2   2;
                0   0   0   1   1   1   2   2   1   1   2   2   2   1   1   1   1   2   2   16  1   1   0   1   1;
                2   0   0   0   0   0   0   0   1   1   1   1   1   1   1   2   2   1   1   1   16  2   1   2   2;
                2   0   0   0   0   0   0   0   1   1   1   1   1   1   1   2   2   1   1   1   2   16  1   2   2;
                1   0   0   0   0   0   0   0   0   0   0   0   0   1   1   1   1   0   0   0   1   1   16  1   1;
                1   0   0   1   1   1   1   1   1   1   1   1   1   1   1   2   2   2   2   1   2   2   1   16  2;
                1   0   0   1   1   1   1   1   1   1   1   1   1   1   1   2   2   2   2   1   2   2   1   2   16
            ]

            function transmitter_idx(cell::Integer, channel::Integer)
                idx = 0
                c = 1
                while c < cell
                    idx += nchannel[c]
                    c += 1
                end
                return idx + channel
            end

            # Test the helper function for discrepancies due to 1-based indexing.
            @test transmitter_idx(1, 1) == 1
            @test transmitter_idx(ncell, 1) == sum(nchannel)

            # Build the model.
            model = cpo_java_model()
            ntransmitter = transmitter_idx(ncell, 1);
            freq = cpo_java_intvararray(model, ntransmitter, 1, nfreq, "freq");

            for cell in 1:ncell
                for channel1 in 1:nchannel[cell]
                    for channel2 in (channel1 + 1):nchannel[cell]
                        diff = cpo_java_diff(model, freq[transmitter_idx(cell, channel1)], freq[transmitter_idx(cell, channel2)])
                        cstr = cpo_java_ge(model, cpo_java_abs(model, diff), 16)
                        cpo_java_add(model, cstr)
                    end
                end
            end

            for cell1 in 1:ncell
                for cell2 in (cell1 + 1):ncell
                    if dist[cell1, cell2] > 0
                        for channel1 in 1:nchannel[cell1]
                            for channel2 in 1:nchannel[cell2]
                                diff = cpo_java_diff(model, freq[transmitter_idx(cell1, channel1)], freq[transmitter_idx(cell2, channel2)])
                                cstr = cpo_java_ge(model, cpo_java_abs(model, diff), dist[cell1, cell2])
                                cpo_java_add(model, cstr)
                            end
                        end
                    end
                end
            end

            obj = cpo_java_countdifferent(model, freq)
            cpo_java_minimize(model, obj)

            status = cpo_java_solve(model)
            @test status
        end
    end
end
