@testset "Java API" begin
    @testset "Unit tests" begin
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

        @testset "Color" begin
            model = cpo_java_model()
            belgium = cpo_java_intvar(model, 0, 3)
            denmark = cpo_java_intvar(model, 0, 3)
            france = cpo_java_intvar(model, 0, 3)
            germany = cpo_java_intvar(model, 0, 3)
            luxembourg = cpo_java_intvar(model, 0, 3)
            netherlands = cpo_java_intvar(model, 0, 3)

            cpo_java_add(model, cpo_java_neq_intexpr(model, belgium, france))
            cpo_java_add(model, cpo_java_neq_intexpr(model, belgium, germany))
            cpo_java_add(model, cpo_java_neq_intexpr(model, belgium, netherlands))
            cpo_java_add(model, cpo_java_neq_intexpr(model, belgium, luxembourg))
            cpo_java_add(model, cpo_java_neq_intexpr(model, denmark, germany))
            cpo_java_add(model, cpo_java_neq_intexpr(model, france, germany))
            cpo_java_add(model, cpo_java_neq_intexpr(model, france, luxembourg))
            cpo_java_add(model, cpo_java_neq_intexpr(model, germany, luxembourg))
            cpo_java_add(model, cpo_java_neq_intexpr(model, germany, netherlands))

            status = cpo_java_solve(model)
            @test status

            @test cpo_java_getvalue_intvar(model, belgium) == cpo_java_getvalue_intexpr(model, belgium)
            @test cpo_java_getvalue_intvar(model, denmark) == cpo_java_getvalue_intexpr(model, denmark)
            @test cpo_java_getvalue_intvar(model, france) == cpo_java_getvalue_intexpr(model, france)
            @test cpo_java_getvalue_intvar(model, germany) == cpo_java_getvalue_intexpr(model, germany)
            @test cpo_java_getvalue_intvar(model, luxembourg) == cpo_java_getvalue_intexpr(model, luxembourg)
            @test cpo_java_getvalue_intvar(model, netherlands) == cpo_java_getvalue_intexpr(model, netherlands)

            @test cpo_java_getvalue_intvar(model, belgium) != cpo_java_getvalue_intvar(model, france)
            @test cpo_java_getvalue_intvar(model, belgium) != cpo_java_getvalue_intvar(model, germany)
            @test cpo_java_getvalue_intvar(model, belgium) != cpo_java_getvalue_intvar(model, netherlands)
            @test cpo_java_getvalue_intvar(model, belgium) != cpo_java_getvalue_intvar(model, luxembourg)
            @test cpo_java_getvalue_intvar(model, denmark) != cpo_java_getvalue_intvar(model, germany)
            @test cpo_java_getvalue_intvar(model, france) != cpo_java_getvalue_intvar(model, germany)
            @test cpo_java_getvalue_intvar(model, france) != cpo_java_getvalue_intvar(model, luxembourg)
            @test cpo_java_getvalue_intvar(model, germany) != cpo_java_getvalue_intvar(model, luxembourg)
            @test cpo_java_getvalue_intvar(model, germany) != cpo_java_getvalue_intvar(model, netherlands)
        end

        @testset "Alloc" begin
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

            # Build the model.
            model = cpo_java_model()
            ntransmitter = transmitter_idx(ncell, 0);
            freq = cpo_java_intvararray(model, ntransmitter, 0, nfreq - 1, "freq");

            for cell in 1:ncell
                for channel1 in 1:nchannel[cell]
                    for channel2 in (channel1 + 1):nchannel[cell]
                        diff = cpo_java_diff_int(model, freq[transmitter_idx(cell, channel1)], freq[transmitter_idx(cell, channel2)])
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
                                diff = cpo_java_diff_int(model, freq[transmitter_idx(cell1, channel1)], freq[transmitter_idx(cell1, channel2)])
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
