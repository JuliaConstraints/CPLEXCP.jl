@testset "Java API" begin
    @testset "Unit tests" begin
        @testset "Type hierarchy" begin
            for T in [
                IloConstraint,
                IloAlternative,
                IloIsomorphism,
                IloNoOverlap,
                IloRange,
                IloSpan,
                IloSynchronize,
            ]
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
            model = cpo_java_model()
            @test typeof(model) == JavaCPOModel
        end

        @testset "Variable creation" begin
            model = cpo_java_model()

            # Integer variables
            @test typeof(cpo_java_intvar(model, Int32(5), Int32(10))) == IloIntVar
            @test typeof(cpo_java_intvar(model, Int32(10), Int32(5))) == IloIntVar # No check on CPLEXCP side
            @test typeof(cpo_java_intvar(model, Int32(5), Int32(10), "var")) == IloIntVar

            @test typeof(cpo_java_intvar(model, Int32[5, 10])) == IloIntVar
            @test typeof(cpo_java_intvar(model, Int32[5, 10], "var")) == IloIntVar

            l = cpo_java_intvararray(model, Int32(20), Int32(5), Int32(10))
            @test typeof(l) == Vector{IloIntVar}
            @test eltype(l) == IloIntVar
            @test length(l) == 20
            l = cpo_java_intvararray(model, Int32(20), Int32[5, 10])
            @test typeof(l) == Vector{IloIntVar}
            @test eltype(l) == IloIntVar
            @test length(l) == 20

            # Numerical variables
            @test typeof(cpo_java_numvar(model, 5.0, 10.0)) == IloNumVar

            l = cpo_java_numvararray(model, Int32(20), 5.0, 10.0)
            @test typeof(l) == Vector{IloNumVar}
            @test eltype(l) == IloNumVar
            @test length(l) == 20

            # Interval variables
            @test typeof(cpo_java_intervalvar(model)) == IloIntervalVar
            @test typeof(cpo_java_intervalvar(model, "var")) == IloIntervalVar
            @test typeof(cpo_java_intervalvar(model, Int32(5))) == IloIntervalVar
            @test typeof(cpo_java_intervalvar(model, Int32(5), "var")) ==
                  IloIntervalVar
            @test typeof(cpo_java_intervalvar(model, Int32(5), Int32(10))) == IloIntervalVar

            # Sequence-of-intervals variables
            i1 = cpo_java_intervalvar(model)
            i2 = cpo_java_intervalvar(model)
            @test typeof(cpo_java_intervalsequencevar(model, [i1, i2])) ==
                  IloIntervalSequenceVar
        end
    end

    @testset "Integration tests" begin
        # Based on the provided examples, translated from Java.

        @testset "Color" begin
            model = cpo_java_model()
            belgium = cpo_java_intvar(model, Int32(0), Int32(3))
            denmark = cpo_java_intvar(model, Int32(0), Int32(3))
            france = cpo_java_intvar(model, Int32(0), Int32(3))
            germany = cpo_java_intvar(model, Int32(0), Int32(3))
            luxembourg = cpo_java_intvar(model, Int32(0), Int32(3))
            netherlands = cpo_java_intvar(model, Int32(0), Int32(3))

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

            @test cpo_java_getvalue(model, belgium) ==
                  cpo_java_getvalue(model, belgium)
            @test cpo_java_getvalue(model, denmark) ==
                  cpo_java_getvalue(model, denmark)
            @test cpo_java_getvalue(model, france) ==
                  cpo_java_getvalue(model, france)
            @test cpo_java_getvalue(model, germany) ==
                  cpo_java_getvalue(model, germany)
            @test cpo_java_getvalue(model, luxembourg) ==
                  cpo_java_getvalue(model, luxembourg)
            @test cpo_java_getvalue(model, netherlands) ==
                  cpo_java_getvalue(model, netherlands)

            @test cpo_java_getvalue(model, belgium) !=
                  cpo_java_getvalue(model, france)
            @test cpo_java_getvalue(model, belgium) !=
                  cpo_java_getvalue(model, germany)
            @test cpo_java_getvalue(model, belgium) !=
                  cpo_java_getvalue(model, netherlands)
            @test cpo_java_getvalue(model, belgium) !=
                  cpo_java_getvalue(model, luxembourg)
            @test cpo_java_getvalue(model, denmark) !=
                  cpo_java_getvalue(model, germany)
            @test cpo_java_getvalue(model, france) !=
                  cpo_java_getvalue(model, germany)
            @test cpo_java_getvalue(model, france) !=
                  cpo_java_getvalue(model, luxembourg)
            @test cpo_java_getvalue(model, germany) !=
                  cpo_java_getvalue(model, luxembourg)
            @test cpo_java_getvalue(model, germany) !=
                  cpo_java_getvalue(model, netherlands)
        end

        @testset "Alloc" begin
            # Model data.
            ncell = Int32(25)
            nfreq = Int32(256)
            nchannel = Int32[
                8,
                6,
                6,
                1,
                4,
                4,
                8,
                8,
                8,
                8,
                4,
                9,
                8,
                4,
                4,
                10,
                8,
                9,
                8,
                4,
                5,
                4,
                8,
                1,
                1,
            ]
            dist = Int32[
                16 1 1 0 0 0 0 0 1 1 1 1 1 2 2 1 1 0 0 0 2 2 1 1 1
                1 16 2 0 0 0 0 0 2 2 1 1 1 2 2 1 1 0 0 0 0 0 0 0 0
                1 2 16 0 0 0 0 0 2 2 1 1 1 2 2 1 1 0 0 0 0 0 0 0 0
                0 0 0 16 2 2 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 1 1
                0 0 0 2 16 2 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 1 1
                0 0 0 2 2 16 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 1 1
                0 0 0 0 0 0 16 2 0 0 1 1 1 0 0 1 1 1 1 2 0 0 0 1 1
                0 0 0 0 0 0 2 16 0 0 1 1 1 0 0 1 1 1 1 2 0 0 0 1 1
                1 2 2 0 0 0 0 0 16 2 2 2 2 2 2 1 1 1 1 1 1 1 0 1 1
                1 2 2 0 0 0 0 0 2 16 2 2 2 2 2 1 1 1 1 1 1 1 0 1 1
                1 1 1 0 0 0 1 1 2 2 16 2 2 2 2 2 2 1 1 2 1 1 0 1 1
                1 1 1 0 0 0 1 1 2 2 2 16 2 2 2 2 2 1 1 2 1 1 0 1 1
                1 1 1 0 0 0 1 1 2 2 2 2 16 2 2 2 2 1 1 2 1 1 0 1 1
                2 2 2 0 0 0 0 0 2 2 2 2 2 16 2 1 1 1 1 1 1 1 1 1 1
                2 2 2 0 0 0 0 0 2 2 2 2 2 2 16 1 1 1 1 1 1 1 1 1 1
                1 1 1 0 0 0 1 1 1 1 2 2 2 1 1 16 2 2 2 1 2 2 1 2 2
                1 1 1 0 0 0 1 1 1 1 2 2 2 1 1 2 16 2 2 1 2 2 1 2 2
                0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 2 2 16 2 2 1 1 0 2 2
                0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 16 2 1 1 0 2 2
                0 0 0 1 1 1 2 2 1 1 2 2 2 1 1 1 1 2 2 16 1 1 0 1 1
                2 0 0 0 0 0 0 0 1 1 1 1 1 1 1 2 2 1 1 1 16 2 1 2 2
                2 0 0 0 0 0 0 0 1 1 1 1 1 1 1 2 2 1 1 1 2 16 1 2 2
                1 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 1 1 16 1 1
                1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 1 2 2 1 16 2
                1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 1 2 2 1 2 16
            ]

            function transmitter_idx(cell::Integer, channel::Integer)::Int32
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
            ntransmitter = transmitter_idx(ncell, 1)
            freq = cpo_java_intvararray(model, ntransmitter, Int32(1), nfreq, "freq")

            for cell in 1:ncell
                for channel1 in 1:nchannel[cell]
                    for channel2 in (channel1 + 1):nchannel[cell]
                        diff = cpo_java_diff(
                            model,
                            freq[transmitter_idx(cell, channel1)],
                            freq[transmitter_idx(cell, channel2)],
                        )
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
                                diff = cpo_java_diff(
                                    model,
                                    freq[transmitter_idx(cell1, channel1)],
                                    freq[transmitter_idx(cell2, channel2)],
                                )
                                cstr = cpo_java_ge(
                                    model,
                                    cpo_java_abs(model, diff),
                                    dist[cell1, cell2],
                                )
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

        @testset "Sports" begin
            # Model data.
            n = Int32(10)
            nweeks = Int32(2 * (n - 1))
            ngamesperweek = Int32(n / 2)
            ngames = n * (n - 1)

            function game(h::Int32, a::Int32, n::Int32)
                if a > h
                    return h * (n - 1) + a - 1
                else
                    return h * (n - 1) + a
                end
            end

            # Build the model.
            model = cpo_java_model()
            games = Dict{Tuple{Int, Int}, IloIntVar}() # nweeks, ngamesperweek
            home = Dict{Tuple{Int, Int}, IloIntVar}() # nweeks, ngamesperweek
            away = Dict{Tuple{Int, Int}, IloIntVar}() # nweeks, ngamesperweek

            for i in 0:(nweeks - 1)
                for j in 0:(ngamesperweek - 1)
                    home[i, j] = cpo_java_intvar(model, Int32(0), Int32(n - 1))
                    away[i, j] = cpo_java_intvar(model, Int32(0), Int32(n - 1))
                    games[i, j] = cpo_java_intvar(model, Int32(0), Int32(ngames - 1))
                end
            end

            # For each play slot, set up correspondance between game id, home team, and away team
            gha = cpo_java_inttable(model, Int32(3))
            for i in Int32(0):Int32(n - 1)
                for j in Int32(0):Int32(n - 1)
                    if i != j
                        cpo_java_inttupleset_addtuple(
                            model,
                            gha,
                            Int32[i, j, game(i, j, n)],
                        )
                    end
                end
            end

            for i in 0:(nweeks - 1)
                for j in 0:(ngamesperweek - 1)
                    vars = [home[i, j], away[i, j], games[i, j]]
                    cpo_java_add(
                        model,
                        cpo_java_allowedassignments(model, vars, gha),
                    )
                end
            end

            # All teams play each week
            for i in 0:(nweeks - 1)
                teamsthisweek = Dict{Int, IloIntVar}() # n
                for j in 0:(ngamesperweek - 1)
                    teamsthisweek[j] = home[i, j]
                    teamsthisweek[ngamesperweek + j] = away[i, j]
                end
                teamsthisweek_vec = [teamsthisweek[i] for i in 0:(n - 1)]
                cpo_java_add(model, cpo_java_alldiff(model, teamsthisweek_vec))
            end

            # Dual representation: for each game id, the play slot is maintained
            weekofgame = Dict{Int, IloIntVar}() # ngames
            allgames = Dict{Int, IloIntVar}() # ngames
            allslots = Dict{Int, IloIntVar}() # ngames

            for i in 0:(ngames - 1)
                weekofgame[i] = cpo_java_intvar(model, Int32(0), Int32(nweeks - 1))
                allslots[i] = cpo_java_intvar(model, Int32(0), Int32(ngames - 1))
            end

            for i in 0:(nweeks - 1)
                for j in 0:(ngamesperweek - 1)
                    allgames[i * ngamesperweek + j] = games[i, j]
                end
            end
            allgames_vec = [allgames[i] for i in 0:(ngames - 1)]
            allslots_vec = [allslots[i] for i in 0:(ngames - 1)]
            cpo_java_add(
                model,
                cpo_java_inverse(model, allgames_vec, allslots_vec),
            )

            for i in 0:(ngames - 1)
                cpo_java_add(
                    model,
                    cpo_java_eq(
                        model,
                        weekofgame[i],
                        cpo_java_div(model, allslots[i], ngamesperweek),
                    ),
                )
            end

            # Two half schedules.  Cannot play the same pair twice in the same half.
            # Plus, impose a minimum number of weeks between two games involving
            # the same teams (up to six weeks)
            mid = Int32(nweeks / 2)
            overlap = Int32(0)
            if n >= 6
                overlap = min(Int32(n / 2), Int32(6))
            end

            for i in Int32(0):Int32(n - 1)
                for j in Int32(i + 1):Int32(n - 1)
                    g1 = game(i, j, n)
                    g2 = game(j, i, n)

                    c = cpo_java_equiv(
                        model,
                        cpo_java_ge(model, weekofgame[g1], mid),
                        cpo_java_lt(model, weekofgame[g2], mid),
                    )
                    cpo_java_add(model, c)

                    if (overlap != 0)
                        c = cpo_java_ge(
                            model,
                            cpo_java_abs(
                                model,
                                cpo_java_diff(
                                    model,
                                    weekofgame[g1],
                                    weekofgame[g2],
                                ),
                            ),
                            overlap,
                        )
                        cpo_java_add(model, c)
                    end
                end
            end

            # Can't have three homes or three aways in a row.
            playhome = Dict{Tuple{Int, Int}, IloIntVar}() # n, nweeks
            for i in Int32(0):Int32(n - 1)
                for j in Int32(0):Int32(nweeks - 1)
                    playhome[i, j] = cpo_java_boolvar(model)
                    home_vec = [home[j, k] for k in 0:(ngamesperweek - 1)]
                    cpo_java_add(
                        model,
                        cpo_java_eq(
                            model,
                            playhome[i, j],
                            cpo_java_count(model, home_vec, i),
                        ),
                    )
                end

                for j in 0:(nweeks - 3 - 1)
                    windowsum =
                        cpo_java_sum(model, [playhome[i, k] for k in j:(j + 2)])
                    cpo_java_add(model, cpo_java_ge(model, windowsum, 1))
                    cpo_java_add(model, cpo_java_le(model, windowsum, 2))
                end
            end

            # If we start the season home, we finish away and vice versa.
            for i in 0:(n - 1)
                cpo_java_add(
                    model,
                    cpo_java_neq(
                        model,
                        playhome[i, 0],
                        playhome[i, nweeks - 1],
                    ),
                )
            end

            # Objective is skipped, not useful for tests. Finds a solution in 0.5s on a powerful machine,
            # no need for more complexity.

            # Each team plays home the same number of times as away
            for i in 0:(n - 1)
                playhomesum = cpo_java_sum(
                    model,
                    [playhome[i, j] for j in 0:(nweeks - 1)],
                )
                cpo_java_add(
                    model,
                    cpo_java_eq(model, playhomesum, Int(nweeks / 2)),
                )
            end

            # Teams are interchangeable. Fix first week. Also breaks reflection symmetry of the whole schedule.
            for i in 0:(ngamesperweek - 1)
                cpo_java_add(model, cpo_java_eq(model, home[0, i], i * 2))
                cpo_java_add(model, cpo_java_eq(model, away[0, i], i * 2 + 1))
            end

            # Order of games in each week is arbitrary. Break symmetry by forcing an order.
            for i in 0:(nweeks - 1)
                for j in 1:(ngamesperweek - 1)
                    cpo_java_add(
                        model,
                        cpo_java_gt(model, games[i, j], games[i, j - 1]),
                    )
                end
            end

            status = cpo_java_solve(model)
            @test status
        end

        @testset "Facility" begin
            # Model data.
            nlocations = 5
            nstores = 8
            capacity = Int32[3, 1, 2, 4, 1]
            fixedcost = Int32[480, 200, 320, 340, 300]
            cost = Int32[
                24 74 31 51 84
                57 54 86 61 68
                57 67 29 91 71
                54 54 65 82 94
                98 81 16 61 27
                13 92 34 94 87
                54 72 41 12 78
                54 64 65 89 89
            ]

            # Build the model.
            model = cpo_java_model()
            supplier = cpo_java_intvararray(model, Int32(nstores), Int32(0), Int32(nlocations - 1))
            open = cpo_java_boolvararray(model, Int32(nlocations))

            for i in 1:nstores
                cpo_java_add(
                    model,
                    cpo_java_eq(
                        model,
                        cpo_java_element(model, open, supplier[i]),
                        1,
                    ),
                )
            end

            for j in Int32(1):Int32(nlocations)
                cpo_java_add(
                    model,
                    cpo_java_le(
                        model,
                        cpo_java_count(model, supplier, j),
                        capacity[j],
                    ),
                )
            end

            obj = cpo_java_scalprod(model, fixedcost, open)
            for i in Int32(1):Int32(nstores)
                obj = cpo_java_sum(
                    model,
                    IntExpr[
                        obj,
                        cpo_java_element(model, cost[i, :], supplier[i]),
                    ],
                )
            end
            cpo_java_add(model, cpo_java_minimize(model, obj))

            status = cpo_java_solve(model)
            @test status
        end
    end
end
