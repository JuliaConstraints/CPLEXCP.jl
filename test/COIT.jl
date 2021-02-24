@testset "AllDifferent: VectorOfVariables" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Interval{Int})
    @test MOI.supports_constraint(model, MOI.VectorOfVariables, CP.AllDifferent)
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.EqualTo{Int})

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(model, x1, MOI.EqualTo(1))
    c2 = MOI.add_constraint(model, x2, MOI.Interval(1, 2))
    
    c3 = MOI.add_constraint(model, MOI.VectorOfVariables(MOI.VariableIndex[x1, x2]), CP.AllDifferent(2))

    @test MOI.is_valid(model, x1)
    @test MOI.is_valid(model, x2)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)
    @test MOI.is_valid(model, c3)

    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x1) == 1
    @test MOI.get(model, MOI.VariablePrimal(), x2) == 2
end

@testset "AllDifferent: VectorAffineFunction" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Interval{Int})
    @test MOI.supports_constraint(model, MOI.VectorAffineFunction{Int}, CP.AllDifferent)
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Int}, MOI.EqualTo{Int})

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(model, x1, MOI.Interval(1, 2))
    c2 = MOI.add_constraint(model, x2, MOI.Interval(1, 2))
    
    c3 = MOI.add_constraint(model, MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1, 2], MOI.ScalarAffineTerm.([1, 1], [x1, x2])), [0, 0]), CP.AllDifferent(2))
    c4 = MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1], [x1]), 0), MOI.EqualTo(1))

    @test MOI.is_valid(model, x1)
    @test MOI.is_valid(model, x2)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)
    @test MOI.is_valid(model, c3)
    @test MOI.is_valid(model, c4)

    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x1) == 1
    @test MOI.get(model, MOI.VariablePrimal(), x2) == 2
end

@testset "Domain: SingleVariable" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Interval{Int})
    @test MOI.supports_constraint(model, MOI.SingleVariable, CP.Domain{Int})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.EqualTo{Int})

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(model, MOI.SingleVariable(x1), CP.Domain(Set([1, 2])))
    c2 = MOI.add_constraint(model, MOI.SingleVariable(x2), CP.Domain(Set([1, 2])))
    
    c3 = MOI.add_constraint(model, MOI.VectorOfVariables(MOI.VariableIndex[x1, x2]), CP.AllDifferent(2))
    c4 = MOI.add_constraint(model, x1, MOI.EqualTo(1))

    @test MOI.is_valid(model, x1)
    @test MOI.is_valid(model, x2)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)
    @test MOI.is_valid(model, c3)
    @test MOI.is_valid(model, c4)

    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x1) == 1
    @test MOI.get(model, MOI.VariablePrimal(), x2) == 2
end

@testset "Domain: ScalarAffineFunction" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Int}, CP.Domain{Int})
    @test MOI.supports_constraint(model, MOI.VectorAffineFunction{Int}, CP.AllDifferent)
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Int}, MOI.EqualTo{Int})

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1], [x1]), 0), CP.Domain(Set([1, 2])))
    c2 = MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1], [x2]), 0), CP.Domain(Set([1, 2])))
    
    c3 = MOI.add_constraint(model, MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1, 2], MOI.ScalarAffineTerm.([1, 1], [x1, x2])), [0, 0]), CP.AllDifferent(2))
    c4 = MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1], [x1]), 0), MOI.EqualTo(1))

    @test MOI.is_valid(model, x1)
    @test MOI.is_valid(model, x2)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)
    @test MOI.is_valid(model, c3)
    @test MOI.is_valid(model, c4)

    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x1) == 1
    @test MOI.get(model, MOI.VariablePrimal(), x2) == 2
end

@testset "DifferentFrom: SingleVariable" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Interval{Int})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.EqualTo{Int})
    @test MOI.supports_constraint(model, MOI.SingleVariable, CP.DifferentFrom{Int})

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(model, x1, MOI.Interval(1, 2))
    
    c2 = MOI.add_constraint(model, x1, CP.DifferentFrom(2))

    @test MOI.is_valid(model, x1)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)

    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x1) == 1
end

@testset "DifferentFrom: ScalarAffineFunction" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Interval{Int})
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Int}, CP.DifferentFrom{Int})
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Int}, MOI.EqualTo{Int})

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(model, x1, MOI.Interval(1, 2))
    c2 = MOI.add_constraint(model, x2, MOI.Interval(1, 2))
    
    c3 = MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1, -1], [x1, x2]), 0), CP.DifferentFrom(0))
    c4 = MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1], [x1]), 0), MOI.EqualTo(1))

    @test MOI.is_valid(model, x1)
    @test MOI.is_valid(model, x2)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)
    @test MOI.is_valid(model, c3)
    @test MOI.is_valid(model, c4)

    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x1) == 1
    @test MOI.get(model, MOI.VariablePrimal(), x2) == 2
end
    
@testset "Count: SingleVariable" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.EqualTo{Int})
    @test MOI.supports_constraint(model, MOI.VectorOfVariables, CP.Count{Int})

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x3, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x4, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(model, x1, MOI.EqualTo(1))
    c2 = MOI.add_constraint(model, x2, MOI.EqualTo(1))
    c3 = MOI.add_constraint(model, x3, MOI.EqualTo(2))
    
    c4 = MOI.add_constraint(model, MOI.VectorOfVariables(MOI.VariableIndex[x4, x1, x2, x3]), CP.Count(1, 3))

    @test MOI.is_valid(model, x1)
    @test MOI.is_valid(model, x2)
    @test MOI.is_valid(model, x3)
    @test MOI.is_valid(model, x4)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)
    @test MOI.is_valid(model, c3)
    @test MOI.is_valid(model, c4)

    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x1) == 1
    @test MOI.get(model, MOI.VariablePrimal(), x2) == 1
    @test MOI.get(model, MOI.VariablePrimal(), x3) == 2
    @test MOI.get(model, MOI.VariablePrimal(), x4) == 2
end

@testset "Count: ScalarAffineFunction" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Int}, MOI.EqualTo{Int})
    @test MOI.supports_constraint(model, MOI.VectorAffineFunction{Int}, CP.Count{Int})

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x3, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x4, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1], [x1]), 0), MOI.EqualTo(1))
    c2 = MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1], [x2]), 0), MOI.EqualTo(1))
    c3 = MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1], [x3]), 0), MOI.EqualTo(2))
    
    c4 = MOI.add_constraint(model, 
                            MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1, 2, 3, 4], MOI.ScalarAffineTerm.([1, 1, 1, 1], [x4, x1, x2, x3])), [0, 0, 0, 0]), 
                            CP.Count(1, 3))

    @test MOI.is_valid(model, x1)
    @test MOI.is_valid(model, x2)
    @test MOI.is_valid(model, x3)
    @test MOI.is_valid(model, x4)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)
    @test MOI.is_valid(model, c3)
    @test MOI.is_valid(model, c4)

    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x1) == 1
    @test MOI.get(model, MOI.VariablePrimal(), x2) == 1
    @test MOI.get(model, MOI.VariablePrimal(), x3) == 2
    @test MOI.get(model, MOI.VariablePrimal(), x4) == 2
end
    
@testset "CountDistinct: SingleVariable" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.EqualTo{Int})
    @test MOI.supports_constraint(model, MOI.VectorOfVariables, CP.CountDistinct)

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x3, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x4, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(model, x1, MOI.EqualTo(1))
    c2 = MOI.add_constraint(model, x2, MOI.EqualTo(1))
    c3 = MOI.add_constraint(model, x3, MOI.EqualTo(2))
    
    c4 = MOI.add_constraint(model, MOI.VectorOfVariables(MOI.VariableIndex[x4, x1, x2, x3]), CP.CountDistinct(3))

    @test MOI.is_valid(model, x1)
    @test MOI.is_valid(model, x2)
    @test MOI.is_valid(model, x3)
    @test MOI.is_valid(model, x4)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)
    @test MOI.is_valid(model, c3)
    @test MOI.is_valid(model, c4)

    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x1) == 1
    @test MOI.get(model, MOI.VariablePrimal(), x2) == 1
    @test MOI.get(model, MOI.VariablePrimal(), x3) == 2
    @test MOI.get(model, MOI.VariablePrimal(), x4) == 2
end

@testset "CountDistinct: ScalarAffineFunction" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Int}, MOI.EqualTo{Int})
    @test MOI.supports_constraint(model, MOI.VectorAffineFunction{Int}, CP.CountDistinct)

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x3, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x4, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1], [x1]), 0), MOI.EqualTo(1))
    c2 = MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1], [x2]), 0), MOI.EqualTo(1))
    c3 = MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1], [x3]), 0), MOI.EqualTo(2))
    
    c4 = MOI.add_constraint(model, 
                            MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1, 2, 3, 4], MOI.ScalarAffineTerm.([1, 1, 1, 1], [x4, x1, x2, x3])), [0, 0, 0, 0]), 
                            CP.CountDistinct(3))

    @test MOI.is_valid(model, x1)
    @test MOI.is_valid(model, x2)
    @test MOI.is_valid(model, x3)
    @test MOI.is_valid(model, x4)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)
    @test MOI.is_valid(model, c3)
    @test MOI.is_valid(model, c4)

    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x1) == 1
    @test MOI.get(model, MOI.VariablePrimal(), x2) == 1
    @test MOI.get(model, MOI.VariablePrimal(), x3) == 2
    @test MOI.get(model, MOI.VariablePrimal(), x4) == 2
end

@testset "Strictly{LessThan}" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(model, MOI.SingleVariable, CP.Domain{Int})
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Int}, CP.Strictly{Int, MOI.LessThan{Int}})

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(model, x1, CP.Domain(Set([1, 2])))
    c2 = MOI.add_constraint(model, x2, MOI.EqualTo(2))
    
    c3 = MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1, -1], [x1, x2]), 0), CP.Strictly(MOI.LessThan(0)))

    @test MOI.is_valid(model, x1)
    @test MOI.is_valid(model, x2)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)

    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x1) == 1
    @test MOI.get(model, MOI.VariablePrimal(), x2) == 2
end

@testset "Strictly{GreaterThan}" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(model, MOI.SingleVariable, CP.Domain{Int})
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Int}, CP.Strictly{Int, MOI.GreaterThan{Int}})

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(model, x1, CP.Domain(Set([1, 2])))
    c2 = MOI.add_constraint(model, x2, MOI.EqualTo(2))
    
    c3 = MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([-1, 1], [x1, x2]), 0), CP.Strictly(MOI.GreaterThan(0)))

    @test MOI.is_valid(model, x1)
    @test MOI.is_valid(model, x2)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)

    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x1) == 1
    @test MOI.get(model, MOI.VariablePrimal(), x2) == 2
end

@testset "Element: SingleVariable" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.EqualTo{Int})
    @test MOI.supports_constraint(model, MOI.VectorOfVariables, CP.Element{Int})

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(model, x1, MOI.EqualTo(1))
    
    c2 = MOI.add_constraint(model, MOI.VectorOfVariables(MOI.VariableIndex[x1, x2]), CP.Element([6, 5, 4]))

    @test MOI.is_valid(model, x1)
    @test MOI.is_valid(model, x2)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)

    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x1) == 1
    @test MOI.get(model, MOI.VariablePrimal(), x2) == 5
end

@testset "Element: ScalarAffineFunction" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Int}, MOI.EqualTo{Int})
    @test MOI.supports_constraint(model, MOI.VectorAffineFunction{Int}, CP.Element{Int})

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1], [x1]), 0), MOI.EqualTo(1))
    
    c2 = MOI.add_constraint(model, 
                            MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1, 2], MOI.ScalarAffineTerm.([1, 1], [x1, x2])), [0, 0]), 
                            CP.Element([6, 5, 4]))

    @test MOI.is_valid(model, x1)
    @test MOI.is_valid(model, x2)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)

    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x1) == 1
    @test MOI.get(model, MOI.VariablePrimal(), x2) == 5
end

@testset "BinPacking: ScalarAffineFunction" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Int}, MOI.EqualTo{Int})
    @test MOI.supports_constraint(model, MOI.VectorAffineFunction{Int}, CP.BinPacking{Int})

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x3, _ = MOI.add_constrained_variable(model, MOI.Integer())
    w1 = 2
    w2 = 2
    
    c1 = MOI.add_constraint(model, 
                            MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1, 2, 3], MOI.ScalarAffineTerm.([1, 1, 1], [x1, x2, x3])), [0, 0, 0]), 
                            CP.BinPacking(1, 2, [w1, w2]))

    @test MOI.is_valid(model, x1)
    @test MOI.is_valid(model, x2)
    @test MOI.is_valid(model, x3)
    @test MOI.is_valid(model, c1)

    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x1) == 4
    @test MOI.get(model, MOI.VariablePrimal(), x2) == 0
    @test MOI.get(model, MOI.VariablePrimal(), x3) == 0
end

@testset "BinPacking: VectorOfVariables" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Int}, MOI.EqualTo{Int})
    @test MOI.supports_constraint(model, MOI.VectorOfVariables, CP.BinPacking{Int})

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x3, _ = MOI.add_constrained_variable(model, MOI.Integer())
    w1 = 2
    w2 = 2
    
    c1 = MOI.add_constraint(model,
                            MOI.VectorOfVariables(MOI.VariableIndex[x1, x2, x3]), 
                            CP.BinPacking(1, 2, [w1, w2]))

    @test MOI.is_valid(model, x1)
    @test MOI.is_valid(model, x2)
    @test MOI.is_valid(model, x3)
    @test MOI.is_valid(model, c1)

    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x1) == 4
    @test MOI.get(model, MOI.VariablePrimal(), x2) == 0
    @test MOI.get(model, MOI.VariablePrimal(), x3) == 0
end

# @testset "BinPacking: ScalarAffineFunction with variable sizes" begin
#     model = OPTIMIZER
#     MOI.empty!(model)

#     @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
#     @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Int}, MOI.EqualTo{Int})
#     @test MOI.supports_constraint(model, MOI.VectorAffineFunction{Int}, CP.BinPacking)

#     x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
#     x2, _ = MOI.add_constrained_variable(model, MOI.Integer())
#     x3, _ = MOI.add_constrained_variable(model, MOI.Integer())
#     x4, _ = MOI.add_constrained_variable(model, MOI.Integer())
#     x5, _ = MOI.add_constrained_variable(model, MOI.Integer())

#     c1 = MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1], [x4]), 0), MOI.EqualTo(2))
#     c1 = MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1], [x5]), 0), MOI.EqualTo(2))
    
# TODO: currently, throws an assertion. https://github.com/dourouc05/ConstraintProgrammingExtensions.jl/issues/4
#     @test_throws(
#         MOI.AddConstraintNotAllowed{MOI.VectorAffineFunction{Int64}, CP.BinPacking},
#         MOI.add_constraint(model, 
#                            MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1, 2, 3, 4, 5], MOI.ScalarAffineTerm.([1, 1, 1, 1, 1], [x1, x2, x3, x4, x5])), [0, 0, 0, 0, 0]), 
#                            CP.BinPacking(1, 2))
#     )
# end

@testset "ReificationSet: SingleVariable" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.EqualTo{Int})
    @test MOI.supports_constraint(model, MOI.VectorOfVariables, CP.ReificationSet{MOI.EqualTo{Int}})

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(model, MOI.SingleVariable(x2), MOI.EqualTo(1))
    c2 = MOI.add_constraint(model, MOI.VectorOfVariables(MOI.VariableIndex[x1, x2]), CP.ReificationSet(MOI.EqualTo(2)))

    @test MOI.is_valid(model, x1)
    @test MOI.is_valid(model, x2)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)

    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x1) == 0
    @test MOI.get(model, MOI.VariablePrimal(), x2) == 1
end

@testset "ReificationSet: ScalarAffineFunction" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Int}, CP.Domain{Int})
    @test MOI.supports_constraint(model, MOI.VectorAffineFunction{Int}, CP.AllDifferent)
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Int}, MOI.EqualTo{Int})

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1], [x1]), 0), MOI.EqualTo(1))
    c2 = MOI.add_constraint(model, MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1, 2], MOI.ScalarAffineTerm.([1, 1], [x1, x2])), [0, 0]), CP.ReificationSet(MOI.EqualTo(2)))

    @test MOI.is_valid(model, x1)
    @test MOI.is_valid(model, x2)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)

    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x1) == 0
    @test MOI.get(model, MOI.VariablePrimal(), x2) == 1
end