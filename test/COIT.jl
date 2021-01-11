@testset "DifferentFrom" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Interval{Int})
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Int}, MOI.EqualTo{Int})
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Int}, CP.DifferentFrom{Int})

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
    
@testset "AllDifferent: VectorOfVariables" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Interval{Int})
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Int}, MOI.EqualTo{Int})
    @test MOI.supports_constraint(model, MOI.VectorOfVariables, CP.AllDifferent)

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(model, x1, MOI.Interval(1, 2))
    c2 = MOI.add_constraint(model, x2, MOI.Interval(1, 2))
    
    c3 = MOI.add_constraint(model, MOI.VectorOfVariables(MOI.VariableIndex[x1, x2]), CP.AllDifferent(2))
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

@testset "AllDifferent: VectorAffineFunction" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Interval{Int})
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Int}, MOI.EqualTo{Int})
    @test MOI.supports_constraint(model, MOI.VectorAffineFunction{Int}, CP.AllDifferent)

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
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Int}, MOI.EqualTo{Int})
    @test MOI.supports_constraint(model, MOI.VectorOfVariables, CP.AllDifferent)

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(model, MOI.SingleVariable(x1), CP.Domain(Set([1, 2])))
    c2 = MOI.add_constraint(model, MOI.SingleVariable(x2), CP.Domain(Set([1, 2])))
    
    c3 = MOI.add_constraint(model, MOI.VectorOfVariables(MOI.VariableIndex[x1, x2]), CP.AllDifferent(2))
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

@testset "Domain: ScalarAffineFunction" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Interval{Int})
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Int}, MOI.EqualTo{Int})
    @test MOI.supports_constraint(model, MOI.VectorAffineFunction{Int}, CP.AllDifferent)

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