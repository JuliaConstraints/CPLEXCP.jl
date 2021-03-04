@testset "Reified: SingleVariable" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.EqualTo{Int})
    @test MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        CP.Reified{MOI.EqualTo{Int}},
    )

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(model, x2, MOI.EqualTo(1))
    c2 = MOI.add_constraint(
        model,
        _vov([x1, x2]),
        CP.Reified(MOI.EqualTo(2)),
    )

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

@testset "Reified: ScalarAffineFunction" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Int},
        MOI.EqualTo{Int},
    )
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Int},
        CP.Reified{MOI.EqualTo{Int}},
    )

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(model, _saf(x2), MOI.EqualTo(1))
    c2 = MOI.add_constraint(
        model,
        _vaf([x1, x2]),
        CP.Reified(MOI.EqualTo(2)),
    )

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

@testset "Equivalence: SingleVariable" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.EqualTo{Int})
    @test MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        CP.Equivalence{MOI.EqualTo{Int}, MOI.EqualTo{Int}},
    )

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(model, x1, MOI.EqualTo(1))
    c2 = MOI.add_constraint(
        model,
        _vov([x1, x2]),
        CP.Equivalence(MOI.EqualTo(1), MOI.EqualTo(1)),
    )

    @test MOI.is_valid(model, x1)
    @test MOI.is_valid(model, x2)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)

    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x1) == 1
    @test MOI.get(model, MOI.VariablePrimal(), x2) == 1
end

@testset "Equivalence: ScalarAffineFunction" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Int},
        MOI.EqualTo{Int},
    )
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Int},
        CP.Equivalence{MOI.EqualTo{Int}, MOI.EqualTo{Int}},
    )

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(model, _saf(x1), MOI.EqualTo(1))
    c2 = MOI.add_constraint(
        model,
        _vaf([x1, x2]),
        CP.Equivalence(MOI.EqualTo(1), MOI.EqualTo(1)),
    )

    @test MOI.is_valid(model, x1)
    @test MOI.is_valid(model, x2)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)

    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x1) == 1
    @test MOI.get(model, MOI.VariablePrimal(), x2) == 1
end

@testset "EquivalenceNot: SingleVariable" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.EqualTo{Int})
    @test MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        CP.EquivalenceNot{MOI.EqualTo{Int}, MOI.EqualTo{Int}},
    )

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(model, x1, MOI.EqualTo(1))
    c2 = MOI.add_constraint(
        model,
        _vov([x1, x2]),
        CP.EquivalenceNot(MOI.EqualTo(1), MOI.EqualTo(1)),
    )

    @test MOI.is_valid(model, x1)
    @test MOI.is_valid(model, x2)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)

    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x1) == 1
    @test MOI.get(model, MOI.VariablePrimal(), x2) != 1
end

@testset "EquivalenceNot: ScalarAffineFunction" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Int},
        MOI.EqualTo{Int},
    )
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Int},
        CP.EquivalenceNot{MOI.EqualTo{Int}, MOI.EqualTo{Int}},
    )

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(model, _saf(x1), MOI.EqualTo(1))
    c2 = MOI.add_constraint(
        model,
        _vaf([x1, x2]),
        CP.EquivalenceNot(MOI.EqualTo(1), MOI.EqualTo(1)),
    )

    @test MOI.is_valid(model, x1)
    @test MOI.is_valid(model, x2)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)

    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x1) == 1
    @test MOI.get(model, MOI.VariablePrimal(), x2) != 1
end

@testset "IfThenElse: SingleVariable" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.EqualTo{Int})
    @test MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        CP.IfThenElse{MOI.EqualTo{Int}, MOI.EqualTo{Int}, MOI.EqualTo{Int}},
    )

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x3, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(model, x1, MOI.EqualTo(1))
    c2 = MOI.add_constraint(
        model,
        _vov([x1, x2, x3]),
        CP.IfThenElse(MOI.EqualTo(1), MOI.EqualTo(1), MOI.EqualTo(0)),
    )

    @test MOI.is_valid(model, x1)
    @test MOI.is_valid(model, x2)
    @test MOI.is_valid(model, x3)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)

    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x1) == 1
    @test MOI.get(model, MOI.VariablePrimal(), x2) == 1
end

@testset "IfThenElse: ScalarAffineFunction" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Int},
        MOI.EqualTo{Int},
    )
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Int},
        CP.IfThenElse{MOI.EqualTo{Int}, MOI.EqualTo{Int}, MOI.EqualTo{Int}},
    )

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x3, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(model, _saf(x1), MOI.EqualTo(1))
    c2 = MOI.add_constraint(
        model,
        _vaf([x1, x2, x3]),
        CP.IfThenElse(MOI.EqualTo(1), MOI.EqualTo(1), MOI.EqualTo(0)),
    )

    @test MOI.is_valid(model, x1)
    @test MOI.is_valid(model, x2)
    @test MOI.is_valid(model, x3)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)

    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x1) == 1
    @test MOI.get(model, MOI.VariablePrimal(), x2) == 1
end

@testset "Imply: SingleVariable" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.EqualTo{Int})
    @test MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        CP.Imply{MOI.EqualTo{Int}, MOI.EqualTo{Int}},
    )

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(model, x1, MOI.EqualTo(1))
    c2 = MOI.add_constraint(
        model,
        _vov([x1, x2]),
        CP.Imply(MOI.EqualTo(1), MOI.EqualTo(1)),
    )

    @test MOI.is_valid(model, x1)
    @test MOI.is_valid(model, x2)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)

    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x1) == 1
    @test MOI.get(model, MOI.VariablePrimal(), x2) == 1
end

@testset "Imply: ScalarAffineFunction" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Int},
        MOI.EqualTo{Int},
    )
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Int},
        CP.Imply{MOI.EqualTo{Int}, MOI.EqualTo{Int}},
    )

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x2, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(model, _saf(x1), MOI.EqualTo(1))
    c2 = MOI.add_constraint(
        model,
        _vaf([x1, x2]),
        CP.Imply(MOI.EqualTo(1), MOI.EqualTo(1)),
    )

    @test MOI.is_valid(model, x1)
    @test MOI.is_valid(model, x2)
    @test MOI.is_valid(model, c1)
    @test MOI.is_valid(model, c2)

    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x1) == 1
    @test MOI.get(model, MOI.VariablePrimal(), x2) == 1
end

@testset "True" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.EqualTo{Int})
    @test MOI.supports_constraint(model, MOI.VectorOfVariables, CP.True)

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(
        model,
        _vov([x1, x1]),
        CP.IfThenElse(CP.True(), MOI.EqualTo(1), MOI.EqualTo(0)),
    )

    @test MOI.is_valid(model, x1)
    @test MOI.is_valid(model, c1)

    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x1) == 1
end

@testset "False" begin
    model = OPTIMIZER
    MOI.empty!(model)

    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.EqualTo{Int})
    @test MOI.supports_constraint(model, MOI.VectorOfVariables, CP.False)

    x1, _ = MOI.add_constrained_variable(model, MOI.Integer())

    c1 = MOI.add_constraint(
        model,
        _vov([x1, x1]),
        CP.IfThenElse(CP.False(), MOI.EqualTo(1), MOI.EqualTo(0)),
    )

    @test MOI.is_valid(model, x1)
    @test MOI.is_valid(model, c1)

    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT

    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.VariablePrimal(), x1) == 0
end
