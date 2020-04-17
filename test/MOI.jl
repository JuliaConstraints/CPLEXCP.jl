const MOI  = MathOptInterface
const MOIT = MOI.Test
const MOIB = MOI.Bridges

const CONFIG = MOIT.TestConfig()

const OPTIMIZER = CPLEXCP.Optimizer()
MOI.set(OPTIMIZER, MOI.Silent(), true)
const BRIDGED_OPTIMIZER = MOI.Bridges.full_bridge_optimizer(OPTIMIZER, Float64)

const CERTIFICATE_OPTIMIZER = CPLEXCP.Optimizer()
MOI.set(CERTIFICATE_OPTIMIZER, MOI.Silent(), true)
MOI.set(CERTIFICATE_OPTIMIZER, MOI.RawParameter("CPX_PARAM_REDUCE"), 0)
MOI.set(CERTIFICATE_OPTIMIZER, MOI.RawParameter("CPX_PARAM_PRELINEAR"), 0)
const BRIDGED_CERTIFICATE_OPTIMIZER =
    MOI.Bridges.full_bridge_optimizer(CERTIFICATE_OPTIMIZER, Float64)

@testset "Unit Tests" begin
    MOIT.basic_constraint_tests(BRIDGED_OPTIMIZER, CONFIG; exclude = [
        (MOI.VectorOfVariables, MOI.SecondOrderCone),
        (MOI.VectorOfVariables, MOI.RotatedSecondOrderCone),
        (MOI.VectorOfVariables, MOI.GeometricMeanCone),
        (MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone),
        (MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone),
        (MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone),
        (MOI.VectorQuadraticFunction{Float64}, MOI.SecondOrderCone),
        (MOI.VectorQuadraticFunction{Float64}, MOI.RotatedSecondOrderCone),
        (MOI.VectorQuadraticFunction{Float64}, MOI.GeometricMeanCone),
        (MOI.VectorAffineFunction{Float64}, MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE, MOI.LessThan{Float64}}),
        (MOI.VectorAffineFunction{Float64}, MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE, MOI.GreaterThan{Float64}}),
    ])
    # TODO(odow): bugs deleting SOC variables. See also the
    # `delete_soc_variables` test.
    MOIT.basic_constraint_tests(
        BRIDGED_OPTIMIZER,
        CONFIG;
        include = [
            (MOI.VectorOfVariables, MOI.SecondOrderCone),
            (MOI.VectorOfVariables, MOI.RotatedSecondOrderCone),
            (MOI.VectorOfVariables, MOI.GeometricMeanCone),
            (MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone),
            (MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone),
            (MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone),
            (MOI.VectorQuadraticFunction{Float64}, MOI.SecondOrderCone),
            (MOI.VectorQuadraticFunction{Float64}, MOI.RotatedSecondOrderCone),
            (MOI.VectorQuadraticFunction{Float64}, MOI.GeometricMeanCone),
            (MOI.VectorAffineFunction{Float64}, MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE,MOI.LessThan{Float64}}),
            (MOI.VectorAffineFunction{Float64}, MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE,MOI.GreaterThan{Float64}}),
        ],
        delete = false
    )

    MOIT.unittest(BRIDGED_OPTIMIZER, CONFIG, [
        # TODO(odow): bug! We can't delete a vector of variables  if one is in
        # a second order cone.
        "delete_soc_variables",

        # CPLEX returns INFEASIBLE_OR_UNBOUNDED without extra parameters.
        # See below for the test.
        "solve_unbounded_model",
    ])
    MOIT.solve_unbounded_model(CERTIFICATE_OPTIMIZER, CONFIG)

    MOIT.modificationtest(BRIDGED_OPTIMIZER, CONFIG)
end

@testset "Linear tests" begin
    MOIT.contlineartest(BRIDGED_OPTIMIZER, CONFIG, [
        # These tests require extra parameters to be set.
        "linear8a", "linear8b", "linear8c",

        # TODO(odow): This test requests the infeasibility certificate of a
        # variable bound.
        "linear12"
    ])

    MOIT.linear8atest(CERTIFICATE_OPTIMIZER, CONFIG)
    MOIT.linear8btest(CERTIFICATE_OPTIMIZER, CONFIG)
    MOIT.linear8ctest(CERTIFICATE_OPTIMIZER, CONFIG)

    MOIT.linear12test(OPTIMIZER, MOIT.TestConfig(infeas_certificates=false))
end

@testset "Integer Linear tests" begin
    # interval somehow needed for indicator tests
    interval_optimizer = MOIB.LazyBridgeOptimizer(OPTIMIZER)
    MOIB.add_bridge(interval_optimizer, MOIB.Constraint.SplitIntervalBridge{Float64})
    MOIT.intlineartest(BRIDGED_OPTIMIZER, CONFIG)
    MOIT.intlineartest(interval_optimizer, CONFIG)
end

@testset "Quadratic tests" begin
    # TODO(odow): duals for quadratic problems.
    quad_config = MOIT.TestConfig(duals = false, atol = 1e-3, rtol = 1e-3)

    MOIT.contquadratictest(
        BRIDGED_CERTIFICATE_OPTIMIZER,
        quad_config, [
            # CPLEX doesn't support non-convex problems
            "ncqcp"
        ]
    )
end

@testset "Conic tests" begin
    MOIT.lintest(BRIDGED_OPTIMIZER, CONFIG, [
        # These tests require extra parameters to be set.
        "lin3", "lin4"
    ])

    MOIT.lin3test(BRIDGED_CERTIFICATE_OPTIMIZER, CONFIG)
    MOIT.lin4test(BRIDGED_CERTIFICATE_OPTIMIZER, CONFIG)

    # TODO(odow): duals for SOC constraints.
    soc_config = MOIT.TestConfig(duals = false, atol=5e-3)

    MOIT.soctest(BRIDGED_OPTIMIZER, soc_config, [
        "soc3"
    ])

    MOIT.soc3test(
        BRIDGED_OPTIMIZER,
        MOIT.TestConfig(duals = false, infeas_certificates = false, atol = 1e-3)
    )

    MOIT.rsoctest(BRIDGED_OPTIMIZER, soc_config)

    MOIT.geomeantest(BRIDGED_OPTIMIZER, soc_config)
end

@testset "ModelLike tests" begin
    @test MOI.get(BRIDGED_OPTIMIZER, MOI.SolverName()) == "CPLEX CP Optimizer"
    @testset "default_objective_test" begin
         MOIT.default_objective_test(BRIDGED_OPTIMIZER)
     end
     @testset "default_status_test" begin
         MOIT.default_status_test(BRIDGED_OPTIMIZER)
     end
    @testset "nametest" begin
        MOIT.nametest(BRIDGED_OPTIMIZER)
    end
    @testset "validtest" begin
        MOIT.validtest(BRIDGED_OPTIMIZER)
    end
    @testset "emptytest" begin
        MOIT.emptytest(BRIDGED_OPTIMIZER)
    end
    @testset "orderedindicestest" begin
        MOIT.orderedindicestest(BRIDGED_OPTIMIZER)
    end
    @testset "copytest" begin
        MOIT.copytest(
            BRIDGED_OPTIMIZER,
            MOI.Bridges.full_bridge_optimizer(CPLEXCP.Optimizer(), Float64)
        )
    end
    @testset "scalar_function_constant_not_zero" begin
        MOIT.scalar_function_constant_not_zero(OPTIMIZER)
    end

    @testset "start_values_test" begin
        model = CPLEXCP.Optimizer()
        x = MOI.add_variables(model, 2)
        @test MOI.supports(model, MOI.VariablePrimalStart(), MOI.VariableIndex)
        @test MOI.get(model, MOI.VariablePrimalStart(), x[1]) === nothing
        @test MOI.get(model, MOI.VariablePrimalStart(), x[2]) === nothing
        MOI.set(model, MOI.VariablePrimalStart(), x[1], 1.0)
        MOI.set(model, MOI.VariablePrimalStart(), x[2], nothing)
        @test MOI.get(model, MOI.VariablePrimalStart(), x[1]) == 1.0
        @test MOI.get(model, MOI.VariablePrimalStart(), x[2]) === nothing
        MOI.optimize!(model)
        @test MOI.get(model, MOI.ObjectiveValue()) == 0.0
    end

    @testset "supports_constrainttest" begin
        # supports_constrainttest needs VectorOfVariables-in-Zeros,
        # MOIT.supports_constrainttest(CPLEXCP.Optimizer(), Float64, Float32)
        # but supports_constrainttest is broken via bridges:
        MOI.empty!(BRIDGED_OPTIMIZER)
        MOI.add_variable(BRIDGED_OPTIMIZER)
        @test  MOI.supports_constraint(BRIDGED_OPTIMIZER, MOI.SingleVariable, MOI.EqualTo{Float64})
        @test  MOI.supports_constraint(BRIDGED_OPTIMIZER, MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})
        # This test is broken for some reason:
        @test_broken !MOI.supports_constraint(BRIDGED_OPTIMIZER, MOI.ScalarAffineFunction{Int}, MOI.EqualTo{Float64})
        @test !MOI.supports_constraint(BRIDGED_OPTIMIZER, MOI.ScalarAffineFunction{Int}, MOI.EqualTo{Int})
        @test !MOI.supports_constraint(BRIDGED_OPTIMIZER, MOI.SingleVariable, MOI.EqualTo{Int})
        @test  MOI.supports_constraint(BRIDGED_OPTIMIZER, MOI.VectorOfVariables, MOI.Zeros)
        @test !MOI.supports_constraint(BRIDGED_OPTIMIZER, MOI.VectorOfVariables, MOI.EqualTo{Float64})
        @test !MOI.supports_constraint(BRIDGED_OPTIMIZER, MOI.SingleVariable, MOI.Zeros)
        @test !MOI.supports_constraint(BRIDGED_OPTIMIZER, MOI.VectorOfVariables, MOIT.UnknownVectorSet)
    end

    @testset "set_lower_bound_twice" begin
        MOIT.set_lower_bound_twice(OPTIMIZER, Float64)
    end

    @testset "set_upper_bound_twice" begin
        MOIT.set_upper_bound_twice(OPTIMIZER, Float64)
    end
end

# @testset "Continuous -> Integer -> Continuous" begin
#     atol = 1e-5
#     rtol = 1e-5
#
#     model = CPLEX.Optimizer()
#     MOI.empty!(model)
#     @test MOI.is_empty(model)
#
#     # min -x
#     # st   x + y <= 1.5   (x + y - 1.5 ∈ Nonpositives)
#     #       x, y >= 0   (x, y ∈ Nonnegatives)
#
#     v = MOI.add_variables(model, 2)
#     @test MOI.get(model, MOI.NumberOfVariables()) == 2
#
#     cf = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0,1.0], v), 0.0)
#     c = MOI.add_constraint(model, cf, MOI.LessThan(1.5))
#     @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1
#
#     MOI.add_constraint.(model, MOI.SingleVariable.(v), MOI.GreaterThan(0.0))
#     @test MOI.get(model, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 2
#
#     objf = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([-1.0,0.0], v), 0.0)
#     MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), objf)
#     MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
#
#     @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MIN_SENSE
#     @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
#
#     MOI.optimize!(model)
#
#     @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
#     @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
#     @test MOI.get(model, MOI.ObjectiveValue()) ≈ -1.5 atol=atol rtol=rtol
#     @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [1.5, 0] atol=atol rtol=rtol
#     @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 1.5 atol=atol rtol=rtol
#
#     # Add integrality constraints
#     int = MOI.add_constraint.(model, MOI.SingleVariable.(v), MOI.Integer())
#     MOI.optimize!(model)
#     @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
#     @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
#     @test MOI.get(model, MOI.ObjectiveValue()) ≈ -1.0 atol=atol rtol=rtol
#     @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [1.0, 0] atol=atol rtol=rtol
#     @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 1.0 atol=atol rtol=rtol
#
#     # Remove integrality constraints
#     MOI.delete.(model, int)
#     MOI.optimize!(model)
#     @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
#     @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
#     @test MOI.get(model, MOI.ObjectiveValue()) ≈ -1.5 atol=atol rtol=rtol
#     @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [1.5, 0] atol=atol rtol=rtol
#     @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 1.5 atol=atol rtol=rtol
# end
