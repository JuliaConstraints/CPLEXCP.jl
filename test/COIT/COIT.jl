@testset "COIT" begin
    include("helpers.jl")

    @testset "Mathematical-programming sets" begin
        # TODO.
    end

    @testset "Constraint-programming sets" begin
        include("sets_cp.jl")
        include("sets_cp_reification.jl")
    end
end
