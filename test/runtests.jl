using CPLEXCP
using JavaCall
using MathOptInterface
using ConstraintProgrammingExtensions

using Test

@testset "CPLEXCP" begin
    include("api_java.jl")
    include("MOI.jl")
end
