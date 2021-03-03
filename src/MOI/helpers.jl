function _slice(f::MOI.VectorOfVariables, slice)
    f_sliced = MOIU.eachscalar(f)[slice]

    if MOI.output_dimension(f_sliced) == 1
        return MOI.SingleVariable(f_sliced.variables[1])
    else
        return f_sliced
    end
end

function _slice(f::MOI.VectorAffineFunction{T}, slice) where {T}
    f_sliced = MOIU.eachscalar(f)[slice]

    if MOI.output_dimension(f_sliced) == 1
        return collect(MOIU.eachscalar(equivalence_first_vector))[1]
    else
        return f_sliced
    end
end
