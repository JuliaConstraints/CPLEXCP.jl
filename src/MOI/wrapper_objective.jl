# TODO: what about @objective(m, Max, count(x .== 1))? Automatically add a constraint (i.e. bridge)? And/or support the constraint as a function?
# TODO: -> rather all constraints, more consistent with how other solvers work.

function _update_objective(model::Optimizer)
    # If the sense is feasibility and there is an internal Concert objective, remove it.
    # Otherwise, this is an optimisation problem.
    if model.objective_sense == MOI.FEASIBILITY_SENSE &&
       model.objective_cp !== nothing
        cpo_java_remove(model.inner, model.objective_cp)
        model.objective_cp = nothing
    end

    # If only no function is available, don't do anything.
    if model.objective_function_cp === nothing
        return
    end

    # Set the new objective.
    obj = if model.objective_sense == MOI.MIN_SENSE
        cpo_java_minimize(model.inner, model.objective_function_cp)
    else
        cpo_java_maximize(model.inner, model.objective_function_cp)
    end
    cpo_java_add(model.inner, obj)
    return
end

function MOI.set(
    model::Optimizer,
    ::MOI.ObjectiveSense,
    sense::MOI.OptimizationSense,
)
    model.objective_sense = sense
    _update_objective(model)
    return
end

function MOI.get(model::Optimizer, ::MOI.ObjectiveSense)
    return model.objective_sense
end

function MOI.set(
    model::Optimizer,
    ::MOI.ObjectiveFunction{F},
    f::F,
) where {F <: MOI.SingleVariable}
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        convert(MOI.ScalarAffineFunction{Float64}, f),
    )
    model.objective_type = MOI.SINGLE_VARIABLE
    return
end

function MOI.get(
    model::Optimizer,
    ::MOI.ObjectiveFunction{F},
) where {F <: MOI.AbstractScalarFunction}
    if typeof(model.objective_function) <: F
        return model.objective_function
    else
        error(
            "Unable to get objective function. Current objective: $(model.objective_function).",
        )
    end
end

function MOI.set(
    model::Optimizer,
    ::MOI.ObjectiveFunction{F},
    f::F,
) where {F <: MOI.AbstractScalarFunction}
    model.objective_function = f
    model.objective_function_cp = _parse(model, f)
    _update_objective(model)
    return
end

# TODO: modifications. Easy to do, as we have a pointer on the Concert expression! Hard to do, as the MOI function must be rebuilt.
# function MOI.modify(
#     model::Optimizer,
#     ::MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}},
#     chg::MOI.ScalarConstantChange{Float64}
# )
#     CPLEX.c_api_chgobjoffset(model.inner, chg.new_constant)
#     return
# end
