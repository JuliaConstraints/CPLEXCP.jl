module CPLEXCP

# TODO: 
# - bridge for Circuit: 
#     https://www.ibm.com/support/knowledgecenter/SSSA5P_12.5.1/ilog.odms.studio.help/OPL_Studio/oplmigration/topics/opl_mig_prev_3x4x_3xCP_constr_circuit.html
#     https://lost-contact.mit.edu/afs/pdc.kth.se/roots/ilse/v0.7/pdc/vol/cplex/12.5/amd64_co5/doc/html/en-US/OPL_Studio/oplmigration/topics/opl_mig_prev_3x4x_3xCP_constr_circuit.html
# - bridge for Cardinality/Sequence: 
#     https://www.ibm.com/support/knowledgecenter/SSSA5P_12.5.1/ilog.odms.studio.help/OPL_Studio/oplmigration/topics/opl_mig_prev_3x4x_3xCP_constr_seq.html
#     https://lost-contact.mit.edu/afs/pdc.kth.se/roots/ilse/v0.7/pdc/vol/cplex/12.5/amd64_co5/doc/html/en-US/OPL_Studio/oplmigration/topics/opl_mig_prev_3x4x_3xCP_constr_seq.html
#     (https://sofdem.github.io/gccat/gccat/Cglobal_cardinality.html)
#     (https://sofdem.github.io/gccat/gccat/Csliding_distribution.html)
# Help for debugging, with `model` an MOI model: 
#   cpo_java_exportmodel(model.inner, "somewhere/model.cpo")
#   -> cpoptimizer, r to read this file, opt to solve, etc.

using JavaCall
import MathOptInterface
import ConstraintProgrammingExtensions

const MOI = MathOptInterface
const MOIU = MOI.Utilities
const CleverDicts = MOIU.CleverDicts
const CP = ConstraintProgrammingExtensions

# Check if the package has been built correctly.
if isfile(joinpath(dirname(@__FILE__), "..", "deps", "deps.jl"))
    include("../deps/deps.jl")
else
    error(
        "CPLEXCP not properly installed. Please run `Pkg.build(\"CPLEXCP\")` or `]build CPLEXCP`",
    )
end

if !@isdefined(libcplexcpojava)
    error(
        "CPLEXCP not properly built. There probably was a problem when running `Pkg.build(\"CPLEXCP\")` or `]build CPLEXCP`",
    )
end

# Initialise the package by setting the right parameters for Java. This assumes
# no other code uses JavaCall...
function __init__()
    if get(ENV, "JULIA_REGISTRYCI_AUTOMERGE", "") != "true"
        cpo_java_init()
    end
end

# Export the Java API.
export cpo_java_init,
    JavaCPOModel,
    IloCP,
    IloIntVar,
    IloIntervalVar,
    IloIntervalSequenceVar,
    IloNumVar,
    IloIntExpr,
    IloNumExpr,
    IloIntTupleSet,
    IloNumToNumSegmentFunction,
    IloNumToNumStepFunction,
    IloCumulFunctionExpr,
    IloTransitionDistance,
    IloStateFunction,
    IloConstraint,
    IloAlternative,
    IloIsomorphism,
    IloNoOverlap,
    IloRange,
    IloSpan,
    IloSynchronize,
    IloObjective,
    IloMultiCriterionExpr,
    IloSolution,
    IloAddable,
    Callback,
    ConflictStatus,
    Constraint,
    IntExpr,
    NumVar,
    NumExpr,
    Addable,
    Variable,
    ConstraintArray,
    IntExprArray,
    NumVarArray,
    NumExprArray,
    AddableArray,
    IloInfinity,
    IloMaxInt,
    IloMinInt,
    cpo_java_release,
    cpo_java_boolvar,
    cpo_java_boolvararray,
    cpo_java_intvar,
    cpo_java_intvararray,
    cpo_java_numvar,
    cpo_java_numvararray,
    cpo_java_intervalvar,
    cpo_java_intervalsequencevar,
    cpo_java_numvar_getlb,
    cpo_java_numvar_getub,
    cpo_java_numvar_setlb,
    cpo_java_numvar_setub,
    cpo_java_intvar_getlb,
    cpo_java_intvar_getub,
    cpo_java_intvar_setlb,
    cpo_java_intvar_setub,
    cpo_java_abs,
    cpo_java_constant,
    cpo_java_count,
    cpo_java_countdifferent,
    cpo_java_div,
    cpo_java_element,
    cpo_java_endof,
    cpo_java_enfofnext,
    cpo_java_enfofprevious,
    cpo_java_exponent,
    cpo_java_heightatend,
    cpo_java_heightatstart,
    cpo_java_intexpr,
    cpo_java_intexprarray,
    cpo_java_lengthof,
    cpo_java_lengthofnext,
    cpo_java_lengthofprevious,
    cpo_java_log,
    cpo_java_max,
    cpo_java_min,
    cpo_java_modulo,
    cpo_java_negative,
    cpo_java_numexprarray,
    cpo_java_numvararray,
    cpo_java_overlaplength,
    cpo_java_piecewiselinear,
    cpo_java_power,
    cpo_java_prod,
    cpo_java_pulse,
    cpo_java_quot,
    cpo_java_scalprod,
    cpo_java_sizeeval,
    cpo_java_sizeof,
    cpo_java_sizeofnext,
    cpo_java_sizeofprevious,
    cpo_java_square,
    cpo_java_standarddeviation,
    cpo_java_starteval,
    cpo_java_startof,
    cpo_java_startofnext,
    cpo_java_startofprevious,
    cpo_java_sum,
    cpo_java_typeofnext,
    cpo_java_typeofprevious,
    cpo_java_linearintexpr,
    cpo_java_linearnumexpr,
    cpo_java_linearintexpr_add,
    cpo_java_linearintexpr_addterm,
    cpo_java_linearintexpr_addterms,
    cpo_java_linearintexpr_clear,
    cpo_java_linearintexpr_getconstant,
    cpo_java_linearintexpr_remove,
    cpo_java_linearintexpr_setconstant,
    cpo_java_linearnumexpr_add,
    cpo_java_linearnumexpr_addterm,
    cpo_java_linearnumexpr_addterms,
    cpo_java_linearnumexpr_clear,
    cpo_java_linearnumexpr_getconstant,
    cpo_java_linearnumexpr_remove,
    cpo_java_linearnumexpr_setconstant,
    cpo_java_inttable,
    cpo_java_inttupleset_addtuple,
    cpo_java_inttupleset_getarity,
    cpo_java_numtonumsegmentfunction,
    cpo_java_piecewiselinearfunction,
    cpo_java_numtonumsegmentfunction_add,
    cpo_java_numtonumsegmentfunction_addvalue,
    cpo_java_numtonumsegmentfunction_copy,
    cpo_java_numtonumsegmentfunction_dilate,
    cpo_java_numtonumsegmentfunction_getarea,
    cpo_java_numtonumsegmentfunction_getdefinitionintervalmax,
    cpo_java_numtonumsegmentfunction_getdefinitionintervalmin,
    cpo_java_numtonumsegmentfunction_getmax,
    cpo_java_numtonumsegmentfunction_getmin,
    cpo_java_numtonumsegmentfunction_getvalue,
    cpo_java_numtonumsegmentfunction_prod,
    cpo_java_numtonumsegmentfunction_setmax,
    cpo_java_numtonumsegmentfunction_setmax,
    cpo_java_numtonumsegmentfunction_setmin,
    cpo_java_numtonumsegmentfunction_setperiodic,
    cpo_java_numtonumsegmentfunction_setperiodicvalue,
    cpo_java_numtonumsegmentfunction_setslope,
    cpo_java_numtonumsegmentfunction_setvalue,
    cpo_java_numtonumsegmentfunction_shift,
    cpo_java_numtonumsegmentfunction_sub,
    cpo_java_numtonumstepfunction,
    cpo_java_numtonumstepfunction_add,
    cpo_java_numtonumstepfunction_addvalue,
    cpo_java_numtonumstepfunction_copy,
    cpo_java_numtonumstepfunction_dilate,
    cpo_java_numtonumstepfunction_getarea,
    cpo_java_numtonumstepfunction_getdefinitionintervalmax,
    cpo_java_numtonumstepfunction_getdefinitionintervalmin,
    cpo_java_numtonumstepfunction_getmax,
    cpo_java_numtonumstepfunction_getmin,
    cpo_java_numtonumstepfunction_getvalue,
    cpo_java_numtonumstepfunction_prod,
    cpo_java_numtonumstepfunction_setmax,
    cpo_java_numtonumstepfunction_setmin,
    cpo_java_numtonumstepfunction_setperiodic,
    cpo_java_numtonumstepfunction_setperiodicvalue,
    cpo_java_numtonumstepfunction_setvalue,
    cpo_java_numtonumstepfunction_shift,
    cpo_java_numtonumstepfunction_sub,
    cpo_java_statefunction,
    cpo_java_cumulfunctionexpr,
    cpo_java_diff,
    cpo_java_getnumberofsegments,
    cpo_java_getsegmentstart,
    cpo_java_getsegmentend,
    cpo_java_getsegmentvalue,
    cpo_java_getvalue_cumulfunctionexpr,
    cpo_java_step,
    cpo_java_stepatend,
    cpo_java_stepatend,
    cpo_java_stepatstart,
    cpo_java_stepatstart,
    cpo_java_sum_cumulfunctionexpr,
    cpo_java_transitiondistance,
    cpo_java_transitiondistance_getsize,
    cpo_java_transitiondistance_getvalue,
    cpo_java_transitiondistance_setvalue,
    cpo_java_add,
    cpo_java_alldiff,
    cpo_java_allmindistance,
    cpo_java_allowedassignments,
    cpo_java_alternative,
    cpo_java_alternative,
    cpo_java_alwaysconstant,
    cpo_java_alwaysequal,
    cpo_java_alwaysin,
    cpo_java_alwaysnostate,
    cpo_java_and,
    cpo_java_before,
    cpo_java_distribute,
    cpo_java_elementm,
    cpo_java_endatend,
    cpo_java_endatstart,
    cpo_java_endbeforeend,
    cpo_java_endbeforestart,
    cpo_java_eq,
    cpo_java_equiv,
    cpo_java_falseconstraint,
    cpo_java_first,
    cpo_java_forbiddenassignments,
    cpo_java_forbidend,
    cpo_java_forbidextent,
    cpo_java_forbidstart,
    cpo_java_ge,
    cpo_java_gt,
    cpo_java_ifthen,
    cpo_java_ifthenelse,
    cpo_java_imply,
    cpo_java_inverse,
    cpo_java_isomorphism,
    cpo_java_last,
    cpo_java_le,
    cpo_java_lexicographic,
    cpo_java_lt,
    cpo_java_neq,
    cpo_java_not,
    cpo_java_nooverlap,
    cpo_java_or,
    cpo_java_pack,
    cpo_java_presenceof,
    cpo_java_previous,
    cpo_java_range,
    cpo_java_samecommonsubsequence,
    cpo_java_samesubsequence,
    cpo_java_sequence,
    cpo_java_span,
    cpo_java_startatend,
    cpo_java_startatstart,
    cpo_java_startbeforeend,
    cpo_java_startbeforestart,
    cpo_java_strong,
    cpo_java_subcircuit,
    cpo_java_synchronize,
    cpo_java_trueconstraint,
    cpo_java_and_add,
    cpo_java_and_remove,
    cpo_java_or_add,
    cpo_java_or_remove,
    cpo_java_range_clearexpr,
    cpo_java_range_getexpr,
    cpo_java_range_getlb,
    cpo_java_range_getub,
    cpo_java_range_setbounds,
    cpo_java_range_setexpr,
    cpo_java_range_setlb,
    cpo_java_range_setub,
    cpo_java_addable_getname,
    cpo_java_addable_setname,
    cpo_java_maximize,
    cpo_java_maximize_multicriterion,
    cpo_java_minimize,
    cpo_java_minimize_multicriterion,
    cpo_java_staticlex,
    cpo_java_addeq,
    cpo_java_addge,
    cpo_java_addle,
    cpo_java_addmaximize,
    cpo_java_addminimize,
    cpo_java_addrange,
    cpo_java_getallconstrainedilocumulfunctionexprs,
    cpo_java_getallilointervalsequencevars,
    cpo_java_getallintervalvars,
    cpo_java_getallintvars,
    cpo_java_getallstatefunctions,
    cpo_java_getallkpinames,
    cpo_java_getdomain,
    cpo_java_getdomainsize,
    cpo_java_getend,
    cpo_java_getendmax,
    cpo_java_getendmin,
    cpo_java_getfirst,
    cpo_java_getincumbentvalue,
    cpo_java_getintvalue,
    cpo_java_getlast,
    cpo_java_getlength,
    cpo_java_getlengthmax,
    cpo_java_getlengthmin,
    cpo_java_getmax,
    cpo_java_getmin,
    cpo_java_getname,
    cpo_java_getnext,
    cpo_java_getobjbound,
    cpo_java_getobjbounds,
    cpo_java_getobjgap,
    cpo_java_getobjgaps,
    cpo_java_getobjvalue,
    cpo_java_getobjvalues,
    cpo_java_getprev,
    cpo_java_getsize,
    cpo_java_getsizemax,
    cpo_java_getsizemin,
    cpo_java_getstart,
    cpo_java_getstartmax,
    cpo_java_getstartmin,
    cpo_java_getvalue,
    cpo_java_getversion,
    cpo_java_hasobjective,
    cpo_java_isabsent,
    cpo_java_isfixed,
    cpo_java_isindomain,
    cpo_java_ispresent,
    cpo_java_next,
    cpo_java_propagate,
    cpo_java_refineconflict,
    cpo_java_restore,
    cpo_java_solve,
    cpo_java_startnewsearch,
    cpo_java_store,
    cpo_java_solution,
    cpo_java_solution_add,
    cpo_java_solution_contains,
    cpo_java_solution_end,
    cpo_java_solution_getend,
    cpo_java_solution_getendmin,
    cpo_java_solution_getlength,
    cpo_java_solution_getlengthmax,
    cpo_java_solution_getlengthmin,
    cpo_java_solution_getmax,
    cpo_java_solution_getmin,
    cpo_java_solution_getsize,
    cpo_java_solution_getsizemax,
    cpo_java_solution_getsizemin,
    cpo_java_solution_getstart,
    cpo_java_solution_getstartmax,
    cpo_java_solution_getstartmin,
    cpo_java_solution_getvalue,
    cpo_java_solution_isabsent,
    cpo_java_solution_isfixed,
    cpo_java_solution_isindomain,
    cpo_java_solution_ispresent,
    cpo_java_solution_remove_intervalvar,
    cpo_java_solution_remove_intervalvararray,
    cpo_java_solution_remove_intvar,
    cpo_java_solution_remove_intvararray,
    cpo_java_solution_setabsent,
    cpo_java_solution_setdomain,
    cpo_java_solution_setend,
    cpo_java_solution_setendmax,
    cpo_java_solution_setendmin,
    cpo_java_solution_setlength,
    cpo_java_solution_setlengthmax,
    cpo_java_solution_setlengthmin,
    cpo_java_solution_setmax,
    cpo_java_solution_setmin,
    cpo_java_solution_setoptional,
    cpo_java_solution_setpresent,
    cpo_java_solution_setsize,
    cpo_java_solution_setsizemax,
    cpo_java_solution_setsizemin,
    cpo_java_solution_setstart,
    cpo_java_solution_setstartmax,
    cpo_java_solution_setstartmin,
    cpo_java_solution_setvalue,
    cpo_java_solution_store,
    cpo_java_dumpmodel,
    cpo_java_exportmodel,
    cpo_java_getbuildid,
    cpo_java_getconflict_constraint,
    cpo_java_getconflict_intervalval,
    cpo_java_getconflict_numvar,
    cpo_java_importmodel,
    cpo_java_printinformation,
    cpo_java_remove,
    cpo_java_removeallcallbacks,
    cpo_java_removeallkpis,
    cpo_java_removecallback,
    cpo_java_removekpi,
    cpo_java_runseeds,
    cpo_java_getintinfo,
    cpo_java_getdoubleinfo,
    cpo_java_getparametervalue,
    cpo_java_getdoubleparameter,
    cpo_java_getdoubledefaultparameter,
    cpo_java_getintparameter,
    cpo_java_getintdefaultparameter,
    cpo_java_setdoubleparameter,
    cpo_java_setintparameter

# Finally, the code.
include("api_java.jl")
include("MOI/wrapper.jl")
include("MOI/parse.jl")
include("MOI/helpers.jl")
include("MOI/wrapper_variables.jl")
include("MOI/wrapper_objective.jl")
include("MOI/wrapper_constraints.jl")
include("MOI/wrapper_constraints_singlevar.jl")
include("MOI/wrapper_constraints_names.jl")
include("MOI/wrapper_constraints_mo.jl")
include("MOI/wrapper_constraints_cp.jl")
include("MOI/wrapper_constraints_cp_reification.jl")

end # module
