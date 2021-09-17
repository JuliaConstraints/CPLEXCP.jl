const CONFIG = MOIT.Config(Int)

const OPTIMIZER = CPLEXCP.Optimizer()
MOI.set(OPTIMIZER, MOI.Silent(), true)
const BRIDGED_OPTIMIZER = MOI.Bridges.full_bridge_optimizer(OPTIMIZER, Float64)

COIT.runtests(
    BRIDGED_OPTIMIZER,
    CONFIG
)
