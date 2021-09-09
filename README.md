# CPLEXCP.jl

CPLEXCP.jl is a wrapper for the [IBM® ILOG® CPLEX® CP Optimizer](https://www.ibm.com/analytics/cplex-cp-optimizer)

You cannot use CPLEXCP.jl without having purchased and installed a copy of CPLEX
Optimization Studio from [IBM](http://www.ibm.com/). However, CPLEX is
available for free to [academics and students](http://ibm.biz/Bdzvqw).

CPLEXCP.jl has two components:
 - a thin wrapper around the complete Java API
 - an interface to [MathOptInterface](https://github.com/jump-dev/MathOptInterface.jl) and 
   [ConstraintProgrammingExtensions.jl](https://github.com/dourouc05/ConstraintProgrammingExtensions.jl)

The Java API can be accessed via `CPLEXCP.cpo_java_xxx` functions, where the names and
arguments are built from the Java API. See the [CPLEX documentation](https://www.ibm.com/support/knowledgecenter/SSSA5P_12.10.0/COS_KC_home.html)
for details.

*Note: This wrapper is maintained by the community and is not
officially supported by IBM. If you are a commercial customer
interested in official support for CPLEX CP Optimizer in Julia, let them know!.*

## Installation

**Minimum version requirement:** CPLEXCP.jl requires CPLEX version 20.1. Other versions may work, 
but are not tested. Versions 12.9 and 12.10 are automatically detected, though.

First, obtain a license of CPLEX and install CPLEX CP optimizer, following the
instructions on [IBM's website](https://www.ibm.com/analytics/cplex-cp-optimizer). Then, set the
`CPLEX_STUDIO_BINARIES` environment variable as appropriate and run
`Pkg.add("CPLEXCP")`, then `Pkg.build("CPLEXCP")`. For example:

```julia
# On Windows, this might be
ENV["CPLEX_STUDIO_BINARIES"] = "C:\\Program Files\\CPLEX_Studio1210\\cplex\\bin\\x86-64_win\\"
import Pkg
Pkg.add("CPLEXCP")
Pkg.build("CPLEXCP")

# On macOS, this might be
ENV["CPLEX_STUDIO_BINARIES"] = "/Applications/CPLEX_Studio1210/cplex/bin/x86-64_osx/"
import Pkg
Pkg.add("CPLEXCP")
Pkg.build("CPLEXCP")

# On Unix, this might be
ENV["CPLEX_STUDIO_BINARIES"] = "/opt/CPLEX_Studio1210/cplex/bin/x86-64_linux/"
import Pkg
Pkg.add("CPLEXCP")
Pkg.build("CPLEXCP")
```

**Note: your path may differ. Check which folder you installed CPLEX in, and
update the path accordingly.**

## Use with JuMP

We highly recommend that you use the *CPLEXCP.jl* package with higher level
packages such as [JuMP.jl](https://github.com/jump-dev/JuMP.jl).

This can be done using a ``CPLEXCP.Optimizer`` object. Here is how to create a
*JuMP* model that uses Chuffed as solver.
```julia
using JuMP, CPLEXCP

model = Model(CPLEXCP.Optimizer)
```
