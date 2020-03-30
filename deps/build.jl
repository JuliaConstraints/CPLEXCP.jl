using Libdl

# Cannot use the static library cp.lib: very hard to build the corresponding
# dynamic library.
# Cannot use CxxWrap, because it is highly tied to MinGW... and CPLEX CPO is
# compiled with Visual C++.
# Use JavaCall, even though it has some problems with JULIA_COPY_STACKS, @async,
# and other parts of the program using it (the classpath must be set before
# initialising JavaCall).

const depsfile = joinpath(dirname(@__FILE__), "deps.jl")
if isfile(depsfile)
    rm(depsfile)
end

function write_depsfile(path)
    open(depsfile, "w") do f
        println(f, "const libcplexcpojava = \"$(escape_string(path))\"")
    end
end

function get_jar_file()
    # Find the path to the CPLEX CP Optimizer executable.
    cplex_path = try
        @static if Sys.isapple() || Sys.isunix()
            dirname(strip(read(`which cpoptimizer`, String)))
        elseif Sys.iswindows()
            dirname(strip(read(`where cpoptimizer`, String)))
        end
    catch
        nothing
    end

    # Iterate through a series of places where CPLEX could be found: either in
    # the path (directly the callable library or the CPLEX executable) or from
    # an environment variable.
    cpxvers = ["129", "1290", "1210", "12100" ]

    if cplex_path !== nothing
        lib_path = abspath(cplex_path * "../../lib/ILOG.CP.jar")
        if isfile(lib_path)
            return lib_path
        end
    end

    if found_path === nothing
        base_env = "CPLEX_STUDIO_DIR"

        for v in reverse(cpxvers)
            for env in [base_env, base_env * v]
                if !haskey(ENV, env)
                    continue
                end

                lib_path = abspath(ENV[env] * "/cpoptimizer/lib/ILOG.CP.jar")
                if isfile(lib_path)
                    return lib_path
                end
            end
        end
    end

    error(
        "Unable to locate CPLEX installation. Note this must be downloaded " *
        "separately. See the CPLEXCP.jl README for further instructions."
    )
end

function try_local_installation()
    write_depsfile(get_jar_file())
    @info("Using CPLEX found in location `$(l)`")
end

try_local_installation()
