#### Julia setup functions ####

#' Build Julia environment and load \code{BayesianRenewalModels.jl} module.
#'
#' @param install_julia Logical indicating if Julia should be installed in the
#' event that an install either is not installed or cannot be found on the path.
#' @param install_R_dependencies Logical indicating whether R package dependencies
#' used in some Julia routines via RCall.jl should be installed.
#'
#' @return A \code{JuliaCall} object which interfaces to a Julia session.
#' @export

julia_interface <- function(install_julia = FALSE, install_R_dependencies = FALSE){
  if(install_R_dependencies){
    # WARNING: THIS IS UNTESTED. Back-to-back install might not work.
    if(!requireNamespace("devtools", quietly = TRUE)){
      install.packages("devtools")
    }
    if(!requireNamespace("hdtg", quietly = TRUE)){
      # (1) consider using remotes instead of devtools. Possibly more lightweight.
      # (2) CRAN issue with hdtg means installing from source is required
      # (3) current .buildignore issue in repo means skipping build step is required.
      devtools::install_github("suchard-group/hdtg", build = FALSE)
    }
  }

  julia <- JuliaCall::julia_setup(installJulia = install_julia, rebuild = TRUE)
  julia$command("using Pkg")
  julia$call("Pkg.activate", system.file("./julia/BayesianRenewalModels.jl/",
                                         package = "BayesianRenewalModels"))
  julia$command("Pkg.instantiate()")
  julia$source(system.file("./julia/BayesianRenewalModels.jl/src/BayesianRenewalModels.jl",
                           package = "BayesianRenewalModels"))
  julia$command("using .BayesianRenewalModels")
  return(julia)
}
