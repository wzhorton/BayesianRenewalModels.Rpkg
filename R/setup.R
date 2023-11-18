#### Julia setup functions ####

#' Build Julia environment and load \code{BayesianRenewalModels.jl} module.
#'
#' @param install_julia Logical indicating if Julia should be installed in the
#' event that an install either is not installed or cannot be found on the path.
#'
#' @return A \code{JuliaCall} object which interfaces to a Julia session.
#' @export

build_julia_interface <- function(install_julia = FALSE){
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
