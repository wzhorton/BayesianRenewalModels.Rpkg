#### Model object builders ####


#----------------------#
# ProcessType builders #
#----------------------#

#' HRP config builder
#'
#' Homogeneous Renewal Process (HRP)
#'
#' @return list containing metadata and relevant parameter info
#' @export
build_hrp_config <- function(){
  list(
    type = "process",
    subtype = "HRP",
    extract_function = extract_hrp_chains,
    parameters = list()
  )
}

extract_hrp_chains <- function(julia){
  chains <- list()
  chains$eval_x = julia$eval('process_output.eval_x')
  chains$eval_kx = julia$eval('process_output.eval_kx')
  chains$density = julia$eval('process_output.density')
  chains$hazard = julia$eval('process_output.hazard')
  chains$kfunction = julia$eval('process_output.kfunction')
  return(chains)
}

#' MRP config builder
#'
#' Markov Renewal Process (MRP)
#'
#' @param a_p transition probability hyperparameter
#' @return list containing metadata and relevant parameter info
#' @export
build_mrp_config <- function(a_p = 0.1){
  list(
    type = "process",
    subtype = "MRP",
    extract_function = extract_mrp_chains,
    parameters = list(
      list(
        julia_name = "a_p",
        value = a_p,
        type = "float",
        default = a_p == formals(build_mrp_config)$a_p
      )
    )
  )
}

extract_mrp_chains <- function(julia){
  chains <- list()
  chains$p = julia$eval('process_output.p')
  chains$eval_x = julia$eval('process_output.eval_x')
  chains$eval_xfp = julia$eval('process_output.eval_xfp')
  chains$density = julia$eval('process_output.density')
  chains$hazard = julia$eval('process_output.hazard')
  chains$first_passage = julia$eval('process_output.first_passage')
  return(chains)
}


#------------------------------#
# StickBreakingPriors builders #
#------------------------------#

#' DPSB config builder
#'
#' Dirichlet Process Stick-Breaking (DPSB)
#'
#' @param a_alpha,b_alpha DP concentration hyperparameters
#' @return list containing metadata and relevant parameter info
#' @export
build_dpsb_config <- function(a_alpha = 5.0, b_alpha = 1.0){
  list(
    type = "stickbreaking",
    subtype = "DPSB",
    extract_function = extract_dpsb_chains,
    parameters = list(
      list(
        julia_name = "a_α",
        value = a_alpha,
        type = "float",
        default = a_alpha == formals(build_dpsb_config)$a_alpha
      ),
      list(
        julia_name = "b_α",
        value = b_alpha,
        type = "float",
        default = b_alpha == formals(build_dpsb_config)$b_alpha
      )
    )
  )
}

extract_dpsb_chains <- function(julia){
  chains <- list()
  chains$alpha = julia$eval('stickbreaking_output.α')
  return(chains)
}

#' LogitSB config builder
#'
#' Logit Stick-Breaking (LogitSB)
#'
#' @param m_mu,s_mulogit stick-breaking mean hyperparameters
#' @param a_sig2,b_sig2 logit stick-breaking variance hyperparameters
#' @return list containing metadata and relevant parameter info
#' @export
build_logitsb_config <- function(m_mu = 0.0, s_mu = 1000.0, a_sig2 = 1.0, b_sig2 = 1.0){
  list(
    type = "stickbreaking",
    subtype = "LogitSB",
    extract_function = extract_logitsb_chains,
    parameters = list(
      list(
        julia_name = "m_μ",
        value = m_mu,
        type = "float",
        default = m_mu == formals(build_logitsb_config)$m_mu
      ),
      list(
        julia_name = "s_μ",
        value = s_mu,
        type = "float",
        default = s_mu == formals(build_logitsb_config)$s_mu
      ),
      list(
        julia_name = "a_σ",
        value = a_sig2,
        type = "float",
        default = a_sig2 == formals(build_logitsb_config)$a_sig2
      ),
      list(
        julia_name = "b_σ",
        value = b_sig2,
        type = "float",
        default = b_sig2 == formals(build_logitsb_config)$b_sig2
      )
    )
  )
}

extract_logitsb_chains <- function(julia){
  chains <- list()
  chains$psi = julia$eval('stickbreaking_output.ψ')
  chains$mu = julia$eval('stickbreaking_output.μ')
  chains$sig2 = julia$eval('stickbreaking_output.σ')
  return(chains)
}

#' DSLogitSB config builder
#'
#' Dependent Sojourns Logit Stick-Breaking (DSLogitSB)
#'
#' @param m_0 common substructure mean hyperparameter
#' @param a_sig2_mu,b_sig2_mu common substructure variance hyperparameters
#' @param a_sig2_alpha,b_sig2_alpha common-from substructure variance hyperparameters
#' @param a_sig2_beta,b_sig2_beta common-to substructure variance hyperparameters
#' @param a_sig2_psi,b_sig2_psi independent substructure variance hyperparameters
#' @return list containing metadata and relevant parameter info
#' @export
build_dslogitsb_config <- function(
    m_0 = 0.0,
    a_sig2_psi = 2.0,
    b_sig2_psi = 1.0,
    a_sig2_alpha = 2.0,
    b_sig2_alpha = 1.0,
    a_sig2_beta = 2.0,
    b_sig2_beta = 1.0,
    a_sig2_mu = 2.0,
    b_sig2_mu = 1.0
){
  list(
    type = "stickbreaking",
    subtype = "DSLogitSB",
    extract_function = extract_dslogitsb_chains,
    parameters = list(
      list(
        julia_name = "m_0",
        value = m_0,
        type = "float",
        default = m_0 == formals(build_dslogitsb_config)$m_0
      ),
      list(
        julia_name = "a_σ_ψ",
        value = a_sig2_psi,
        type = "float",
        default = a_sig2_psi == formals(build_dslogitsb_config)$a_sig2_psi
      ),
      list(
        julia_name = "b_σ_ψ",
        value = b_sig2_psi,
        type = "float",
        default = b_sig2_psi == formals(build_dslogitsb_config)$b_sig2_psi
      ),
      list(
        julia_name = "a_σ_α",
        value = a_sig2_alpha,
        type = "float",
        default = a_sig2_alpha == formals(build_dslogitsb_config)$a_sig2_alpha
      ),
      list(
        julia_name = "b_σ_α",
        value = b_sig2_alpha,
        type = "float",
        default = b_sig2_alpha == formals(build_dslogitsb_config)$b_sig2_alpha
      ),
      list(
        julia_name = "a_σ_β",
        value = a_sig2_beta,
        type = "float",
        default = a_sig2_beta == formals(build_dslogitsb_config)$a_sig2_beta
      ),
      list(
        julia_name = "b_σ_β",
        value = b_sig2_beta,
        type = "float",
        default = b_sig2_beta == formals(build_dslogitsb_config)$b_sig2_beta
      ),
      list(
        julia_name = "a_σ_μ",
        value = a_sig2_mu,
        type = "float",
        default = a_sig2_mu == formals(build_dslogitsb_config)$a_sig2_mu
      ),
      list(
        julia_name = "b_σ_μ",
        value = b_sig2_mu,
        type = "float",
        default = b_sig2_mu == formals(build_dslogitsb_config)$b_sig2_mu
      )
    )
  )
}

extract_dslogitsb_chains <- function(julia){
  chains <- list()
  chains$psi = julia$eval('stickbreaking_output.ψ')
  chains$mu = julia$eval('stickbreaking_output.μ')
  chains$alpha = julia$eval('stickbreaking_output.α')
  chains$beta = julia$eval('stickbreaking_output.β')
  chains$sig2_psi = julia$eval('stickbreaking_output.σ_ψ')
  chains$sig2_alpha = julia$eval('stickbreaking_output.σ_α')
  chains$sig2_beta = julia$eval('stickbreaking_output.σ_β')
  chains$sig2_mu = julia$eval('stickbreaking_output.σ_μ')
  return(chains)
}


#-----------------------#
# DensityModel builders #
#-----------------------#

#' Weibull config builder
#'
#' @param a_alpha,b_alpha weibull shape hyperparameters
#' @param a_lambda,b_lambda weibull scale hyperparameters
#' @return list containing metadata and relevant parameter info
#' @export
build_weibull_config <- function(
    a_alpha = 1.0, b_alpha = 1.0, a_lambda = 1.0, b_lambda = 1.0
){
  list(
    type = "density",
    subtype = "WeibullModel",
    requires_stickbreaking = FALSE,
    extract_function = extract_weibull_chains,
    parameters = list(
      list(
        julia_name = "a_α",
        value = a_alpha,
        type = "float",
        default = a_alpha == formals(build_weibull_config)$a_alpha
      ),
      list(
        julia_name = "b_α",
        value = b_alpha,
        type = "float",
        default = b_alpha == formals(build_weibull_config)$b_alpha
      ),
      list(
        julia_name = "a_λ",
        value = a_lambda,
        type = "float",
        default = a_lambda == formals(build_weibull_config)$a_lambda
      ),
      list(
        julia_name = "b_λ",
        value = b_lambda,
        type = "float",
        default = b_lambda == formals(build_weibull_config)$b_lambda
      )
    )
  )
}

extract_weibull_chains <- function(julia){
  chains <- list()
  chains$alpha = julia$eval('density_output.α')
  chains$lambda = julia$eval('density_output.λ')
  return(chains)
}

#' GammaMixture config builder
#'
#' @param n_component number of mixture components
#' @param a_kappa gamma kernel shape prior
#' @param a_gamma_kappa,b_gamma_kappa gamma kernel shape hyperparameters
#' @param a_lambda gamma kernel scale prior
#' @param a_gamma_lambda,b_gamma_lambda gamma kernel scale hyperparameters
#' @return list containing metadata and relevant parameter info
#' @export
build_gammamix_config <- function(
    n_component,
    a_kappa = 3.0,
    a_gamma_kappa = 1.0,
    b_gamma_kappa = 1.0,
    a_lambda = 2.1,
    a_gamma_lambda = 1.0,
    b_gamma_lambda = 1.0
){
  list(
    type = "density",
    subtype = "GammaMixtureModel",
    extract_function = extract_gammamix_chains,
    requires_stickbreaking = TRUE,
    parameters = list(
      list(
        julia_name = "n_component",
        value = n_component,
        type = "int",
        default = FALSE
      ),
      list(
        julia_name = "a_κ",
        value = a_kappa,
        type = "float",
        default = a_kappa == formals(build_gammamix_config)$a_kappa
      ),
      list(
        julia_name = "a_γ_κ",
        value = a_gamma_kappa,
        type = "float",
        default = a_gamma_kappa == formals(build_gammamix_config)$a_gamma_kappa
      ),
      list(
        julia_name = "b_γ_κ",
        value = b_gamma_kappa,
        type = "float",
        default = b_gamma_kappa == formals(build_gammamix_config)$b_gamma_kappa
      ),
      list(
        julia_name = "a_λ",
        value = a_lambda,
        type = "float",
        default = a_lambda == formals(build_gammamix_config)$a_lambda
      ),
      list(
        julia_name = "a_γ_λ",
        value = a_gamma_lambda,
        type = "float",
        default = a_gamma_lambda == formals(build_gammamix_config)$a_gamma_lambda
      ),
      list(
        julia_name = "b_γ_λ",
        value = b_gamma_lambda,
        type = "float",
        default = b_gamma_lambda == formals(build_gammamix_config)$b_gamma_lambda
      )
    )
  )
}

extract_gammamix_chains <- function(julia){
  chains <- list()
  chains$kappa = julia$eval('density_output.κ')
  chains$lambda = julia$eval('density_output.λ')
  chains$gamma_kappa = julia$eval('density_output.γ_κ')
  chains$gamma_lambda = julia$eval('density_output.γ_λ')
  chains$omega = julia$eval('density_output.ω')
  return(chains)
}

#' UniformMixture config builder
#'
#' @param n_component number of mixture components
#' @param a_eta,b_eta inverse gamma shape hyperparameters
#' @param a_gamma,b_gamma inverse gamma scale hyperparameters
#' @return list containing metadata and relevant parameter info
#' @export
build_uniformmix_config <- function(
    n_component,
    a_eta = 1.0,
    b_eta = 1.0,
    a_gamma = 1.0,
    b_gamma = 1.0
){
  list(
    type = "density",
    subtype = "UniformMixtureModel",
    extract_function = extract_uniformmix_chains,
    requires_stickbreaking = TRUE,
    parameters = list(
      list(
        julia_name = "n_component",
        value = n_component,
        type = "int",
        default = FALSE
      ),
      list(
        julia_name = "a_η",
        value = a_eta,
        type = "float",
        default = a_eta == formals(build_uniformmix_config)$a_eta
      ),
      list(
        julia_name = "b_η",
        value = b_eta,
        type = "float",
        default = b_eta == formals(build_uniformmix_config)$b_eta
      ),
      list(
        julia_name = "a_γ",
        value = a_gamma,
        type = "float",
        default = a_gamma == formals(build_uniformmix_config)$a_gamma
      ),
      list(
        julia_name = "b_γ",
        value = b_gamma,
        type = "float",
        default = b_gamma == formals(build_uniformmix_config)$b_gamma
      )
    )
  )
}

extract_uniformmix_chains <- function(julia){
  chains <- list()
  chains$theta = julia$eval('density_output.θ')
  chains$eta_theta = julia$eval('density_output.η_θ')
  chains$gamma_theta = julia$eval('density_output.γ_θ')
  chains$omega = julia$eval('density_output.ω')
  return(chains)
}
