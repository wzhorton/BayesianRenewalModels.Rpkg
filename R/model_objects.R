#### Model object builders ####

#------------------------------#
# Generic config object loader #
#------------------------------#

nondefault_config_string <- function(config){
  output_command <- paste0(config$subtype,"(")
  for(parm in config$parameters){
    if(!parm$default){
      if(parm$type == "float" && abs(parm$value) == floor(abs(parm$value))){
        formatted_value <- sprintf("%.1f", parm$value)
      } else {
        formatted_value <- as.character(parm$value)
      }
      output_command <- paste0(
        output_command, parm$julia_name, "=", formatted_value,", "
      )
    }
  }
  paste0(output_command,")")
}


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
    parameters = list()
  )
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


#-----------------------#
# DensityModel builders #
#-----------------------#

#' Weibull config builder
#'
#' @param a_alpha,b_alpha weibull shape hyperparameters
#' @param a_lam,b_lam weibull scale hyperparameters
#' @return list containing metadata and relevant parameter info
#' @export
build_weibull_config <- function(
    a_alpha = 1.0, b_alpha = 1.0, a_lam = 1.0, b_lam = 1.0
){
  list(
    type = "density",
    subtype = "WeibullModel",
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
        value = a_lam,
        type = "float",
        default = a_lam == formals(build_weibull_config)$a_lam
      ),
      list(
        julia_name = "b_λ",
        value = b_lam,
        type = "float",
        default = b_lam == formals(build_weibull_config)$b_lam
      )
    )
  )
}

#' GammaMixture config builder
#'
#' @param n_component number of mixture components
#' @param a_kappa gamma kernel shape prior
#' @param a_gam_kappa,b_gam_kappa gamma kernel shape hyperparameters
#' @param a_lam gamma kernel scale prior
#' @param a_gam_lam,b_gam_lam gamma kernel scale hyperparameters
#' @return list containing metadata and relevant parameter info
#' @export
build_gammamix_config <- function(
    n_component,
    a_kappa = 3.0,
    a_gam_kappa = 1.0,
    b_gam_kappa = 1.0,
    a_lam = 2.1,
    a_gam_lam = 1.0,
    b_gam_lam = 1.0
){
  list(
    type = "density",
    subtype = "GammaMixtureModel",
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
        value = a_gam_kappa,
        type = "float",
        default = a_gam_kappa == formals(build_gammamix_config)$a_gam_kappa
      ),
      list(
        julia_name = "b_γ_κ",
        value = b_gam_kappa,
        type = "float",
        default = b_gam_kappa == formals(build_gammamix_config)$b_gam_kappa
      ),
      list(
        julia_name = "a_λ",
        value = a_lam,
        type = "float",
        default = a_lam == formals(build_gammamix_config)$a_lam
      ),
      list(
        julia_name = "a_γ_λ",
        value = a_gam_lam,
        type = "float",
        default = a_gam_lam == formals(build_gammamix_config)$a_gam_lam
      ),
      list(
        julia_name = "b_γ_λ",
        value = b_gam_lam,
        type = "float",
        default = b_gam_lam == formals(build_gammamix_config)$b_gam_lam
      )
    )
  )
}

#' UniformMixture config builder
#'
#' @param n_component number of mixture components
#' @param a_eta,b_eta inverse gamma shape hyperparameters
#' @param a_gam,b_gam inverse gamma scale hyperparameters
#' @return list containing metadata and relevant parameter info
#' @export
build_uniformmix_config <- function(
    n_component,
    a_eta = 1.0,
    b_eta = 1.0,
    a_gam = 1.0,
    b_gam = 1.0
){
  list(
    type = "density",
    subtype = "UniformMixtureModel",
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
        value = a_gam,
        type = "float",
        default = a_gam == formals(build_uniformmix_config)$a_gam
      ),
      list(
        julia_name = "b_γ",
        value = b_gam,
        type = "float",
        default = b_gam == formals(build_uniformmix_config)$b_gam
      )
    )
  )
}
