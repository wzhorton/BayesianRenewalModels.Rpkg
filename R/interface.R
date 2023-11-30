#### Main interface functions ####

#-----------------------------#
# Generic config string maker #
#-----------------------------#

nondefault_config_string <- function(config, function_name = config$subtype){
  output_command <- function_name
  if(length(function_name) > 0){
    output_command <- paste0(output_command,"(")
  }
  for(parm in config$parameters){
    if(!parm$default){
      if(parm$type == "float" && abs(parm$value) == floor(abs(parm$value))){
        formatted_value <- sprintf("%.1f", parm$value)
      } else if(parm$type == "float" || parm$type == "int") {
        formatted_value <- as.character(parm$value)
      } else if(parm$type == "bool"){
        formatted_value <- ifelse(parm$value, "true", "false")
      } else if(parm$type == "raw"){
        formatted_value <- as.character(parm$value)
      }
      output_command <- paste0(
        output_command, parm$julia_name, "=", formatted_value,", "
      )
    }
  }
  if(length(function_name) > 0){
      output_command <- paste0(output_command,")")
  }
  output_command
}

#' Fit a Bayesian Renewal Model
#'
#' Given a certain configuration setup, passes relevant data to julia, runs the
#' appropriate MCMC algorithm, and returns a list of posterior samples.
#'
#' @param julia Julia object returned from `build_julia_interface`
#' @param process_config builder output for renewal process type: HRP, MRP
#' @param density_config builder output for density model:
#'  WeibullModel, GammaMixtureModel, UniformMixtureModel
#' @param stickbreaking_config builder output for stick-breaking model:
#'  DPSB, LogitSB, DSLogitSB. Is only required for mixture density models
#' @param sojourns vector of sojourn times or inter-arrival times
#' @param max_time upper bound on the observation time window
#' @param states integer vector of states for MRP process models
#' @param initial_state integer value indicating initial state for MRP process models
#' @param n_iteration,n_burnin,n_thin MCMC sampling output variables
#' @param n_dendsity_eval,n_kfunction_eval grid size for function evaluation.
#'  K-function only applies to HRP process models
#' @param eval_factor multiplier for upper bound of evaluation grid
#'  (normally at max(sojourns))
#'.@param first_eval_factor adjustment for first value in evaluation grid.
#'  A value of 1 results in an even grid, while values <1 lead to lower
#'  initial values.
#' @param save_hazard,save_kfunction indicate whether to save these functions.
#'  K-function only applies to HRP process models
#' @param save_ecdf_error indicate whether to save the empirical cdf minus
#' modeled cdf squared error for HRP models
#' @param verbose indicates whether to display sampling progress messages
#' @param display_call indicate whether to print the function call generated
#' @export
fit_renewal_model <- function(
    julia,
    process_config,
    density_config,
    stickbreaking_config,
    sojourns,
    max_time,
    states,
    initial_state = 1,
    n_iteration = 5000,
    n_burnin = 1000,
    n_thin = 1,
    n_density_eval = 100,
    n_kfunction_eval = 30,
    n_first_passage_eval = 30,
    eval_factor = 1.0,
    first_eval_factor = 1.0,
    save_hazard = FALSE,
    save_kfunction = FALSE,
    save_first_passage = FALSE,
    save_ecdf_error = FALSE,
    verbose = TRUE,
    display_call = FALSE
){
    process_string <- paste0(nondefault_config_string(process_config), ", ")
    density_string <- paste0(nondefault_config_string(density_config), ", ")
    if(density_config$requires_stickbreaking){
        stickbreaking_string <- paste0(nondefault_config_string(stickbreaking_config),", ")
    } else {
      stickbreaking_string <- ""
    }

    # create output config starting with universal requirements
    output_config <- list(
      parameters = list(
        list(
          julia_name = "n_iteration",
          value = n_iteration,
          type = "int",
          default = n_iteration == formals(fit_renewal_model)$n_iteration
        ),
        list(
          julia_name = "n_burnin",
          value = n_burnin,
          type = "int",
          default = n_burnin == formals(fit_renewal_model)$n_burnin
        ),
        list(
          julia_name = "n_thin",
          value = n_thin,
          type = "int",
          default = n_thin == formals(fit_renewal_model)$n_thin
        ),
        list(
          julia_name = "n_density_eval",
          value = n_density_eval,
          type = "int",
          default = n_density_eval == formals(fit_renewal_model)$n_density_eval
        ),
        list(
          julia_name = "eval_factor",
          value = eval_factor,
          type = "float",
          default = eval_factor == formals(fit_renewal_model)$eval_factor
        ),
        list(
          julia_name = "first_eval_factor",
          value = first_eval_factor,
          type = "raw",
          default = first_eval_factor == formals(fit_renewal_model)$first_eval_factor
        ),
        list(
          julia_name = "save_hazard",
          value = save_hazard,
          type = "bool",
          default = save_hazard == formals(fit_renewal_model)$save_hazard
        ),
        list(
          julia_name = "verbose",
          value = verbose,
          type = "bool",
          default = verbose == formals(fit_renewal_model)$verbose
        )
      )
    )

    # append additional arguments based on model configs
    if(process_config$subtype == "HRP"){
      output_config$parameters <- append(output_config$parameters, list(
        list(
          julia_name = "save_kfunction",
          value = save_kfunction,
          type = "bool",
          default = save_kfunction == formals(fit_renewal_model)$save_kfunction
        ),
        list(
          julia_name = "save_ecdf_error",
          value = save_ecdf_error,
          type = "bool",
          default = save_ecdf_error == formals(fit_renewal_model)$save_ecdf_error
        ),
        list(
          julia_name = "n_kfunction_eval",
          value = n_kfunction_eval,
          type = "int",
          default = n_kfunction_eval == formals(fit_renewal_model)$n_kfunction_eval
        )
      ))
    }
    if(process_config$subtype == "MRP"){
      output_config$parameters <- append(output_config$parameters, list(
        list(
          julia_name = "initial_state",
          value = initial_state,
          type = "int",
          default = save_kfunction == formals(fit_renewal_model)$save_kfunction
        ),
        list(
          julia_name = "save_first_passage",
          value = save_first_passage,
          type = "bool",
          default = save_first_passage == formals(fit_renewal_model)$save_first_passage
        ),
        list(
          julia_name = "n_first_passage_eval",
          value = n_first_passage_eval,
          type = "int",
          default = n_first_passage_eval == formals(fit_renewal_model)$n_first_passage_eval
        )
      ))
    }
    output_config_string <- nondefault_config_string(output_config)

    # assign required data elements to julia environment
    julia$assign("max_time", max_time)
    if(process_config$subtype == "HRP"){
      julia$assign("interarrivals", sojourns)
    }else if(process_config$subtype == "MRP"){
      julia$assign("sojourns", sojourns)
      julia$assign("states", as.integer(states))
    }

    # construct command
    unpack <- "process_output, density_output"
    if(density_config$requires_stickbreaking){
      unpack <- paste0(unpack, ", stickbreaking_output")
    }

    function_call <- paste0(
      unpack,
      " = fit_renewal_model(",
      process_string,
      density_string,
      stickbreaking_string
    )
    if(process_config$subtype == "HRP"){
      function_call <- paste0(function_call, "interarrivals, max_time; ")
    } else if(process_config$subtype == "MRP"){
      function_call <- paste0(function_call, "sojourns, states, max_time; ")
    }
    function_call <- paste0(function_call, output_config_string,")")

    # optionally display function call
    if(display_call){
      print(function_call)
    }
    # run command and collect results
    julia$command(function_call, show_value = FALSE)
    chains <- c(
      process_config$extract_function(julia),
      density_config$extract_function(julia)
    )
    if(density_config$requires_stickbreaking){
      chains <- c(chains, stickbreaking_config$extract_function(julia))
    }
    chains$function_call <- function_call

    return(chains)
}
