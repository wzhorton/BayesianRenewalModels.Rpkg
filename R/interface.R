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
#' @param process_config builder output for renewal process type: HRP, MRP, ModRP
#' @param density_config builder output for density model:
#'  WeibullModel, GammaMixtureModel, UniformMixtureModel, LogLogisticHazardMixtureModel
#' @param stickbreaking_config builder output for stick-breaking model:
#'  DPSB, LogitSB, DSLogitSB. Is only required for mixture density models
#' @param sojourns vector of sojourn times or inter-arrival times
#' @param max_time upper bound on the observation time window
#' @param states integer vector of states for MRP process models
#' @param initial_state integer value indicating initial state for MRP process models
#' @param n_iteration,n_burnin,n_thin MCMC sampling output variables
#' @param n_density_eval grid size for the density function evaluation
#' @param n_intensity_eval grid size for the intensity function evaluation
#' @param first_eval_factor,last_eval_factor multiplicative adjustments for density
#' evaluation grid, which defaults to evenly spaced from zero to the max datapoint.
#' @param save_hazard logical indicating that the hazard inference should be computed.
#' Matches the dimensionality of the density inference.
#' @param save_kfunction,n_kfunction_eval configuration indicating whether the HRP
#' K-function should be computed and how dense the grid should be. The bounds
#' will match the density.
#' @param save_ecdf_model_error,n_segments_per_eval configuration indicating whether
#' the HRP ECDF model error should be computed and how many evaluations between
#' observations.
#' @param save_predictive_samples indicates whether a complete predictive
#' dataset should be generated for each iteration. States are also saved for
#' MRP models.
#' @param save_predictive_state_recurrence_ecdf,n_ecdf_eval configuration for
#' whether to compute ecdf evaluations and ecdf errors from the predictive
#' samples.
#' @param save_conditional_interarrival_densitiesl,conditional_times configuration
#' for wether to compute modrp conditional interarrival densities and at what
#' conditional times.
#' @param save_time_rescaling_values configuration for whether to compute the
#' time rescaling theorem values for model checking
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
    n_intensity_eval = 100,
    first_eval_factor = 1.0,
    last_eval_factor = 1.0,
    save_hazard = FALSE,
    save_kfunction = FALSE,
    n_kfunction_eval = 30,
    save_ecdf_model_error = FALSE,
    n_segments_per_eval = 10,
    save_predictive_samples = FALSE,
    save_predictive_state_recurrence_ecdf = FALSE,
    n_ecdf_eval = 100,
    save_conditional_interarrival_densities = FALSE,
    conditional_times = c(0, max_time),
    save_time_rescaling_values = FALSE,
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
          julia_name = "first_eval_factor",
          value = first_eval_factor,
          type = "raw",
          default = first_eval_factor == formals(fit_renewal_model)$first_eval_factor
        ),
        list(
          julia_name = "last_eval_factor",
          value = last_eval_factor,
          type = "float",
          default = last_eval_factor == formals(fit_renewal_model)$last_eval_factor
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
    if(process_config$subtype == "MRP"){
      output_config$parameters <- append(output_config$parameters, list(
        list(
          julia_name = "initial_state",
          value = initial_state,
          type = "int",
          default = save_kfunction == formals(fit_renewal_model)$save_kfunction
        )
      ))
    }
    if(process_config$subtype == "ModRP"){
      output_config$parameters <- append(output_config$parameters, list(
        list(
          julia_name = "n_intensity_eval",
          value = n_intensity_eval,
          type = "int",
          default = n_intensity_eval == formals(fit_renewal_model)$n_intensity_eval
        )
      ))
    }
    output_config_string <- nondefault_config_string(output_config)

    # assign required data elements to julia environment
    julia$assign("max_time", max_time)
    if(process_config$subtype %in% c("HRP","ModRP")){
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
      "tuple_output = fit_renewal_model(",
      process_string,
      density_string,
      stickbreaking_string
    )
    if(process_config$subtype %in% c("HRP", "ModRP")){
      function_call <- paste0(function_call, "interarrivals, max_time; ")
    } else if(process_config$subtype == "MRP"){
      function_call <- paste0(function_call, "sojourns, states, max_time; ")
    }
    function_call <- paste0(function_call, output_config_string,")")

    # optionally display function call
    if(display_call){
      print(function_call)
    }

    # run command and higher order inferences
    julia$command(function_call, show_value = FALSE)
    julia$command(paste0(unpack, " = tuple_output"), show_value = FALSE)
    if(save_hazard){
      julia$command(
        paste0(
          "save_hazard!(tuple_output...; verbose = ",
          ifelse(verbose, "true", "false"),
          ")"
        ),
        show_value = FALSE
      )
    }
    if(save_kfunction && process_config$subtype == "HRP"){
      julia$command(
        paste0(
          "save_kfunction!(tuple_output...; n_kfunction_eval = ",
          n_kfunction_eval,
          ", verbose = ",
          ifelse(verbose, "true", "false"),
          ")"
        ),
        show_value = FALSE
      )
    }
    if(save_ecdf_model_error && process_config$subtype == "HRP"){
      julia$command(
        paste0(
          "save_ecdf_model_error!(interarrivals, tuple_output...; n_segments_per_eval = ",
          n_segments_per_eval,
          ", verbose = ",
          ifelse(verbose, "true", "false"),
          ")"
        ),
        show_value = FALSE
      )
    }
    if(save_predictive_samples){
      julia$command(
        paste0(
          "save_predictive_samples!(max_time, ",
          ifelse(process_config$subtype != "MRP", "",
                 paste0(initial_state,", ")),
          "tuple_output...; verbose = ",
          ifelse(verbose, "true", "false"),
          ")"
        ),
        show_value = FALSE
      )
    }
    if(save_predictive_state_recurrence_ecdf && save_predictive_samples && process_config$subtype == "MRP"){
      julia$command(
        paste0(
          "save_predictive_state_recurrence_ecdf!(sojourns, states, ",
          initial_state,
          ", tuple_output...; n_ecdf_eval = ",
          n_ecdf_eval,
          ", verbose = ",
          ifelse(verbose, "true", "false"),
          ")"
        )
      )
    }
    if(save_conditional_interarrival_densities && process_config$subtype == "ModRP"){
      julia$assign("conditional_times", conditional_times)
      julia$command(
        paste0(
          "save_conditional_interarrival_densities!(tuple_output..., conditional_times",
          "; verbose = ",
          ifelse(verbose, "true", "false"),
          ")"
        )
      )
    }
    if(save_time_rescaling_values && process_config$subtype == "ModRP"){
      julia$command(
        paste0(
          "save_time_rescaling_values!(interarrivals, tuple_output...; verbose = ",
          ifelse(verbose, "true", "false"),
          ")"
        )
      )
    }

    # collect results
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
