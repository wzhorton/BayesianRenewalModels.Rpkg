% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interface.R
\name{fit_renewal_model}
\alias{fit_renewal_model}
\title{Fit a Bayesian Renewal Model}
\usage{
fit_renewal_model(
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
  first_eval_factor = 1,
  last_eval_factor = 1,
  save_hazard = FALSE,
  save_kfunction = FALSE,
  n_kfunction_eval = 30,
  save_ecdf_model_error = FALSE,
  n_segments_per_eval = 10,
  save_predictive_samples = FALSE,
  save_predictive_state_recurrence_ecdf = FALSE,
  n_ecdf_eval = 100,
  verbose = TRUE,
  display_call = FALSE
)
}
\arguments{
\item{julia}{Julia object returned from \code{build_julia_interface}}

\item{process_config}{builder output for renewal process type: HRP, MRP, ModRP}

\item{density_config}{builder output for density model:
WeibullModel, GammaMixtureModel, UniformMixtureModel, LogLogisticHazardMixtureModel}

\item{stickbreaking_config}{builder output for stick-breaking model:
DPSB, LogitSB, DSLogitSB. Is only required for mixture density models}

\item{sojourns}{vector of sojourn times or inter-arrival times}

\item{max_time}{upper bound on the observation time window}

\item{states}{integer vector of states for MRP process models}

\item{initial_state}{integer value indicating initial state for MRP process models}

\item{n_iteration, n_burnin, n_thin}{MCMC sampling output variables}

\item{n_density_eval}{grid size for the density function evaluation}

\item{n_intensity_eval}{grid size for the intensity function evaluation}

\item{first_eval_factor, last_eval_factor}{multiplicative adjustments for density
evaluation grid, which defaults to evenly spaced from zero to the max datapoint.}

\item{save_hazard}{logical indicating that the hazard inference should be computed.
Matches the dimensionality of the density inference.}

\item{save_kfunction, n_kfunction_eval}{configuration indicating whether the HRP
K-function should be computed and how dense the grid should be. The bounds
will match the density.}

\item{save_ecdf_model_error, n_segments_per_eval}{configuration indicating whether
the HRP ECDF model error should be computed and how many evaluations between
observations.}

\item{save_predictive_samples}{indicates whether a complete predictive
dataset should be generated for each iteration. States are also saved for
MRP models.}

\item{save_predictive_state_recurrence_ecdf, n_ecdf_eval}{configuration for
whether to compute ecdf evaluations and ecdf errors from the predictive
samples.}

\item{verbose}{indicates whether to display sampling progress messages}

\item{display_call}{indicate whether to print the function call generated}
}
\description{
Given a certain configuration setup, passes relevant data to julia, runs the
appropriate MCMC algorithm, and returns a list of posterior samples.
}
