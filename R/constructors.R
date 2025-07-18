
#' Constructors for Covariate Definitions
#'
#' @name constructors
#' @title Functions to define individual covariate properties
#'
#' @description These functions serve as constructors to create objects that define the
#'   properties of different covariate types (continuous, binary, categorical).
#'   These definition objects are then used within the `gen_data` function's
#'   `covariate_defns` argument to specify the characteristics of the simulated covariates.
#'
NULL

#' Constructor for continuous covariates
#' @rdname constructors
#' @export
continuous <- function(mean, sd) {
  structure(
    list(
      mean = mean,
      sd = sd),
    class = "cts_var_type")
}

#' Constructor for binary covariates
#' @rdname constructors
#' @export
binary <- function(prob) {
  if (prob < 0 || prob > 1) {
    stop("Probability for binary covariate must be between 0 and 1.")
  }

  structure(
    list(
      prob = prob,
      threshold = qnorm(prob),
      latent_mean = 0,         # implicit latent mean
      latent_sd = 1),          # implicit latent sd
    class = "bin_var_type")
}

#' Constructor for categorical covariates
#' @rdname constructors
#' @export
categorical <- function(levels, probs = NULL) {
  if (is.null(probs)) {
    probs <- rep(1 / length(levels), length(levels))
  } else if (sum(probs) != 1) {
    stop("Probabilities for categorical covariate must sum to 1.")
  }

  structure(
    list(
      levels = levels,
      probs = probs),
    class = "cat_var_type")
}
