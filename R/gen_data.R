
#' Generate simulated datasets of IPD covariates and outcome for a trial
#'
#' @param N
#' @param b_0
#' @param b_trt
#' @param covariate_defns
#' @param b_prognostic
#' @param b_effect_modifier
#' @param cor_matrix Applies to all latent covariates
#' @param trt_assignment
#' @param error_params
#' @param family
#'
#' @returns Data frame of simulated IPD
#' @export
#'
#' @examples
#'
gen_data <- function(N, b_0, b_trt,
                     covariate_defns,
                     b_prognostic = NULL,
                     b_effect_modifier = NULL,
                     cor_matrix = NULL,
                     trt_assignment = list(prob_trt1 = 0.5),
                     error_params = list(sd = 1.0),
                     family = gaussian(link = "identity")) {

  data <- data.frame(id = 1:N)

  covariate_defns <- assign_default_covariate_names(covariate_defns)

  # collect info for all covariates that rely on Normal distribution
  # (continuous actual, binary latent)
  normal_names <- c()
  normal_means <- c()
  normal_sds <- c()
  bin_thresholds <- list()  # store thresholds keyed by covariate name

  for (name in names(covariate_defns)) {
    cov_def <- covariate_defns[[name]]
    cov_type_obj <- cov_def$type

    if (inherits(cov_type_obj, "cts_var_type")) {

      normal_names <- c(normal_names, name)
      normal_means <- c(normal_means, cov_type_obj$mean)
      normal_sds <- c(normal_sds, cov_type_obj$sd)

    } else if (inherits(cov_type_obj, "bin_var_type")) {

      normal_names <- c(normal_names, name)
      normal_means <- c(normal_means, cov_type_obj$latent_mean)
      normal_sds <- c(normal_sds, cov_type_obj$latent_sd)
      bin_thresholds[[name]] <- cov_type_obj$threshold

    } else {
      stop(paste("Unsupported covariate type object for covariate '", name, "'.", sep=""))
    }
  }

  # generate correlated Normal variables (for continuous and latent binary)

  if (length(normal_names) > 0) {

    if (is.null(cor_matrix)) {
      # if no correlation matrix, generate independently
      generated_normal_data <- matrix(NA, nrow = N, ncol = length(normal_names))

      for (i in seq_along(normal_names)) {
        generated_normal_data[, i] <-
          rnorm(N, mean = normal_means[i], sd = normal_sds[i])
      }

      colnames(generated_normal_data) <- normal_names

    } else {
      # ensure cor_matrix dimensions and names match all normal_names
      if (!all(rownames(cor_matrix) %in% normal_names) ||
          !all(normal_names %in% rownames(cor_matrix))) {
        stop("`cor_matrix` row/column names must match names of continuous
             and binary covariates in `covariate_defns`.")
      }

      ordered_cor_matrix <- cor_matrix[normal_names, normal_names]

      sigma_matrix <- diag(normal_sds) %*% ordered_cor_matrix %*% diag(normal_sds)

      generated_normal_data <- MASS::mvrnorm(n = N, mu = normal_means, Sigma = sigma_matrix)
      colnames(generated_normal_data) <- normal_names
    }

    for (col_name in normal_names) {
      data[[col_name]] <- generated_normal_data[, col_name]
    }
  }

  # apply thresholds for Binary covariates
  for (bin_name in names(bin_thresholds)) {
    data[[bin_name]] <- ifelse(data[[bin_name]] < bin_thresholds[[bin_name]], 1, 0)
  }

  # generate treatment assignment
  prob_trt1 <- trt_assignment$prob_trt1
  data$trt <- rbinom(N, size = 1, prob = prob_trt1)

  # calculate linear predictor

  eta <- rep(b_0, N)
  eta <- eta + data$trt * b_trt

  if (!is.null(b_prognostic)) {
    for (cov_name in names(b_prognostic)) {

      if (!cov_name %in% names(data)) {
        stop(paste("Prognostic covariate '", cov_name, "' not found.", sep=""))
      }

      eta <- eta + data[[cov_name]] * b_prognostic[[cov_name]]
    }
  }

  if (!is.null(b_effect_modifier)) {
    for (cov_name in names(b_effect_modifier)) {

      if (!cov_name %in% names(data)) {
        stop(paste("Effect modifier covariate '", cov_name, "' not found.", sep=""))
      }

      eta <- eta + data[[cov_name]] * data$trt * b_effect_modifier[[cov_name]]
    }
  }

  # generate outcome

  data$y <- switch(
    family$family,
    "binomial" = rbinom(n = N, size = 1, prob = family$linkinv(eta)),
    "gaussian" = rnorm(n = N, mean = eta, sd = error_params$sd),
    "poisson"  = rpois(n = N, lambda = family$linkinv(eta)),
    stop("Unsupported family.")
  )

  data$true_eta <- eta

  return(data)
}
