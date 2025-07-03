
#' Generate simulated datasets of IPD covariates and outcome for a trial
#'
#' @param N Total number of patients
#' @param b_trt `b` coefficient for active treatment vs. common comparator
#' @param meanX Mean vector of each normally-distributed covariate `X`
#' @param sdX Standard deviation vector of each covariate `X`
#' @param b_X `b` coefficients for each prognostic variable `X`
#' @param meanX_EM Mean vector of each normally-distributed EM covariate `X`
#' @param sdX_EM Standard deviation vector of each EM covariate `X`
#' @param b_EM `b` coefficients effect modifiers
#' @param prob_X_bin Probability for each binary distributed covariate `X`
#' @param b_X_bin `b` coefficients for each prognostic variable `X`
#' @param prob_EM_bin Probability for each binary distributed EM covariate `X`
#' @param b_EM_bin `b` coefficients effect modifiers
#' @param b_0 Intercept coefficient
#' @param corX Covariate correlation coefficient of `X`
#' @param allocation Allocation to active treatment as proportion of total; 0 to 1
#' @param sigma Standard deviation of outcome; optional
#' @param family Family object
#' @return Data frame of `X`, `trt` and `y`
#'
#' @importFrom MASS mvrnorm
#' @importFrom dplyr relocate mutate
#' @export
#'
#' @examples
#'
#' \dontrun{
#' x <- gen_data(
#'  N = 100,
#'  b_trt = log(0.17),
#'  b_X = -log(0.5),
#'  b_EM = -log(0.67),
#'  b_0 = -0.62,
#'  meanX = c(0.6, 0.6),
#'  sdX = c(0.4, 0.4),
#'  meanX_EM = c(0.6, 0.6),
#'  sdX_EM = c(0.4, 0.4),
#'  corX = 0.2,
#'  allocation = 2/3)
#'
#' head(x)
#' }
gen_data <- function(N, b_trt,
                     # continuous covariates
                     meanX = NULL, sdX = NULL, b_X = NULL,
                     meanX_EM = NULL, sdX_EM = NULL, b_EM = NULL,
                     # binary covariates
                     prob_X_bin = NULL, b_X_bin = NULL,
                     prob_EM_bin = NULL, b_EM_bin = NULL,
                     #
                     b_0,
                     corX, allocation,
                     sigma = 1,
                     family = binomial("logit")) {

  n_X <- length(meanX)
  n_X_EM <- length(meanX_EM)
  n_X_bin <- length(prob_X_bin)
  n_EM_bin <- length(prob_EM_bin)

  n_c <- n_X + n_X_EM + n_X_bin + n_EM_bin
  if (n_c == 0) stop("No covariates specified.", call. = FALSE)

  # internally build the mean and sd vectors for mvrnorm
  # for binary latent variables, we ALWAYS use mean=0, sd=1
  # user does not need to know or provide this
  all_means <- c(meanX, meanX_EM, rep(0, n_X_bin), rep(0, n_EM_bin))
  all_sds <- c(sdX, sdX_EM, rep(1, n_X_bin), rep(1, n_EM_bin))

  if (length(b_X) == 1) {
    b_X <- rep(b_X, n_X)
  }
  if (length(b_EM) == 1) {
    b_EM <- rep(b_EM, n_X_EM)
  }
  if (length(b_X_bin) == 1) {
    b_X_bin <- rep(b_X_bin, n_X_bin)
  }
  if (length(b_EM_bin) == 1) {
    b_EM_bin <- rep(b_EM_bin, n_EM_bin)
  }

  # create clear names for all variables
  PF_cont_names <- if (n_X > 0) paste0("PF_cont_", 1:n_X) else NULL
  EM_cont_names <- if (n_X_EM > 0) paste0("EM_cont_", 1:n_X_EM) else NULL
  PF_bin_names <- if (n_X_bin > 0) paste0("PF_bin_", 1:n_X_bin) else NULL
  EM_bin_names <- if (n_EM_bin > 0) paste0("EM_bin_", 1:n_EM_bin) else NULL

  all_names <- c(PF_cont_names, EM_cont_names, PF_bin_names, EM_bin_names)

  # set correlation matrix
  if (is.matrix(corX)) {
    rho <- corX
  } else {
    rho <- matrix(corX, n_c, n_c)
    diag(rho) <- 1
  }

  cov.mat <- cor2cov(rho, all_sds)
  diag(rho) <- rep(1, n_c)
  N_active <- round(N*allocation)  # number of patients under active treatment
  N_control <- N - N_active        # number of patients under control

  # simulate correlated continuous covariates using multivariate normal
  # patients under active treatment
  X_active <-
    as.data.frame(
      MASS::mvrnorm(n = N_active,
                    mu = all_means,
                    Sigma = cov.mat))

  # patients under control treatment
  X_control <-
    as.data.frame(
      MASS::mvrnorm(n = N_control,
                    mu = all_means,
                    Sigma = cov.mat))

  # all patients
  final_data <- rbind(X_active, X_control)
  colnames(final_data) <- all_names

  # use probability for threshold
  if (n_X_bin > 0) {
    thresholds_X <- qnorm(prob_X_bin)

    for (i in seq_len(n_X_bin)) {
      final_data[[PF_bin_names[i]]] <-
        ifelse(final_data[[PF_bin_names[i]]] < thresholds_X[i], 1, 0)
    }
  }

  if (n_EM_bin > 0) {
    thresholds_EM <- qnorm(prob_EM_bin)

    for (i in seq_len(n_EM_bin)) {
      final_data[[EM_bin_names[i]]] <-
        ifelse(final_data[[EM_bin_names[i]]] < thresholds_EM[i], 1, 0)
    }
  }

  trt <- c(rep(1, round(N*allocation)),
           rep(0, N - round(N*allocation)))

  PF_names <- c(PF_cont_names, PF_bin_names)
  EM_names <- c(EM_cont_names, EM_bin_names)

  design_mat <- cbind(Intercept = 1,
                      as.matrix(final_data),
                      trt = trt)

  interaction_mat <- as.matrix(final_data[, EM_names, drop = FALSE]) * trt
  final_design_mat <- cbind(design_mat, interaction_mat)

  b_prognostic <- c(b_X, b_X_bin)
  b_interaction <- c(b_EM, b_EM_bin)
  b_main_EM <- rep(0, length(EM_names))  # ?

  betas <- c(b_0, b_prognostic, b_main_EM, b_trt, b_interaction)

  linear_pred <- final_design_mat %*% betas

  y <- switch(
    family$family,
    "binomial" = rbinom(n=N, size=1, prob=family$linkinv(linear_pred)),
    "gaussian" = rnorm(n=N, mean=linear_pred, sd=sigma),
    "poisson"  = rpois(n=N, lambda=family$linkinv(linear_pred)),
    stop("Unsupported family.")
  )

  return(data.frame(final_data, trt, y))
}
