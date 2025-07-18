#

# Define covariate types using the constructors

covariate_info = list(
  X1 = list(type = continuous(mean = 0, sd = 1),
            role = "prognostic"),
  X2 = list(type = binary(prob = 0.5),
            role = "effect_modifier"),
  X3 = list(type = continuous(mean = 10, sd = 2),
            role = c("prognostic", "effect_modifier")),
  X4 = list(type = continuous(mean = 5, sd = 1),
            role = c("prognostic", "effect_modifier"))
)

# Correlation matrix
cor_mat <- matrix(c(
  1.0, 0.5, 0.2, 0.1,
  0.5, 1.0, 0.2, 0.3,
  0.2, 0.2, 1.0, 0.2,
  0.1, 0.3, 0.2, 1.0),
  nrow = 4,
  byrow = TRUE,
  dimnames = list(c("X1", "X2", "X3", "X4"),
                  c("X1", "X2", "X3", "X4")))

# coefficients for prognostic factors
b_prog <- c(X1 = 0.7, X3 = -0.3, X4 = 2.0)

# coefficients for effect modifiers
b_em <- c(X2 = 1.5, X3 = 0.8, X4 = 1.1)

# generate data
BC.IPD <- gen_data(
  N = 1000,
  b_0 = 10,
  b_trt = 2,
  covariate_defns = covariate_info,
  b_prognostic = b_prog,
  b_effect_modifier = b_em,
  cor_matrix = cor_mat_for_cont_covs,
  treatment_assignment = list(prob_trt1 = 0.5),
  error_params = list(sd = 5),
  family = gaussian(link = "identity")
)

head(BC.IPD)
summary(BC.IPD)
str(BC.IPD)
