

x <- gen_data(
 N = 1000,
 b_trt = log(0.17),
 # continuous
 b_X = -log(0.5),
 b_EM = -log(0.67),
 meanX = c(0.6, 0.6),
 sdX = c(0.4, 0.4),
 meanX_EM = c(0.6, 0.6),
 sdX_EM = c(0.4, 0.4),
 #
 b_0 = -0.62,
 corX = 0.2,
 allocation = 2/3)

head(x)

apply(x, 2, \(x) round(mean(x), 1))

x <- gen_data(
 N = 1000,
 b_trt = log(0.17),
 # continuous
 b_X = -log(0.5),
 b_EM = -log(0.67),
 meanX = c(0.6, 0.6),
 sdX = c(0.4, 0.4),
 meanX_EM = c(0.6, 0.6),
 sdX_EM = c(0.4, 0.4),
 # binomial
 b_X_bin = 0.5,
 prob_X_bin = 0.2,
 b_EM_bin = 0.3,
 prob_EM_bin = 0.1,
 #
 b_0 = -0.62,
 corX = 0.2,
 allocation = 2/3)

head(x)

apply(x, 2, \(x) round(mean(x), 1))

x <- gen_data(
 N = 1000,
 b_trt = log(0.17),
 # binomial
 b_X_bin = 0.5,
 prob_X_bin = 0.2,
 b_EM_bin = 0.3,
 prob_EM_bin = 0.1,
 #
 b_0 = -0.62,
 corX = 0.2,
 allocation = 2/3)

head(x)

apply(x, 2, \(x) round(mean(x), 2))
