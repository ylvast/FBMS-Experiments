# Config for Boston Housing

experiment_config <- list(
  count = 30,
  transforms = c("troot","sigmoid","exp_dbl","p3","p0"),
  eps = 0.05,
  D = 10,
  L = 5,
  B = c(1,4),
  P = c(200,50), 
  N_init = 1000,
  N_final = 3000,
  Q = c(21,42),
  probs = list(c(0.22,0.22,0.06,0.06,0.22,0.22),c(0.3,0.3,0.1,0.1,0.1,0.1),c(0.4,0.4,0.1,0.1,0,0))
)

