#Extended Kepler config

experiment_config <- list(
  count = 30,
  transforms = c("sigmoid","sin_deg","exp_dbl","troot"),
  eps = 0.05,
  D = 10,
  L = 5,
  B = c(1,4),
  P = c(200,500,50,125),
  N_init = c(2500,1000),
  N_final = c(2500,1000),
  Q = 15,
  probs = list(c(0.22,0.22,0.06,0.06,0.22,0.22),c(0.3,0.3,0.1,0.1,0.1,0.1),c(0.4,0.4,0.1,0.1,0,0))
)