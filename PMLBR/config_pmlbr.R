# Config 

experiment_config <- list(
  count = 10,
  transforms = c("p0","troot","sigmoid","sin_deg","cos_deg"),
  eps = 0.05,
  D = 10,
  L = 5,
  P = 100,
  N_init = 1000,
  N_final = 1000,
  probs = list(c(0.22,0.22,0.06,0.06,0.22,0.22),c(0.4,0.4,0.1,0.1,0,0))
)