# Script to run PMLBR experiments

library(pmlbr)
source("./config_pmlbr.R")
library(devtools)
install_github("ylvast/GMJMCMC@FBMSY")
library(FBMS)
results = list()

# Change to 2 if second experiment is run
ex <- 1

# Min max normalization
min_max_scale <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Config file
transforms <- experiment_config$transforms
P <- experiment_config$P
ninit <- experiment_config$N_init
nfinal <- experiment_config$N_final

# Folder for results
now <-format(Sys.time(), "%Y-%m-%d_%H_%M")
dir_path = now
dir.create(dir_path)
experiment_names <- c("S1","S2")

# Function to run experiments
experiment_func <- function(data,data_name,P,ninit,nfinal,probs,transforms,run){
  # If not done, create a folder to store results
  set.seed(run)
  path <- file.path(dir_path,run)
  dir.create(path, showWarnings = FALSE)
  
  # Make train and test
  n = dim(data)[1]
  p = dim(data)[2]
  idt = sample.int(n = n, size = as.integer(n*0.75), replace = F)
  train = data[idt,]
  test = data[-idt,]
  
  # Params
  params <- gen.params.gmjmcmc(train)
  params$feat$pop.max <- ceiling(p*1.5)
  params$feat$D <- experiment_config$D
  params$feat$L <- experiment_config$L
  params$feat$esp <- experiment_config$eps
  params$feat$check.col <- F
  params$loglik$var = "unknown"
  
  # Correct transformation probabilities
  prior <- gaussian.loglik
  to_file <- "gaussian.loglik"
  probs$gen <- experiment_config$probs[[ex]]
  
  try ({
  # Run experiment
  time_taken <- system.time({
    sink(file.path(path, "Output.txt"), append = TRUE)
    model <- fbms(formula = target ~ ., data = train, transforms = transforms,
                    method = "gmjmcmc", probs = probs, params = params, P = P,
                    N.init = ninit, N.final = nfinal, loglik.pi = prior)
    summary(model)
    sink()
    })
  
  # Create summary
  summary <- summary(model, tol=0.1, pop="best")
  features <- summary$feats.strings
  margs <- summary$marg.probs
  
  
  # Three most important features 
  feature1 <- if (length(features) > 0) features[1] else "NA"
  feature2 <- if (length(features) > 1) features[2] else "NA"
  feature3 <- if (length(features) > 2) features[3] else "NA"
  marg1 <- if (length(features) > 0) margs[1] else "NA"
  marg2 <- if (length(features) > 1) margs[2] else "NA"
  marg3 <- if (length(features) > 2) margs[3] else "NA"
  
  # MAE 
  preds <- predict(model,test)$aggr$mean
  mae <- mean(abs(preds-test$target))
  
  # R^2
  mean_y <- sum(test$target)/length(test$target)
  ss_res <- sum((test$target-preds)^2)
  ss_tot <- sum((test$target-mean_y)^2)
  r2 <- 1-ss_res/ss_tot
  
  # RMSE
  rmse <- sqrt(mean((preds-test$target)^2))
  
  # Correlation
  cor <- cor(preds, test$target, method = "pearson", use = "complete.obs")
  
  
  # Time elapsed
  elapsed_time <- time_taken["elapsed"]
  
  actual_run <- if (run > experiment_config$count) run-experiment_config$count else run
  
  # Write to csv
  current_results <- data.frame(
    Data = data_name,
    n = n,
    p = p,
    Experiment = experiment_names[ex],
    Run = actual_run,
    Correlation = cor,
    MAE = mae,
    RMSE = rmse, 
    LMP = max(unlist(model$best.margs)),
    R2 = r2,
    Count_P = length(features),
    Time = elapsed_time,
    First = feature1,
    Second = feature2,
    Third = feature3,
    FirstM = marg1,
    SecondM = marg2,
    ThirdM = marg3
  )
  
  Results <- paste(path,"/results.csv", sep="")
  if (file.exists(Results)) {
    write.table(current_results, Results, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
  } else {
    write.csv(current_results, Results, row.names = FALSE)
  }
  
  # Adding complete list of features with probabilities
  Results_features <- paste(path,"/features.csv", sep="")
  
  # To double check
  writeLines(c(paste("Prior:", to_file), paste("Probs:", toString(unlist(probs$gen)))), paste(path,"/variables.txt", sep=""))
  
  # Summary with lower tolerance
  summary_comp <- summary(model, tol=0.001, pop="best")
  features_comp <- summary_comp$feats.strings
  margs <- summary_comp$marg.probs
  run_data <- data.frame(Experiment = rep(experiment_names[ex],length(features_comp)), Run = rep(actual_run, length(features_comp)), Number = paste0("Feature", 1:length(features_comp)), 
                         Feature = features_comp, Margs = margs, Data = rep(data_name, length(features_comp)))
  
  # If the CSV file already exists, append new rows (avoid overwriting)
  if (file.exists(Results_features)) {
    write.table(run_data, Results_features, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
  } else {
    write.csv(run_data, Results_features, row.names = FALSE)
  }
  })
}

datasets <- pmlbr::regression_datasets()

for(data_name in datasets)
  
{
  
  data <-  pmlbr::fetch_data(data_name)
  data <- as.data.frame(lapply(data, min_max_scale))
  
  if(dim(data)[1]>15000)
    
    next
    
  # Initialize probs
  probs <- gen.probs.gmjmcmc(transforms)
    
  mclapply(seq_len(experiment_config$count), function (i) experiment_func(data,data_name,P,ninit,nfinal,probs,transforms,i), mc.cores=experiment_config$count)
  
}
