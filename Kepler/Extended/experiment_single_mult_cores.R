# Script to run the single-chain extended experiments for the Exoplanet dataset

source("./config_ex.R")
source("../helpers.R")
library(devtools)
install_github("ylvast/GMJMCMC@FBMSY")
library(FBMS)
library(dplyr)
library(parallel)

# Whether to use data with added noise
noise <- FALSE

if(noise){
  train <- read.csv("../train_noisy.csv")
} else{
  train <- read.csv("../train.csv")
}

test <- read.csv("../test.csv")
# Result csv
now <-format(Sys.time(), "%Y-%m-%d_%H_%M")
dir_path = file.path(now)
dir.create(dir_path)

# Name of each experiment, same order as in thesis table
experiment_names <- c("S1","S2","S3","S4","S5","S6","P1","P2","P3","P4","P5","P6")

experiment_func <- function(P,ninit,nfinal,params,probs,transforms,ex,seed){
  set.seed(seed)
  dir_path_seed = file.path(dir_path,seed)
  dir.create(dir_path_seed, showWarnings = FALSE)
  time_taken <- system.time({
    sink(file.path(dir_path_seed,"Output.txt"), append = TRUE)
    if(noise){
      model <- fbms(formula = MajorAxisNoisy ~ ., data = train, transforms = transforms,
                    method = "gmjmcmc", probs = probs, params = params, P = P,
                    N.init = ninit, N.final = nfinal)
    } else{
      model <- fbms(formula = MajorAxis ~ ., data = train, transforms = transforms,
                    method = "gmjmcmc", probs = probs, params = params, P = P,
                    N.init = ninit, N.final = nfinal)
    }
      summary(model)
      sink()
  })
  
  summary <- summary(model, tol=0.1, pop="best")
  features <- summary$feats.strings
  margs <- summary$marg.probs
  
  # True positives 
  list_tp <- any_tp(col_number=dim(train[,2:ncol(train)])[2], features)
  count_tp <- length(list_tp) # Number of true positives
  F1 <- length(any_tp(col_number=dim(train[,2:ncol(train)])[2], features,kepler_feature=c("troot(x4*x4*x6)")))>0 
  F2 <- length(any_tp(col_number=dim(train[,2:ncol(train)])[2], features,kepler_feature=c("troot(x4*x4*x9)")))>0
  F3 <- length(any_tp(col_number=dim(train[,2:ncol(train)])[2], features,kepler_feature=c("troot(x4*x4*x9*x9)")))>0
  F4 <- length(any_tp(col_number=dim(train[,2:ncol(train)])[2], features,kepler_feature=c("troot(x4*x4*x7)")))>0
  fp <- length(features) - count_tp # Number of false positives
  
  # Correlations
  cor <- average_correlation(test,features) 
  if (count_tp==0){
    cor_fp <- average_correlation(test,features) # Average correlation false positives
  } else{
    cor_fp <- average_correlation(test,features[-list_tp]) # Average correlation false positives
  }
  
  # Three most important features 
  feature1 <- if (length(features) > 0) features[1] else "NA"
  feature2 <- if (length(features) > 1) features[2] else "NA"
  feature3 <- if (length(features) > 2) features[3] else "NA"
  marg1 <- if (length(features) > 0) margs[1] else "NA"
  marg2 <- if (length(features) > 1) margs[2] else "NA"
  marg3 <- if (length(features) > 2) margs[3] else "NA"
  
  # MAE 
  preds <- predict(model,test[,-1])$aggr$mean
  mae <- mean(abs(preds-test$MajorAxis))
  
  # RMSE
  rmse <- sqrt(mean((preds-test$MajorAxis)^2))
  
  # R^2
  mean_y <- sum(test$MajorAxis)/length(test$MajorAxis)
  ss_res <- sum((test$MajorAxis-preds)^2)
  ss_tot <- sum((test$MajorAxis-mean_y)^2)
  r2 <- 1-ss_res/ss_tot
  
  
  # Time elapsed
  elapsed_time <- time_taken["elapsed"]
  
  
  # Write to csv
  current_results <- data.frame(
    Experiment = experiment_names[ex],
    Run = seed,
    F1 = F1,
    F2 = F2,
    F3 = F3,
    F4 = F4,
    Count_FP = fp,
    Correlation = cor,
    Correlation_FP = cor_fp,
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
  
  Results <- paste(dir_path_seed,"/results_",seed,".csv", sep="")
  if (file.exists(Results)) {
    write.table(current_results, Results, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
  } else {
    write.csv(current_results, Results, row.names = FALSE)
  }
  
  # Adding complete list of features with probabilities
  
  Results_features <- paste(dir_path_seed,"/features_",seed,".csv", sep="")
  summary_comp <- summary(model, tol=0.001, pop="best")
  features_comp <- summary_comp$feats.strings
  margs <- summary_comp$marg.probs
  run_data <- data.frame(Experiment = rep(experiment_names[ex],length(features_comp)), Run = rep(seed, length(features_comp)), Number = paste0("Feature", 1:length(features_comp)), 
                         Feature = features_comp, Margs = margs)
  
  # If the CSV file already exists, append new rows (avoid overwriting)
  if (file.exists(Results_features)) {
    write.table(run_data, Results_features, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
  } else {
    write.csv(run_data, Results_features, row.names = FALSE)
  }
}

for (ex in c(1:6)) {
  # To specify in model
  P <- experiment_config$P[((ex-1) %% 2)+1]
  ninit <- experiment_config$N_init[((ex-1) %% 2)+1]
  nfinal <- experiment_config$N_final[((ex-1) %% 2)+1]
  transforms <- experiment_config$transforms
  
  # Fix params and probs
  probs <- gen.probs.gmjmcmc(transforms)
  params <- gen.params.gmjmcmc(train)
  if (ex %in% c(1,2,7,8)){
    probs$gen <- experiment_config$probs[[1]]
  } else if (ex %in% c(3,4,9,10)){
    probs$gen <- experiment_config$probs[[2]]
  } else if (ex %in% c(5,6,11,12)){
    probs$gen <- experiment_config$probs[[3]]
  }
  params$feat$pop.max <- experiment_config$Q
  params$feat$D <- experiment_config$D
  params$feat$L <- experiment_config$L
  params$feat$esp <- experiment_config$eps
  params$loglik$var = "unknown"
  mclapply(seq_len(experiment_config$count), function (i) experiment_func(P,ninit,nfinal,params,probs,transforms,ex,i), mc.cores=experiment_config$count)
}