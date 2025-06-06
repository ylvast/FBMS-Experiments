# Script to run parallel-chain experiments, extended

source("./config_ex.R")
source("../helpers.R")
library(devtools)
install_github("ylvast/GMJMCMC@FBMSY")
library(FBMS)
library(dplyr)
library(parallel)

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

# Runs and saves all needed information from each run
experiment_func <- function(path,P,ninit,nfinal,params,probs,transforms,ex,chain_number,run,model=NULL){
  # Must set unique seed /// CHECK
  unique_seed <- 4*(run-1)+chain_number+1
  set.seed(unique_seed)

  model_merge <- is.null(model)
  time_taken <- 0
  if (model_merge){
    time_taken <- system.time({
      sink(file.path(path, paste0("Output_", chain_number, ".txt")), append = TRUE)
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
  }

  # Create summary
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
    Run = run,
    F1 = F1,
    F2 = F2,
    F3 = F3,
    F4 = F4,
    Count_FP = fp,
    Correlation = cor,
    Correlation_FP = cor_fp,
    MAE = mae,
    LMP = if (!model_merge) model$reported[[1]] else max(unlist(model$best.margs)),
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
  if (model_merge){
    Results <- paste(path,"/results_",chain_number,".csv", sep="")
  } else {
    Results <- paste(path,"/merged_results.csv", sep="")
  }
  if (file.exists(Results)) {
    write.table(current_results, Results, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
  } else {
    write.csv(current_results, Results, row.names = FALSE)
  }
  
  # Adding complete list of features with probabilities
  if (model_merge){
    Results_features <- paste(path,"/features_",chain_number,".csv", sep="")
  } else {
    Results_features <- paste(path,"/merged_features.csv", sep="")
  }
  
  # Summary with lower tolerance
  summary_comp <- summary(model, tol=0.001, pop="best")
  features_comp <- summary_comp$feats.strings
  margs <- summary_comp$marg.probs
  run_data <- data.frame(Experiment = rep(experiment_names[ex],length(features_comp)), Run = rep(run, length(features_comp)), Number = paste0("Feature", 1:length(features_comp)), 
                         Feature = features_comp, Margs = margs)
  
  # If the CSV file already exists, append new rows (avoid overwriting)
  if (file.exists(Results_features)) {
    write.table(run_data, Results_features, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
  } else {
    write.csv(run_data, Results_features, row.names = FALSE)
  }
  # Return for merge
  return(model)
}

# Runs all 6 parallel experiments
for (ex in c(11)) {
  # To specify in model
  P <- experiment_config$P[((ex-1) %% 2)+3]
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
  # Run all 30 runs
  for (run in c(1:experiment_config$count)) {
    # Create folder for each run
    dir_path_run = file.path(dir_path,run+filler)
    dir.create(dir_path_run)
    # Run parallel runs
    parallel_runs <- mclapply(seq_len(experiment_config$B[2]), function (i) experiment_func(dir_path_run,P,ninit,nfinal,params,probs,transforms,ex,i,run), mc.cores=experiment_config$B[2])
    class(parallel_runs) <- "gmjmcmc_parallel"
    # Merge runs
    merged_model <- merge_results(parallel_runs, "best", 2, 0.0000001, train)
    # Save merged results 
    experiment_func(dir_path_run,P,ninit,nfinal,params,probs,transforms,ex,5,run,merged_model)
  }
}