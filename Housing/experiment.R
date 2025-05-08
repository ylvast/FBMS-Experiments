# Script that can run all experiments, used to run the parallel-chain experiments

source("./Housing/config.R")
library(devtools)
install_github("ylvast/GMJMCMC@FBMSY")
library(FBMS)
library(dplyr)

set.seed(2024)

train <- read.csv("./train.csv")
test <- read.csv("./test.csv")

# Result csv
now <-format(Sys.time(), "%Y-%m-%d_%H_%M")
Results <- paste("results_",now,".csv", sep="")

# Simple checks
common_rows <- inner_join(train, test, by = names(train))
if (nrow(common_rows) != 0) {
  stop("Error: There are common rows between training and testing sets.")
}

# Name of each experiment, same order as in thesis table
experiment_names <- c("S1","S2","S3","S4","S5","S6","P1","P2","P3","P4","P5","P6")

# Running the experiments
for (ex in c(7:12)){
  
  # To specify in model
  chains <- experiment_config$B[ifelse(ex <= 6, 1, 2)]
  P <- experiment_config$P[ifelse(ex <= 6, 1, 2)]
  ninit <- experiment_config$N_init
  nfinal <- experiment_config$N_final
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
  params$feat$pop.max <- experiment_config$Q[((ex-1) %% 2)+1]
  params$feat$D <- experiment_config$D
  params$feat$L <- experiment_config$L
  params$feat$esp <- experiment_config$eps
  params$feat$check.col <- F
  params$loglik$var = "unknown"
  
  
  # Run each experiment count times
  for (number in c(1:experiment_config$count)){
    # The model
    if (chains != 1){
      time_taken <- system.time({
        model <- fbms(formula = MEDV ~ ., runs = chains, cores = chains, data = train, 
                      transforms = transforms, method = "gmjmcmc.parallel", probs = probs,
                      params = params, P = P, N.init = ninit, N.final = nfinal)
      })
    } else {
      time_taken <- system.time({
        model <- fbms(formula = MEDV ~ ., data = train, transforms = transforms,
                      method = "gmjmcmc", probs = probs, params = params, P = P, 
                      N.init = ninit, N.final = nfinal)
      })
    }
    
    summary <- summary(model, tol=0.1, pop="best")
    features <- summary$feats.strings
    
    # Three most important features 
    feature1 <- if (length(features) > 0) features[1] else "NA"
    feature2 <- if (length(features) > 1) features[2] else "NA"
    feature3 <- if (length(features) > 2) features[3] else "NA"
    feature4 <- if (length(features) > 3) features[4] else "NA"
    feature5 <- if (length(features) > 4) features[5] else "NA"
    
    # MAE 
    preds <- predict(model,test[,-14])$aggr$mean
    mae <- mean(abs(preds-test$MEDV))
    
    # RMSE
    rmse <- sqrt(mean((preds-test$MEDV)^2))
    
    # Correlation
    cor <- cor(preds, test$MEDV, method = "pearson", use = "complete.obs")
    
    # Time elapsed
    elapsed_time <- time_taken["elapsed"]
    
    # Write to csv
    current_results <- data.frame(
      Experiment = experiment_names[ex],
      Run = number,
      Correlation = cor,
      MAE = mae,
      RMSE = rmse,
      Time = elapsed_time,
      FirstFeature = feature1,
      SecondFeature = feature2,
      ThirdFeature = feature3,
      FourthFeature = feature4,
      FifthFeature = feature5
    )
    if (file.exists(Results)) {
      write.table(current_results, Results, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
    } else {
      write.csv(current_results, Results, row.names = FALSE)
    }
  }
}