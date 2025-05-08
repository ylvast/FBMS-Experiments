# Script that runs the single-chain experiments using multiple cores

# Packages and config file
source("./config.R")
library(devtools)
install_github("ylvast/GMJMCMC@FBMSY")
library(FBMS)
library(dplyr)
library(parallel)

train <- read.csv("./train.csv")
test <- read.csv("./test.csv")

# Result folder
now <- format(Sys.time(), "%Y-%m-%d_%H_%M")
dir_path <- now
dir.create(dir_path)

# Simple checks
common_rows <- inner_join(train, test, by = names(train))
if (nrow(common_rows) != 0) {
  stop("Error: There are common rows between training and testing sets.")
}

# Name of each experiment, same order as in thesis table
experiment_names <- c("S1","S2","S3","S4","S5","S6","P1","P2","P3","P4","P5","P6")

experiment_func <- function(chains,P,ninit,nfinal,params,probs,transforms,ex,seed){
  set.seed(seed)
  if (chains != 1){
    time_taken <- system.time({
      model <- fbms(formula = Rings ~ ., runs = chains, cores = chains, data = train, 
                    transforms = transforms, method = "gmjmcmc.parallel", probs = probs,
                    params = params, P = P, N.init = ninit, N.final = nfinal, verbose = FALSE)
    })
  } else {
    time_taken <- system.time({
      model <- fbms(formula = Rings ~ ., data = train, transforms = transforms,
                    method = "gmjmcmc", probs = probs, params = params, P = P, 
                    N.init = ninit, N.final = nfinal, verbose = FALSE)
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
  preds <- predict(model,test[,-1])$aggr$mean
  mae <- mean(abs(preds-test$Rings))
  
  # RMSE
  rmse <- sqrt(mean((preds-test$Rings)^2))
  
  # Correlation
  cor <- cor(preds, test$Rings, method = "pearson", use = "complete.obs")
  
  # Time elapsed
  elapsed_time <- time_taken["elapsed"]
  
  
  # Write to csv
  current_results <- data.frame(
    Experiment = experiment_names[ex],
    Run = seed,
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
  Results <- paste(dir_path,"/results_",seed,".csv", sep="")
  if (file.exists(Results)) {
    write.table(current_results, Results, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
  } else {
    write.csv(current_results, Results, row.names = FALSE)
  }
}

for (ex in c(1:6)) {
  # To specify in model
  chains <- abalone_config$B[ifelse(ex <= 6, 1, 2)]
  P <- abalone_config$P[ifelse(ex <= 6, 1, 2)]
  ninit <- abalone_config$N_init
  nfinal <- abalone_config$N_final
  transforms <- abalone_config$transforms
  
  # Fix params and probs
  probs <- gen.probs.gmjmcmc(transforms)
  params <- gen.params.gmjmcmc(train)
  if (ex %in% c(1,2,7,8)){
    probs$gen <- abalone_config$probs[[1]]
  } else if (ex %in% c(3,4,9,10)){
    probs$gen <- abalone_config$probs[[2]]
  } else if (ex %in% c(5,6,11,12)){
    probs$gen <- abalone_config$probs[[3]]
  }
  params$feat$pop.max <- abalone_config$Q[((ex-1) %% 2)+1]
  params$feat$D <- abalone_config$D
  params$feat$L <- abalone_config$L
  params$feat$esp <- abalone_config$eps
  params$loglik$var = "unknown"
  mclapply(seq_len(abalone_config$count), function (i) experiment_func(chains,P,ninit,nfinal,params,probs,transforms,ex,i), mc.cores=abalone_config$count)
}