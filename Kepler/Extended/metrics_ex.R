# Script to get metrics from the extended experiments
source("../helpers.R")
library(FBMS)

# Single results without noise, comment in
results <- read.csv("./Results/Single_results.csv")
feature_results <- read.csv("./Results/Single_features.csv")
colnames <- c("S1","S2","S3","S4","S5","S6")

# Parallel results without noise, comment in
# results <- read.csv("./Results/Parallel_results.csv")
# feature_results <- read.csv("./Results/Parallel_features.csv")
# colnames <- c("P1","P2","P3","P4","P5","P6")

# Single results without noise, comment in
# results <- read.csv("./Results/Single_results_noise.csv")
# feature_results <- read.csv("./Results/Single_features_noise.csv")
# colnames <- c("S1","S2","S3","S4","S5","S6")

# Parallel results with noise, comment in
# results <- read.csv("./Results/Parallel_results_noise.csv")
# feature_results <- read.csv("./Results/Parallel_features_noise.csv")
# colnames <- c("P1","P2","P3","P4","P5","P6")

rownames <- c("Pow","F1","F2","F3","F4","Count_P","Count_FP","FDR","Correlation_P","Correlation_FP","MAE","Time","Correlation_P_Med",
              "Correlation_P_Max","Correlation_P_Min","Correlation_FP_Med","Correlation_FP_Max","Correlation_FP_Min","MAE_Med","MAE_Max",
              "MAE_Min", "R_Med", "R_Min", "R_Max", "LMP_Med", "LMP_Min", "LMP_Max", "pos_Med","pos_Min","pos_Max")
Metrics <- data.frame(matrix(ncol = length(colnames), nrow = length(rownames)))
colnames(Metrics) <- colnames
rownames(Metrics) <- rownames

for (experiment in colnames) {
  exp_df <- results[results$Experiment==experiment,]
  features_df <- feature_results[feature_results$Experiment==experiment,]
  exp_df <- exp_df[rowSums(is.na(exp_df)) != ncol(exp_df), ]
  Metrics["Pow",experiment] <- mean(apply(exp_df[c("F1","F2","F3","F4")], 1, any))
  Metrics["F1",experiment] <- sum(as.logical(exp_df$F1))
  Metrics["F2",experiment] <- sum(as.logical(exp_df$F2))
  Metrics["F3",experiment] <- sum(as.logical(exp_df$F3)) 
  Metrics["F4",experiment] <- sum(as.logical(exp_df$F4))
  Metrics["Count_P",experiment] <- mean(as.integer(exp_df$Count_P)) 
  Metrics["Count_FP",experiment] <- mean(as.integer(exp_df$Count_FP))
  Metrics["FDR",experiment] <- mean(as.integer(exp_df$Count_FP)/as.integer(exp_df$Count_P))
  Metrics["Correlation_P",experiment] <- mean(as.numeric(exp_df$Correlation))
  Metrics["Correlation_FP",experiment] <- mean(as.numeric(exp_df$Correlation_FP), na.rm = TRUE)
  Metrics["Correlation_P_Med",experiment] <- median(as.numeric(exp_df$Correlation))
  Metrics["Correlation_FP_Med",experiment] <- median(as.numeric(exp_df$Correlation_FP), na.rm = TRUE)
  Metrics["Correlation_P_Max",experiment] <- max(as.numeric(exp_df$Correlation))
  Metrics["Correlation_FP_Max",experiment] <- max(as.numeric(exp_df$Correlation_FP), na.rm = TRUE)
  Metrics["Correlation_P_Min",experiment] <- min(as.numeric(exp_df$Correlation))
  Metrics["Correlation_FP_Min",experiment] <- min(as.numeric(exp_df$Correlation_FP), na.rm = TRUE)
  Metrics["MAE",experiment] <- mean(as.numeric(exp_df$MAE), na.rm = TRUE)
  Metrics["MAE_Med",experiment] <- median(as.numeric(exp_df$MAE), na.rm = TRUE)
  Metrics["MAE_Min",experiment] <- min(as.numeric(exp_df$MAE), na.rm = TRUE)
  Metrics["MAE_Max",experiment] <- max(as.numeric(exp_df$MAE), na.rm = TRUE)
  Metrics["LMP_Med",experiment] <- median(as.numeric(exp_df$LMP), na.rm = TRUE)
  Metrics["LMP_Min",experiment] <- min(as.numeric(exp_df$LMP), na.rm = TRUE)
  Metrics["LMP_Max",experiment] <- max(as.numeric(exp_df$LMP), na.rm = TRUE)
  Metrics["R_Med",experiment] <- median(as.numeric(exp_df$R2), na.rm = TRUE)
  Metrics["R_Min",experiment] <- min(as.numeric(exp_df$R2), na.rm = TRUE)
  Metrics["R_Max",experiment] <- max(as.numeric(exp_df$R2), na.rm = TRUE)
  list_of_margs <- max_prob_tp(10,features_df)
  Metrics["pos_Med",experiment] <- median(list_of_margs[list_of_margs>0.1], na.rm = TRUE)
  Metrics["pos_Min",experiment] <- min(list_of_margs[list_of_margs>0.1], na.rm = TRUE)
  Metrics["pos_Max",experiment] <- max(list_of_margs, na.rm = TRUE)
  Metrics["Time",experiment] <- mean(as.numeric(exp_df$Time))
}

Metrics

