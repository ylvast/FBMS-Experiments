# Results for extended boston housing

# results <- read.csv("./Results/Parallel_results/merged_results.csv")
results <- read.csv("./Results/Single_results/merged_results.csv")

colnames <- c("S1","S2","S3","S4","S5","S6","P1","P2","P3","P4","P5","P6")
rownames <- c("Correlation","MAE","RMSE","Time")
Metrics <- data.frame(matrix(ncol = length(colnames), nrow = length(rownames)))
colnames(Metrics) <- colnames
rownames(Metrics) <- rownames


for (experiment in colnames) {
  exp_df <- results[results$Experiment==experiment,]
  exp_df <- exp_df[rowSums(is.na(exp_df)) != ncol(exp_df), ]
  Metrics["Correlation",experiment] <- mean(exp_df$Correlation)
  Metrics["Corr_Med",experiment] <- median(as.numeric(exp_df$Correlation), na.rm = TRUE)
  Metrics["Corr_Min",experiment] <- min(as.numeric(exp_df$Correlation), na.rm = TRUE)
  Metrics["Corr_Max",experiment] <- max(as.numeric(exp_df$Correlation), na.rm = TRUE)
  Metrics["MAE",experiment] <- mean(exp_df$MAE, na.rm = TRUE)
  Metrics["MAE_Med",experiment] <- median(as.numeric(exp_df$MAE), na.rm = TRUE)
  Metrics["MAE_Min",experiment] <- min(as.numeric(exp_df$MAE), na.rm = TRUE)
  Metrics["MAE_Max",experiment] <- max(as.numeric(exp_df$MAE), na.rm = TRUE)
  Metrics["RMSE",experiment] <- mean(exp_df$RMSE, na.rm = TRUE)
  Metrics["RMSE_Med",experiment] <- median(as.numeric(exp_df$RMSE), na.rm = TRUE)
  Metrics["RMSE_Min",experiment] <- min(as.numeric(exp_df$RMSE), na.rm = TRUE)
  Metrics["RMSE_Max",experiment] <- max(as.numeric(exp_df$RMSE), na.rm = TRUE)
  Metrics["LMP_Med",experiment] <- median(as.numeric(exp_df$LMP), na.rm = TRUE)
  Metrics["LMP_Min",experiment] <- min(as.numeric(exp_df$LMP), na.rm = TRUE)
  Metrics["LMP_Max",experiment] <- max(as.numeric(exp_df$LMP), na.rm = TRUE)
  Metrics["R_Med",experiment] <- median(as.numeric(exp_df$R2), na.rm = TRUE)
  Metrics["R_Min",experiment] <- min(as.numeric(exp_df$R2), na.rm = TRUE)
  Metrics["R_Max",experiment] <- max(as.numeric(exp_df$R2), na.rm = TRUE)
  Metrics["Time",experiment] <- max(as.numeric(exp_df$Time), na.rm = TRUE)
}

Metrics
