# Script to get the phi-metric for the parallel chain

path <- "./Results/Parallel_results"
# path <- "./Results/Parallel_results_noise"
folders <- c(1:30)

results <- data.frame(minOne = rep(NA, 6), Zero = rep(NA, 6))
rownames(results) <- c("P1", "P2", "P3", "P4", "P5", "P6")
experiment_results <- list(P1 = c(), P2 = c(), P3 = c(), P4 = c(), P5 = c(), P6 = c())
zeros <- c()

for (experiment in c("P1", "P2", "P3", "P4", "P5", "P6")) {
  count_min_one <- 0
  tp_min_one <- 0
  count_zero <- 0
  tp_zero <- 0
  zeros <- c(zeros, experiment)
  
  for (folder in folders) {
    experiment_path <- file.path(path, folder)
    merged_result <- read.csv(paste(experiment_path, "/merged_results.csv", sep = ""))
    
    if (!any(merged_result$Experiment == experiment)) {
      next
    }
    
    merged_row <- merged_result[merged_result$Experiment == experiment, ]
    true_positive <- FALSE
    already_checked <- FALSE
    merged_result <- apply(merged_row[c("F1", "F2", "F3", "F4")], 1, any)[[1]]
    for (chain in c(1:4)) {
      chain_result <- read.csv(paste(experiment_path, "/results_", chain, ".csv", sep = ""))
      correct_row <- chain_result[chain_result$Experiment == experiment, ]
      true_positive <- apply(correct_row[c("F1", "F2", "F3", "F4")], 1, any)
      if (true_positive) {
        if (!already_checked) {
          count_min_one <- count_min_one + 1
          if (merged_result){
            tp_min_one <- tp_min_one + 1
          } 
          already_checked <- TRUE
        }
      }
    }
    if (already_checked && !merged_result) {
      experiment_results[[experiment]] <- c(experiment_results[[experiment]], folder)
    }
    if (!already_checked) {
      count_zero <- count_zero + 1
      tp_zero <- tp_zero + apply(merged_row[c("F1", "F2", "F3", "F4")], 1, function(x) as.integer(any(x)))
      if (apply(merged_row[c("F1", "F2", "F3", "F4")], 1, any)) {
        zeros <- c(zeros, folder)
      }
    }
  }
  
  results[experiment, "minOne"] <- tp_min_one / count_min_one
  results[experiment, "Zero"] <- tp_zero / count_zero
}

# Print results for debugging
print(results)
print(experiment_results)
