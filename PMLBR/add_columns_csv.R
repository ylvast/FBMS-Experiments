library(pmlbr)

first <- read.csv("./Results/First_results.csv")
second <- read.csv("./Results/Second_results.csv")

dim(first)
data_names <- unique(first$Data)
data_names
for (name in data_names) {
  # Mean CORR
  data <- pmlbr::fetch_data(name)
  head(data)
  covariates <- data[, !names(data) %in% "target"] 
  cor_matrix <- cor(covariates)
  cor_upper <- cor_matrix[upper.tri(cor_matrix)]
  mean_corr <- mean(abs(cor_upper), na.rm = TRUE)
  high_corr_count <- sum(abs(cor_upper) > 0.8, na.rm = TRUE)
  
  # Update the 'Mean_CORR' and 'High_Corr_Count' columns in the 'first' data frame
  first[first$Data == name, "Mean_CORR"] <- mean_corr
  first[first$Data == name, "High_Corr_Count"] <- high_corr_count
  second[second$Data == name, "Mean_CORR"] <- mean_corr
  second[second$Data == name, "High_Corr_Count"] <- high_corr_count
}
