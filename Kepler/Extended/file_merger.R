# Only used to merge files to a csv file

# The parent directory containing the 30 folders
parent_dir <- "./Results/Single_results"

# Get all folder names (1 to 30)
folder_names <- as.character(1:30)

# Initialize an empty list to store data
all_data <- list()

# Loop through each folder and read the corresponding CSV file
for (folder in folder_names) {
  file_path <- file.path(parent_dir, folder, paste0("results_", folder, ".csv"))
  
  if (file.exists(file_path)) {  # Check if the file exists
    data <- read.csv(file_path)  
    all_data[[folder]] <- data  # Store data in the list
  } else {
    message("File not found: ", file_path)
  }
}

# Combine all data frames into one
merged_data <- do.call(rbind, all_data)

# Save the merged file
write.csv(merged_data, file.path(parent_dir, "merged_results.csv"), row.names = FALSE)

message("Merged CSV saved as merged_results.csv")
