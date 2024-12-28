# Load required libraries
library(tibble)
library(dplyr)

# Define the function to get the repository structure
get_repo_structure <- function(repo_path = ".", output_path = "utils/repo_file_tree.csv") {
  # List all files and directories recursively
  all_files <- list.files(path = repo_path, recursive = TRUE, full.names = TRUE)
  
  # Filter out unwanted files (like `.Rproj.user`)
  filtered_files <- all_files[!grepl("\\.Rproj\\.user", all_files)]
  
  # Create a data frame (tibble) for a structured view of the file tree
  file_tree <- tibble(
    path = filtered_files,
    type = ifelse(file.info(filtered_files)$isdir, "directory", "file")
  )
  
  # Ensure the output directory exists
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Save the file tree to the specified CSV
  write.csv(file_tree, file = output_path, row.names = FALSE)
  
  return(file_tree)
}

# Test the function: Save the file tree to `utils/repo_file_tree.csv`
repo_structure <- get_repo_structure()
print(paste("File tree saved to:", "utils/repo_file_tree.csv"))
