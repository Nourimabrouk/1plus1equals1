library(dplyr)
library(tidyverse)

collate_R_files <- function(output_base = "collated_code", format = c("txt", "md"), max_lines = Inf) {
  format <- match.arg(format)
  file_ext <- paste0(".", format)
  r_files <- list.files(pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  
  if (length(r_files) == 0) {
    stop("No R files found in the current repository!")
  }
  
  all_content <- r_files %>%
    set_names(basename) %>%
    map(~ readLines(.x, warn = FALSE)) %>%
    map(~ .[!grepl("^\\s*(#|$)", .)]) %>% # Remove comments and blank lines
    imap(~ paste0("\n\n# File: ./", .y, "\n", paste(rep("-", 80), collapse = ""), "\n\n", paste(.x, collapse = "\n")))
  
  output_text <- paste(unlist(all_content), collapse = "\n")
  output_file <- paste0(output_base, file_ext)
  writeLines(output_text, output_file)
  
  message(paste0("Code collation complete. Saved to: ", output_file))
  invisible(output_file)
}

# Example usage
collate_R_files("collated_code", format = "md")