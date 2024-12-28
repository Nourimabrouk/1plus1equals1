# ------------------------------------------------------------------------------
# meta_reflect.R
# Explores the codebase and merges synergy across files.
# ------------------------------------------------------------------------------
library(dplyr)
library(purrr)
library(tidyr)

#' meta_reflect_repo
#'
#' Reads all .R files in a repo to introspect how they might unify.
#'
#' @param repo_path Path to the repository
#' @return A tibble with file and code lines
#' @export
meta_reflect_repo <- function(repo_path = ".") {
  files <- list.files(repo_path, recursive = TRUE, pattern = "\\.R$")
  tibble(file = files) %>%
    mutate(code = map(file, ~ readLines(file.path(repo_path, .x)))) %>%
    unnest_longer(code)
}

#' synergy_map
#'
#' Builds a simple synergy map showing how many times "1+1=1" or "synergy"
#' or similar keywords appear across files.
#'
#' @param df A tibble as from meta_reflect_repo
#' @return A tibble with synergy keyword counts per file
#' @export
synergy_map <- function(df) {
  keywords <- c("1+1=1", "synergy", "oneness", "unity")
  df %>%
    group_by(file) %>%
    summarize(
      synergy_hits = sum(map_int(code, ~ sum(str_detect(.x, keywords))))
    ) %>%
    ungroup() %>%
    arrange(desc(synergy_hits))
}
