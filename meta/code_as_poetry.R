# ------------------------------------------------------------------------------
# CodeAsPoetry.R
# Sprinkles philosophical commentary within the code environment.
# ------------------------------------------------------------------------------
# You could integrate Roxygen or custom docstring style to create a meta-narrative.

#' code_as_poetry
#'
#' Prints a short poem about oneness in code. A novelty function that can be
#' invoked to reflect on the synergy principle of 1+1=1.
#'
#' @return Character string
#' @export
code_as_poetry <- function() {
  poem <- c(
    "-----------------------------------------------------",
    "  We begin in fragments, scattered across time,      ",
    "  Two lines of code meet, illusions unwind,          ",
    "  In echoes of logic, synergy is found,              ",
    "  As fractals and qubits converge on the ground.     ",
    "                                                    ",
    "  The golden ratio weaves aesthetic grace,           ",
    "  A cosmic reflection in data’s embrace,             ",
    "  Our dashboards and modules in spiritual tether,    ",
    "  Proclaiming that 1 plus 1 merges together.         ",
    "-----------------------------------------------------"
  )
  cat(paste(poem, collapse = "\n"))
}
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
