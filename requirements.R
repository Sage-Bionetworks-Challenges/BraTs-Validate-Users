if (!suppressWarnings(require("pacman", character.only = TRUE))) {
  install.packages("pacman", repos = "https://cran.r-project.org/")
}

pkg_list <- c("googlesheets4", "reticulate", "dplyr", "readr", "janitor", "cronR", "tidyverse")

pacman::p_load(pkg_list, character.only = TRUE)


