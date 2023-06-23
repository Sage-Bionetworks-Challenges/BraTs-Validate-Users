Sys.setenv(TZ = "UTC")

suppressPackageStartupMessages({
  library(googlesheets4)
  library(reticulate)
  library(dplyr)
  library(readr)
  library(janitor)
})

# setwd(dirname(dirname(rstudioapi::getSourceEditorContext()$path)) # uncomment for rstudio
# setwd(system("pwd", intern = TRUE)) # uncomment for Rscript

# read config
source("config.R")

# TODO: add testing for input, now assume config is fine
# clean up config info
config <- lapply(config, function(x) toString(x) %>% trimws("both"))
if (any(sapply(config, function(i) i == ""))) stop("config information not completed")

questions <- lapply(
  config[c(
    "first_name_question",
    "last_name_question",
    "username_question"
  )],
  janitor::make_clean_names
)

# hide warning
options(gargle_oauth_email = config$your_email_address) # for googlesheet

use_condaenv("brats-tool", required = TRUE)

cu <- reticulate::import("challengeutils")
pd <- reticulate::import("pandas")
synapseclient <- reticulate::import("synapseclient")

# log in to synapse
syn <- synapseclient$Synapse()
syn$login(authToken = config$pat, silent = TRUE)

# Reading google sheet from response of form
# need to successfully authenticate once in an interactive session
# token will be store automatically; gs4_has_token()

# FIXME: on instance, token is not stored and/or retreived successfully
#        when in non-interactive mode. Using gs4_deauth() for now to
#        prevent attempt of getting credentials.
gs4_deauth()

suppressMessages(
  response <- remove_empty(
    read_sheet(config$google_sheet_url, trim_ws = TRUE, col_types = "c"),
    which = "rows"
  ) %>%
    setNames(janitor::make_clean_names(colnames(.)))
)

if (any(sapply(questions, function(i) !i %in% colnames(response)))) stop("not all questions matched")
