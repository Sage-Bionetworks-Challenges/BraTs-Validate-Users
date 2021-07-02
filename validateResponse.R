# load variable
source("setup.R")

# prepare detecting new submissions
dir.create("tmp", showWarnings = FALSE)
dir.create("log", showWarnings = FALSE)
# tools::md5sum("tmp_old.csv") != tools::md5sum("tmp_new.csv")
if (!file.exists("tmp/before.csv")) {
  readr::write_csv(response, "tmp/before.csv")
} else {
  readr::write_csv(response, "tmp/after.csv")
}

# validation ----------------------------------------------------------
if (file.exists("tmp/after.csv")) {
  # read all characters to avoid errors for empty sheet
  old_data <- readr::read_csv("tmp/before.csv", col_types = cols(.default = "c"))
  new_data <- readr::read_csv("tmp/after.csv", col_types = cols(.default = "c"))

  if (!identical(old_data, new_data, ignore.environment = TRUE)) {
    print (questions)
    # identify new submissions
    new_response <- anti_join(new_data, old_data, by = colnames(new_data)) %>%
      setNames(janitor::make_clean_names(colnames(.))) %>%
      select(timestamp, questions[[3]]) %>% #questions[[1]], questions[[2]],
      setNames(c("timestamp", "userName")) #"firstName", "lastName", 

    # replace na to "NA" just in case
    new_response[is.na(new_response)] <- "NA"
    # get new submission user names
    new_usernames <- unique(new_response$userName)
    print (new_usernames)
    # find difference of users between two teams
    diff <- pd$DataFrame(
      cu$teams$team_members_diff(
        syn,
        config$preregistrant_teamID,
        config$validated_teamID
      )
    )

    un <- pd$DataFrame(
      cu$teams$team_members_union(
        syn,
        config$preregistrant_teamID,
        config$validated_teamID
      )
    )

    team2_members <- setdiff(un$userName, diff$userName)

    footer <- "Thank you!<br><br>Challenge Administrator"
    # find user who is in the diff, aka users in the pre-registrant team, but not in the validate team
    waitList_users <- intersect(new_usernames, diff$userName)

    if (length(waitList_users) != 0) {
      invisible(
        lapply(waitList_users, function(usr) {

          id <- syn$getUserProfile(usr)["ownerId"]

          # compare first name, last name and user name
          a <- new_response %>%
            filter(userName == usr) %>%
            # only take the latest submission
            filter(timestamp == max(timestamp)) %>%
            select(-timestamp) %>%
            as.character()
          b <- diff %>%
            filter(userName == usr) %>%
            # firstName, lastName, 
            select(userName) %>%
            as.character()

          if (identical(a, b)) { # if validate
            # invite to the team
            syn$invite_to_team(config$validated_teamID, id)
            msg <- paste0(
              "Hello ", usr, ",<br><br>",
              "An invitation to join the data access synapse team (BraTS 2021 Challenge Participants) has been sent, please accept and join.<br><br>",
              "Once you join, you'll be able to download the BraTS 2021 Challenge training datset <a href='https://www.synapse.org/#!Synapse:syn25909708'>here</a>.",
              footer
            )
            cat(paste0(c(format(Sys.time(), " %Y-%m-%dT%H-%M-%S"), usr, "validate\n"), collapse = ","),
              file = "log/out.log", append = TRUE
            )
          } else { # if not validate
            inx <- which(a != b)
            errorMsg <- sapply(inx, function(i) {
              paste0(
                colnames(new_response)[-1][i], ": '",
                a[i], "' does not match the '",
                b[i], "' in your synapse profile<br>"
              )
            }) %>% paste0(collapse = "")
            msg <- paste0(
              "Hello ", usr, ",<br><br>",
              errorMsg, "<br>",
              "Please make sure the information you put into the google form matches your synapse profile ",
              "and submit the <a href='", config$google_form_url, "' target='_blank'>google form</a>", " again.<br><br>",
              footer
            )
            cat(paste0(c(format(Sys.time(), " %Y-%m-%dT%H-%M-%S"), usr, "mismatched names\n"), collapse = ","),
              file = "log/out.log", append = TRUE
            )
          }
          invisible(
            syn$sendMessage(
              userIds = list(id), messageSubject = "Form Response Validation Results",
              messageBody = msg, contentType = "text/html"
            )
          )
        })
      )
    }

    # find who is not in the diff:
    # they could not in the preregistrant team yet or
    # already in the validated team
    not_waitList_users <- setdiff(new_usernames, diff$userName)

    if (length(not_waitList_users) != 0) {
      invisible(
        lapply(not_waitList_users, function(usr) {
          # if users not in the pre-registrant team, but already in the validate team, like admin
          if (usr %in% team2_members) {
            msg <- paste0(
              "Hello ", usr, ",<br><br>",
              "You have already filled out the google form. You can access the ",
              "BraTS Challenge training data <a href='https://www.synapse.org/#!Synapse:syn25909708'>here</a>.<br><br>",
              footer
            )
            id <- syn$getUserProfile(usr)["ownerId"]
            cat(paste0(c(format(Sys.time(), " %Y-%m-%dT%H-%M-%S"), usr, "already in the validated team\n"), collapse = ","),
              file = "log/out.log", append = TRUE
            )
            invisible(
              syn$sendMessage(
                userIds = list(id), messageSubject = "Form Response Validation Results",
                messageBody = msg, contentType = "text/html"
              )
            )
          } else { # if user not in either of team
            msg <- paste0(
              "Hello ", usr, ",<br><br>",
              "You have not first registered with the BraTS Challenge prior to filling out the google form.<br><br>",
              "Please <a href='https://www.synapse.org/#!Synapse:syn25829070/wiki/611101'>register</a> with the challenge first and then ",
              "submit the <a href='", config$google_form_url, "' target='_blank'>google form</a>", " again.<br><br>",
              footer
            )
            id <- try(syn$getUserProfile(usr), silent = TRUE)["ownerId"] # hope user don't give crazy username, "null", "NUL", ""

            if (is.na(id)) {
              # if username is incorrect, then you wont' get an email, since we cant get their userId
              cat(paste0(c(format(Sys.time(), " %Y-%m-%dT%H-%M-%S"), usr, "username not found\n"), collapse = ","),
                file = "log/out.log", append = TRUE
              )
            } else {
              cat(paste0(c(format(Sys.time(), " %Y-%m-%dT%H-%M-%S"), usr, "not in the preregistrant team\n"), collapse = ","),
                file = "log/out.log", append = TRUE
              )
              invisible(
                syn$sendMessage(
                  userIds = list(id), messageSubject = "Form Response Validation Results",
                  messageBody = msg, contentType = "text/html"
                )
              )
            }
          }
        })
      )
    }
  }

  # remove cronR log and replace old
  unlink(c("validateResponse.log", "tmp/after"))
  write_csv(response, "tmp/before.csv")
}
