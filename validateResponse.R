# load variable
source("setup.R")

# prepare detecting new submissions
dir.create("tmp", showWarnings = FALSE)
dir.create("log", showWarnings = FALSE)

invitations = c()
team_invitations <- syn$get_team_open_invitations(config$validated_teamID)
for (x in iterate(team_invitations)) {
  invitations <- c(invitations, syn$getUserProfile(x$inviteeId)["userName"])
}

if (!file.exists("tmp/before.csv")) {
  readr::write_csv(response, "tmp/before.csv")
} else {
  readr::write_csv(response, "tmp/after.csv")
}

# get validated_teamID name
validated_team_name <- syn$getTeam(config$validated_teamID)$name

# validation ----------------------------------------------------------
if (file.exists("tmp/after.csv")) {
  # read all characters to avoid errors for empty sheet
  old_data <- readr::read_csv(
    "tmp/before.csv", 
    col_types = cols(.default = "c")
  )
  new_data <- readr::read_csv(
    "tmp/after.csv", 
    col_types = cols(.default = "c")
  )
  
  if (!identical(old_data, new_data, ignore.environment = TRUE)) {
    # identify new submissions
    new_response <- anti_join(new_data, old_data, by = colnames(new_data)) %>%
      setNames(janitor::make_clean_names(colnames(.))) %>%
      select(timestamp, questions[[3]]) %>% 
      setNames(c("timestamp", "userName")) 

    # replace na to "NA" just in case
    new_response[is.na(new_response)] <- "NA"

    # get new submission user names
    new_usernames <- unique(new_response$userName) %>%
      stringr::str_replace("@synapse.org", "")

    # find difference of users between two teams
    diff <- pd$DataFrame(
      cu$teams$team_members_diff(
        syn,
        config$challenge_teamID,
        config$validated_teamID
      )
    )
    un <- pd$DataFrame(
      cu$teams$team_members_union(
        syn,
        config$challenge_teamID,
        config$validated_teamID
      )
    )
    team2_members <- setdiff(un$userName, diff$userName)
    team2_members <- c(team2_members, invitations)

    # find user who is in the diff, aka users in the challenge team, 
    # but not in the data access team
    waitList_users <- intersect(new_usernames, diff$userName)
    if (length(waitList_users) != 0) {
      invisible(
        lapply(waitList_users, function(usr) {
          Sys.sleep(0.5)
          id <- tryCatch({
              syn$getUserProfile(usr)$ownerId
            }, error=function(err) {
              syn$restGET(
                sprintf("/userGroupHeaders?prefix=%s", usr)
              )$children[[1]]$ownerId
            })
          
          if (usr %in% invitations) {
            subject <- paste0(config$challenge_name, " Data Access Form - Duplicate request")
            msg <- paste0(
              "Dear ", usr, ",<br><br>",
              "An email invite to join the ", validated_team_name, " has already been sent.<br/><br/>",
              "Please check your inbox or spam folder for an email from BraTS bot (brats-fets-bot@synapse.org). ",
              "You may also respond to the invite on Synapse from your ",
              "<a href='https://www.synapse.org/#!Profile:", id, "/teams'>Teams page</a>.",
              "<br/><br/> Only after joining the Data Access team will you have access to the ",
              "<a href='https://www.synapse.org/#!Synapse:", config$folder_synId,"'>challenge data</a>."
              
            )
            invisible(
              syn$sendMessage(
                userIds = list(id), messageSubject = subject,
                messageBody = msg, contentType = "text/html"
              )
            )
            log <- "pending invite response\n"
          } else {
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
              msg <- paste0(
                "Thank you for your interest in the ", config$challenge_name,"! <br/><br/>",
                "Once you click 'Join', you will be granted access to the ",
                "<a href='https://www.synapse.org/#!Synapse:", config$folder_synId,"'>challenge data</a>."
              )
              syn$invite_to_team(config$validated_teamID, user=id, message=msg)
              log <- "invite sent\n"
            } else { # if not validate
              inx <- which(a != b)
              errorMsg <- sapply(inx, function(i) {
                paste0(
                  colnames(new_response)[-1][i], ": '",
                  a[i], "' does not match the '",
                  b[i], "' in your Synapse profile<br>"
                )
              }) %>% paste0(collapse = "")
              msg <- paste0(
                "Dear ", usr, ",<br><br>",
                errorMsg, "<br>",
                "The information provided in the ", config$challenge_name, " Data Access Form ",
                "does not match any Synapse profiles. Please try ",
                "<a href='", config$google_form_url, "' target='_blank'>submitting the form</a> again."
              )
              invisible(
                syn$sendMessage(
                  userIds = list(id), messageSubject = paste0(
                    config$challenge_name, 
                    " Data Access Form - Errors Found"
                  ),
                  messageBody = msg, contentType = "text/html"
                )
              )
              log <- "mismatched names\n"
            }
          }
          cat(paste0(c(format(Sys.time(), " %Y-%m-%dT%H-%M-%S"), usr, log), collapse = ","),
            file = "log/out.log", append = TRUE
          )
          Sys.sleep(1)
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
          Sys.sleep(0.5)
          id <- tryCatch({
            syn$getUserProfile(usr)$ownerId
          }, error=function(err) {
            tryCatch({
              syn$restGET(
                sprintf("/userGroupHeaders?prefix=%s", usr)
              )$children[[1]]$ownerId  
            }, error=function(err2) {
              NA
            })
          })
          
          # if users not in the pre-registrant team, but already in the validate team, like admin
          # Email will not be sent
          if (usr %in% team2_members) {
            subject <- paste0(config$challenge_name, " Data Access Form - Access already granted")
            msg <- paste0(
              "Dear ", usr, ",<br><br>",
              "You have already joined the ", config$challenge_name, " Data Access team. You may ",
              "now access the <a href='https://www.synapse.org/#!Synapse:",
              config$folder_synId,"'>challenge data</a>."
            )
            invisible(
              syn$sendMessage(
                userIds = list(id), messageSubject = subject,
                messageBody = msg, contentType = "text/html"
              )
            )
            log <- "access already granted\n"
          } else { # if user not in either of team
    
            if (is.na(id)) {
              # if username is incorrect, then you wont' get an email, since we cant get their userId
              log <- "username not found\n"
            } else {
              subject <- paste0(config$challenge_name, " Data Access Form - Errors Found")
              msg <- paste0(
                "Dear ", usr, ",<br><br>",
                "You must first register and agree to the Terms and Conditions of the ", config$challenge_name,
                ".<br/><br/>If you are still interested in gaining acces to the challenge data, please ",
                "register for the challenge first, then submit the ", "<a href='", config$google_form_url,
                "' target='_blank'>Data Access Form</a> again."
              )
              invisible(
                syn$sendMessage(
                  userIds = list(id), messageSubject = subject,
                  messageBody = msg, contentType = "text/html"
                )
              )
              log <- "missing registration\n"
            }
          }
          cat(paste0(c(format(Sys.time(), " %Y-%m-%dT%H-%M-%S"), usr, log), collapse = ","),
            file = "log/out.log", append = TRUE
          )
          Sys.sleep(1)
        })
      )
    }
  }

  # remove cronR log and replace old
  unlink(c("validateResponse.log", "tmp/after"))
  write_csv(response, "tmp/before.csv")
}
