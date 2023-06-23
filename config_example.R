# fill information in the double string. Note: questions need to match in the google form

config <- list(
  google_form_url = "", # link to your google form
  google_sheet_url = "", # link to your google sheet reponse
  first_name_question = "", # your google form question for first name
  last_name_question = "", # your google form question for last name
  username_question = "", # your google form question for user name
  preregistrant_teamID = "", # registrant team ID
  validated_teamID = "", # validated team ID
  your_email_address = "", # email address that have access to google form/sheet
  pat = "", # synapse PAT which can be generated here: https://www.synapse.org/#!PersonalAccessTokens:0
)

#### example
# config <- list(
#   google_form_url = "https://docs.google.com/forms/1w23213124asd" 
#   google_sheet_url = "https://docs.google.com/spreadsheets/1w23213124asd",
#   first_name_question = "What is your first name?",
#   last_name_question = "What is your last name?",
#   userID_question = "What is your synapse user name?",
#   pregistrant_teamID = "3332433",
#   validate_teamID = "3435342",
#   your_email_address = "brats@sagebase.org",
#   pat = "123456abcd"
# )
