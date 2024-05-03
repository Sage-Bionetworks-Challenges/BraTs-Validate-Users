######################################################
## Fill information in the double string. 
##
## Note: questions need to match in the Google Form
######################################################

config <- list(
  google_form_url = "", # link to your google form
  google_sheet_url = "", # link to your google sheet reponse
  
  first_name_question = "", # your google form question for first name
  last_name_question = "", # your google form question for last name
  username_question = "", # your google form question for user name
  
  challenge_name = "",  # name of the challenge
  challenge_teamID = "", # participant team ID
  validated_teamID = "", # data access team ID
  
  your_email_address = "", # email address that have access to google form/sheet
  pat = "" # synapse PAT which can be generated here: https://www.synapse.org/#!PersonalAccessTokens:0
)

######################################################
## Example
######################################################
# config <- list(
#   google_form_url = "https://docs.google.com/forms/1w23213124asd" 
#   google_sheet_url = "https://docs.google.com/spreadsheets/1w23213124asd",
#
#   first_name_question = "What is your first name?",
#   last_name_question = "What is your last name?",
#   username_question = "What is your synapse user name?",
#
#   challenge_name = "BraTS Challenge",
#   challenge_teamID = "3332433",
#   validated_teamID = "3435342",
#
#   your_email_address = "brats@sagebase.org",
#   pat = "123456abcd==="
# )
