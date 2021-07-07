source("config.R")
require(cronR)
# list the contents of a crontab
cron_ls()
# list the full path of where the rscript is located
path <- "validateResponse.R"
# Create a command to execute an R-script
cmd <- cron_rscript(path, workdir = system("pwd", intern = TRUE))
# add the command and specify the days/times to start
cron_add(
  command = cmd, frequency = "minutely", days_of_week = c(0:7),
  id = "braTs", description = "validateJob"
)
# kill the job by id
cron_rm(id = "braTs")
