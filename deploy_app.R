# rsconnect::setAccountInfo(name=Sys.getenv("rsconnect_name"), 
#                           token=Sys.getenv("rsconnect_token"), 
#                           secret=Sys.getenv("rsconnect_secret"))


rsconnect::deployApp(
  appDir = getwd(),
  appFiles = c("Ecobat2.R",
               "server.R",
               "ui.R",
               "www/",
               "droptoken.rds",
               "Nightly.Rmd"),
  appName = "Ecobat2",
  appTitle = "EcoBat"
)



rsconnect::showLogs()
