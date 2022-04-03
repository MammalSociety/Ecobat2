######## PRE MARKDOWN SCRIPT ###########
## AUTHOR: Charlie Le Marquand
## DATE LAST EDITED: 15/02/2022
#######################################################################################################################

#NEXT STEPS:
#When all done, everything working and it's online, go through and delete all unnecessary hashed out code
#Make sure all the print line numbers refer to the actual correct lines - use these for tracking errors when line # is not marked

#######################################################################################################################

library(tidyr)
library(dplyr)
library(tidyverse)
library(rmarkdown)
library(rsconnect) #for connecting to shiny.io
library(tinytex) #for ccreating the pdf
library(rdrop2) #for connecting to dropbox
library(lubridate)

#######################################################################################################################

print("starting Ecobat2.R script")

options(shiny.sanitize.errors=FALSE)

#when doing manually
# dataframe1 <- read.csv("EcobatDF.rdata") #changed all EcobatDF.csv files to Ecobat.rdata

token <- params$Token

#print(sort( sapply(ls(),function(x){object.size(get(x))}))) #print memory of things in local environment

#when doing from dropbox
#dataframe1 <- rdrop2::drop_download("ecobat/EcobatDF.rdata", overwrite = TRUE) #, header = TRUE) # cut out:, dtoken = token) - guidance says should find it - changed drop_read_csv to drop_get for rdata

################################
### new code for doing as rdata format...
# Create a path to a temporary file

temp <- tempfile()

# Download the file to the temporary location
rdrop2::drop_download(path = "ecobat/EcobatDF.rds",
                      local_path = temp,
                      dtoken = token)

# Read in the file
dataframe1 <- readRDS(temp)


# delete the temporary file
unlink(temp)

print("database read in")

################################

dataframe1 <- dataframe1 %>%
  select(-"X")

print("removed X column from dataframe1")

#remove 0 data from passes
dataframe1 <- dataframe1 %>%
  dplyr::filter(passes > 0)

print("removed passes from dataframe1")

######################################################

# WHICH VERSION OF PROFORMA LOADING AND PARAMS TO USE

# #when doing manually
# proforma <- read.csv("file.csv")
 # geofilter <- "Country"
 # timefilter <- "+/- 1 month from survey start date"
 # save <- "Already uploaded records, do NOT save to database"

# #when doing remotely
proforma <- params$n
geofilter <- params$GeoFilter
timefilter <- params$TimeFilter
save <- params$Save  #don't need to put as a column in database because this is covered by Sensitivity and this will be taken care of
# #in the code.
 
 #to avoid any random x columns, select so just have the real columns
 proforma <- proforma %>%
   select("Name.of.User":"Notes")  #the : says select this column to that column 

print("brought params in")

#######################################################

#renaming to match usual download csv
names(proforma)[names(proforma) == "Name.of.User"] <- "name"
proforma$name <- as.character(proforma$name)  
dataframe1$name <- as.character(dataframe1$name)  
 
names(proforma)[names(proforma) == "Contact.Email.of.User"] <- "email"
proforma$email <- as.character(proforma$email)
dataframe1$email <- as.character(dataframe1$email) 

names(proforma)[names(proforma) == "Site.name"] <- "site_name"
proforma$site_name <- as.character(proforma$site_name)
dataframe1$site_name <- as.character(dataframe1$site_name)

names(proforma)[names(proforma) == "Detector.identity"] <- "location_name"
proforma$location_name <- as.character(proforma$location_name)
dataframe1$location_name <- as.character(dataframe1$location_name)

names(proforma)[names(proforma) == "Lat"] <- "lat"
proforma$lat <- as.numeric(proforma$lat)
dataframe1$lat <- as.numeric(dataframe1$lat)

names(proforma)[names(proforma) == "Lon"] <- "lon"
proforma$lon <- as.numeric(proforma$lon)
dataframe1$lon <- as.numeric(dataframe1$lon)

names(proforma)[names(proforma) == "Date.start"] <- "date"
proforma$date <- as.character(proforma$date)
dataframe1$date <- as.character(dataframe1$date)

names(proforma)[names(proforma) == "Pass.Time"] <- "time"
proforma$time <- as.character(proforma$time)
dataframe1$time <- as.character(dataframe1$time)

names(proforma)[names(proforma) == "Species...Taxa.taxon.list"] <- "species"
proforma$species <- as.character(proforma$species) #is character in dataframe1 but factor in proforma
dataframe1$time <- as.character(dataframe1$time)

names(proforma)[names(proforma) == "Number.of.bats"] <- "number_of_bats"
proforma$number_of_bats <- as.integer(proforma$number_of_bats)
dataframe1$number_of_bats <- as.integer(dataframe1$number_of_bats)

names(proforma)[names(proforma) == "Sensitivity"] <- "sensitivity"
proforma$sensitivity <- as.character(proforma$sensitivity) 
dataframe1$sensitivity <- as.character(dataframe1$sensitivity) 

names(proforma)[names(proforma) == "When.can.your.data.be.made.public."] <- "public_date"
proforma$public_date <- as.character(proforma$public_date) 
dataframe1$public_date <- as.character(dataframe1$public_date) 

names(proforma)[names(proforma) == "Pass.Definition"] <- "pass_definition"
proforma$pass_definition <- as.character(proforma$pass_definition) 
dataframe1$pass_definition <- as.character(dataframe1$pass_definition) 

names(proforma)[names(proforma) == "Detector.Make"] <- "detector_make"
proforma$detector_make <- as.character(proforma$detector_make)
dataframe1$detector_make <- as.character(dataframe1$detector_make)

names(proforma)[names(proforma) == "Detector.model"] <- "detector_model"
proforma$detector_model <- as.character(proforma$detector_model)
dataframe1$detector_model <- as.character(dataframe1$detector_model)

names(proforma)[names(proforma) == "Detector.height.m"] <- "detector_height_m"
proforma$detector_height_m <- as.character(proforma$detector_height_m) #is character in dataframe1 but integer in proforma
dataframe1$detector_height_m <- as.character(dataframe1$detector_height_m)

names(proforma)[names(proforma) == "Roost.within.25m"] <- "roost_within_25m"
proforma$roost_within_25m <- as.character(proforma$roost_within_25m) #is character in dataframe1 and logical in proforma
dataframe1$roost_within_25m <- as.character(dataframe1$roost_within_25m)

names(proforma)[names(proforma) == "Activity.elevated.by.roost"] <- "activity_elevated_by_roost"
proforma$activity_elevated_by_roost <- as.character(proforma$activity_elevated_by_roost) #is character in dataframe1 and logical in proforma
dataframe1$activity_elevated_by_roost <- as.character(dataframe1$activity_elevated_by_roost)

names(proforma)[names(proforma) == "Linear.feature.adjacent"] <- "linear_feature_adjacent"
proforma$linear_feature_adjacent <- as.character(proforma$linear_feature_adjacent)
dataframe1$linear_feature_adjacent <- as.character(dataframe1$linear_feature_adjacent)

names(proforma)[names(proforma) == "Linear.feature.25m"] <- "linear_feature_25m"
proforma$linear_feature_25m <- as.character(proforma$linear_feature_25m)
dataframe1$linear_feature_25m <- as.character(dataframe1$linear_feature_25m)

names(proforma)[names(proforma) == "Anthropogenic.feature.adjacent"] <- "anthropogenic_feature_adjacent"
proforma$anthropogenic_feature_adjacent <- as.character(proforma$anthropogenic_feature_adjacent)
dataframe1$anthropogenic_feature_adjacent <- as.character(dataframe1$anthropogenic_feature_adjacent)

names(proforma)[names(proforma) == "Anthropogenic.feature.25m"] <- "anthropogenic_feature_25m"
proforma$anthropogenic_feature_25m <- as.character(proforma$anthropogenic_feature_25m)
dataframe1$anthropogenic_feature_25m <- as.character(dataframe1$anthropogenic_feature_25m)

names(proforma)[names(proforma) == "Temperature.c"] <- "temperature_c"
proforma$temperature_c <- as.numeric(proforma$temperature_c) #is numeric in dataframe1 and logical in proforma
dataframe1$temperature_c <- as.numeric(dataframe1$temperature_c)

names(proforma)[names(proforma) == "Rainfall"] <- "rainfall"
proforma$rainfall <- as.character(proforma$rainfall) #is character in dataframe1 and logical in proforma
dataframe1$rainfall <- as.character(dataframe1$rainfall)

names(proforma)[names(proforma) == "Wind.speed.mph"] <- "wind_speed_mph"
proforma$wind_speed_mph <- as.integer(proforma$wind_speed_mph) #is integer in dataframe1 and logical in proforma
dataframe1$wind_speed_mph <- as.integer(dataframe1$wind_speed_mph)

names(proforma)[names(proforma) == "Method.of.classification"] <- "method_of_classification"
proforma$method_of_classification <- as.character(proforma$method_of_classification)
dataframe1$method_of_classification <- as.character(dataframe1$method_of_classification)

names(proforma)[names(proforma) == "Analysis.software.used"] <- "analysis_software_used"
proforma$analysis_software_used <- as.character(proforma$analysis_software_used)
dataframe1$analysis_software_used <- as.character(dataframe1$analysis_software_used)

names(proforma)[names(proforma) == "County"] <- "county"
proforma$county <- as.character(proforma$county)
dataframe1$county <- as.character(dataframe1$county)

names(proforma)[names(proforma) == "Region"] <- "region"
proforma$region <- as.character(proforma$region)
dataframe1$region <- as.character(dataframe1$region)

names(proforma)[names(proforma) == "Country"] <- "country"
proforma$country <- as.character(proforma$country)
dataframe1$country <- as.character(dataframe1$country)

names(proforma)[names(proforma) == "Notes"] <- "notes"
proforma$notes <- as.character(proforma$notes) #is character in dataframe1 and logical in proforma
dataframe1$notes <- as.character(dataframe1$notes)

print("proforma names changed and classes fixed")

#merge lat lon column to use as location for everything else
proforma <- tidyr::unite(proforma, latlon, c(lat, lon), remove=FALSE)
proforma$latlon <- as.character(proforma$latlon)
dataframe1$latlon <- as.character(dataframe1$latlon)

proforma <- proforma %>%
  dplyr::filter(number_of_bats > 0)

proforma <- proforma %>%
  dplyr::group_by(latlon, species, date) %>%
  dplyr::mutate(passes=sum(number_of_bats))

proforma$passes <- as.numeric(proforma$passes)

print("lat and long merged, 0 data removed, pass sum calculated")

#USE THIS TO SAVE PROFORMA TO THE DATABASE
proforma$proformaORdf <- "dataframe1"
proforma <- tidyr::separate(proforma, date, sep="/", into = c("day", "month", "year"), remove = FALSE)
timestampfordata <- strftime(Sys.time())
#proforma$timestamp <- strftime(Sys.time())
proforma$timestamp <- timestampfordata
proforma$timestamp <- as.character(proforma$timestamp)
dataframe1$timestamp <- as.character(dataframe1$timestamp)

print("proformaORdf is df on proforma, separated day, month and year and added timestamp")

print(ncol(proforma))
print(ncol(dataframe1))

print(colnames(dataframe1))

#GET NEW DATA TO SAVE INTO THE DATABASE 
 if (save == "New records, save to database") {newmaster <- rbind(dataframe1, proforma)
 print("df and proforma combined to make newmaster")
  #move original master to a holding folder
  drop_move(from_path = "ecobat/EcobatDF.rdata", to_path = "holding/EcobatDF.rdata")
  print("dropbox dataframe has been moved to holding folder")
  
  #write new master into dropbox and then move into ecobat folder - having issues trying to write straight into it
  write.csv(newmaster, file = "EcobatDF.rdata")
  drop_upload("EcobatDF.rdata")
  print("newmaster is on dropbox")
  drop_move(from_path = "EcobatDF.rdata", to_path = "ecobat/EcobatDF.rdata")
  print("newmaster is in ecobat folder on dropbox")
  
  #delete the master copy in the holding folder
  drop_delete(path = "holding/EcobatDF.rdata")
  print("old master in holding has been deleted")
  
  print("data has been saved to the database")
  
 } else {print("data has already been added, data has not been saved to database")
  }


#now want to work with proforma but with proformaORdf column as proforma so can identify data when combined with database data
proforma$proformaORdf <- "proforma"
proforma$proformaORdf <- as.character(proforma$proformaORdf)
dataframe1$proformaORdf <- as.character(dataframe1$proformaORdf)

print("proformaORdf changed to proforma")

#######################################################################################################################

  #need to take duplicate data out of 'total' used. Take highest count per year and median across years.

dataframe1$year <- as.numeric(dataframe1$year)
dataframe1$passes <- as.numeric(dataframe1$passes)

print("changed year and passes from integer to numeric")

  dataframe1 <- dataframe1 %>%
    dplyr::group_by(latlon, species, year) %>%
    dplyr::mutate(max = max(passes)) #put mutate instead of summarise
  
  dataframe1 <- dataframe1 %>%
    dplyr::group_by(latlon, species) %>%
    dplyr::mutate(median = median(max))

  print("df grouped by highest per year and median across years")

  #remove passes column so distinct will work and go by the max and medians  
  dataframe1 <- dataframe1 %>%
    select(-"passes", -"max")
  
  print("removed passes and max columns from df")
  
  dataframe1 <- distinct(dataframe1)
  
  print("took out duplicates in dataframe")

  #use passes in calculations so need to make the passes of the distinct rows left the median
  dataframe1$passes <- dataframe1$median
  
  print("changed passes values to be the median for when calculating percentiles later")
  
  #now remove median so it matches up with proforma
  dataframe1 <- dataframe1 %>%
    select(-"median")
  
  print("median column removed from dataframe")
  
  #get columns in same order again, need to move passes in proforma
  proforma$passes2 <- proforma$passes
  
  proforma <- proforma %>%
    select(-"passes")
  
  proforma$passes <- proforma$passes2
  
  proforma <- proforma %>%
    select(-"passes2")
  
  dataframe1$passes <- as.numeric(dataframe1$passes)
  
  print("reordered proforma columns to match dataframe1, passes is last column, no median or max columns.")
  
######################################################################################################################
  
### filter the database to only have geo and time filters 
  
#make sure that day month and year for mutation AND df is the same - going to try integer first
proforma$day <- as.integer(proforma$day)
proforma$month <- as.integer(proforma$month)
proforma$year <- as.integer(proforma$year)

dataframe1$day <- as.integer(dataframe1$day)
dataframe1$month <- as.integer(dataframe1$month)
dataframe1$year <- as.integer(dataframe1$year)


print("classes of df day, month and year are integers, name are characters for proforma and df")


proforma$month <- as.numeric(proforma$month)
firstmonth <- min(proforma$month)
lastmonth <- max(proforma$month)
premonth <- firstmonth -1
postmonth <- lastmonth +1

proforma$month <- as.integer(proforma$month)

print("survey month etc worked out, month of proforma changed to integer")

# create the database to work from depending on the different filters the user might have asked for
# option1 = geofilter = all data and timefilter = all data

onerow <- proforma[1,]

#next need to decide which option need to use, and create it
if (geofilter == "All Data" & timefilter == "All Data") {
  dataframe2 <- dataframe1
  print("option 1 done")
} else if (geofilter == "County" & timefilter == "All Data") {
  dataframe2 <- dataframe1 %>%
    filter(county == onerow$county)
  print("option 2 done")
} else if (geofilter == "Region" & timefilter == "All Data") {
  dataframe2 <- dataframe1 %>%
    filter(region == onerow$region)
  print("option 3 done")
} else if (geofilter == "Country" & timefilter == "All Data") {
  dataframe2 <- dataframe1 %>%
    filter(country == onerow$country)
  print("option 4 done")
} else if (geofilter == "All Data" & timefilter == "+/- 1 month from survey start date") {
  dataframe2 <- dataframe1 %>%
    filter(month %in% (premonth:postmonth))
  print("option 5 done")
} else if (geofilter == "County" & timefilter == "+/- 1 month from survey start date") {
  dataframe2 <- dataframe1 %>%
    filter(county == onerow$county) %>%
    filter(month %in% (premonth:postmonth))
  print("option 6 done")
} else if (geofilter == "Region" & timefilter == "+/- 1 month from survey start date") {
  dataframe2 <- dataframe1 %>%
    filter(region == onerow$region) %>%
    filter(month %in% (premonth:postmonth))
  print("option 7 done")
} else if (geofilter == "Country" & timefilter == "+/- 1 month from survey start date") {
  dataframe2 <- dataframe1 %>%
    filter(country == onerow$country) %>%
    filter(month %in% (premonth:postmonth))
  print("option 8 done")
} else {print("error no option assigned")
}

#remove the original dataframe to save space
rm(dataframe1)

#######################################################################################################################  

#create a list of strings containing each of the species levels in the proforma NOTE has to be done to a dataframe before you've inserted all the 
#potential levels (may find you don't need the levels in the end)
listproformaspecies <- unique(proforma$species) #come from proforma

listproformaspecies <- as.character(listproformaspecies)
proforma$species <- as.factor(proforma$species)


#automatically subset based on species in the proforma
#useful stack on this: https://stackoverflow.com/questions/44422397/automatically-subset-data-frame-by-factor
for (i in 1:length(listproformaspecies)) {
  assign(paste0(listproformaspecies[i]), proforma %>% filter(species == listproformaspecies[i]), envir = .GlobalEnv) %>%
  #group by location and night (must be date started) and sum passes
  dplyr::group_by(location_name, date) %>%
    dplyr::mutate(passes = sum(number_of_bats))
}


#this creates a dataframe called "[species]df" for every species found in the proforma. tryCatch means that when it errors for species
# found in df but not in proforma it skips them and keeps going
for (i in 1:length(listproformaspecies)){
  tryCatch({
    (assign(paste0(listproformaspecies[i], "dataframe2"), dataframe2 %>% filter(species == listproformaspecies[i]), envir = .GlobalEnv))
    dev.off()
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#######################################################################################################################

#combine species from proforma and df to work out the percentiles for each 

BarbastellusALL <- tryCatch(rbind(`Barbastellus`, `Barbastellusdataframe2`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
BarbastellabarbastellusALL <- tryCatch(rbind(`Barbastella barbastellus`, `Barbastella barbastellusdataframe2`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
EptesicusserotinusALL <- tryCatch(rbind(`Eptesicus serotinus`, `Eptesicus serotinusdataframe2`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
EptesicusALL <- tryCatch(rbind(Eptesicus, Eptesicusdataframe2), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
MyotisalcathoeALL <- tryCatch(rbind(`Myotis alcathoe`, `Myotis alcathoedataframe2`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
MyotisbechsteiniiALL <- tryCatch(rbind(`Myotis bechsteinii`, `Myotis bechsteiniidataframe2`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
MyotisbrandtiiALL <- tryCatch(rbind(`Myotis brandtii`, `Myotis brandtiidataframe2`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
MyotisdaubentoniiALL <- tryCatch(rbind(`Myotis daubentonii`, `Myotis daubentoniidataframe2`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
MyotismystanicusALL <- tryCatch(rbind(`Myotis mystanicus`, `Myotis mystanicusdataframe2`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
MyotisnattereriALL <- tryCatch(rbind(`Myotis nattereri`, `Myotis nattereridataframe2`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
MyotisALL <- tryCatch(rbind(Myotis, Myotisdataframe2), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
NyctalusleisleriALL <- tryCatch(rbind(`Nyctalus leisleri`, `Nyctalus leisleridataframe2`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
NyctalusnoctulaALL <- tryCatch(rbind(`Nyctalus noctula`, `Nyctalus noctuladataframe2`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
NyctalusALL <- tryCatch(rbind(Nyctalus, Nyctalusdataframe2), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
NyctaloidALL <- tryCatch(rbind(Nyctaloid, Nyctaloiddataframe2), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
PipistrellusnathusiiALL <- tryCatch(rbind(`Pipistrellus nathusii`, `Pipistrellus nathusiidataframe2`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
PipistrelluspipistrellusALL <- tryCatch(rbind(`Pipistrellus pipistrellus`, `Pipistrellus pipistrellusdataframe2`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
PipistrelluspygmaeusALL <- tryCatch(rbind(`Pipistrellus pygmaeus`, `Pipistrellus pygmaeusdataframe2`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
PipistrellusALL <- tryCatch(rbind(Pipistrellus, Pipistrellusdataframe2), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
PlecotusauritusALL <- tryCatch(rbind(`Plecotus auritus`, `Plecotus auritusdataframe2`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
PlecotusaustriacusALL <- tryCatch(rbind(`Plecotus austriacus`, `Plecotus austriacusdataframe2`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
PlecotusALL <- tryCatch(rbind(Plecotus, Plecotusdataframe2), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
RhinolophusferrumequinumALL <- tryCatch(rbind(`Rhinolophus ferrumiquinum`, `Rhinolophus ferrumiquinumdataframe2`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
RhinolophushipposiderosALL <- tryCatch(rbind(`Rhinolophus hipposideros`, `Rhinolophus hipposiderosdataframe2`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
RhinolophusALL <- tryCatch(rbind(Rhinolophus, Rhinolophusdataframe2), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})


#Eventually loop the following process but right now think it would actually take more time for me to figure it out than to just
#copy and paste and manually switch the species. Using tryCatch so it will go past anything that doesn't exist.

#Don't think we need df and proforma again so remove them now to save space
rm(proforma, dataframe2, onerow, firstmonth, lastmonth, premonth, postmonth)

######################################################################################################################
### BARBASTELLUS ###

#make percentiles and make sure everything is numeric
#BarbastellusALL <- tryCatch(rbind(`Barbastellus`, `Barbastellusdf`), error = function(e) {problem <- data.frame(errorcol1, errorcol2)})

BarbastellusALL$percentile <- tryCatch((ecdf(BarbastellusALL$passes)(BarbastellusALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
BarbastellusALL$percentile <- tryCatch((lapply(BarbastellusALL$percentile, as.integer)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
BarbastellusALL$percentile <- tryCatch((as.numeric(BarbastellusALL$percentile)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
BarbastellusALL$passes <- tryCatch((as.numeric(BarbastellusALL$passes)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#tryCatch((BarbastellusALL$percentile <- ecdf(BarbastellusALL$passes)(BarbastellusALL$passes)*100), error = function(e) {print("hello")})
#tryCatch((BarbastellusALL$percentile <- lapply(BarbastellusALL$percentile, as.integer)), error = function(e) {print("hello")})
#tryCatch((BarbastellusALL$percentile <- as.numeric(BarbastellusALL$percentile)), error = function(e) {print("hello")})
#tryCatch((BarbastellusALL$passes <- as.numeric(BarbastellusALL$passes)), error = function(e) {print("hello")})

tryCatch((q0 <- quantile(BarbastellusALL$passes, c(.0))), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch((q1 <- quantile(BarbastellusALL$passes, c(.20))), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch((q2 <- quantile(BarbastellusALL$passes, c(.40))), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch((q3 <- quantile(BarbastellusALL$passes, c(.60))), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch((q4 <- quantile(BarbastellusALL$passes, c(.80))), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch((q5 <- quantile(BarbastellusALL$passes, c(.95))), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#Create percentile activity_level column - KEEP AN EYE ON Q0 AND Q1 at the moment they are both 1 so dataframe is always low/mderate with no
#low because all the low become low/moderate because is the same. This may be because there are loads of 1s in dataframe... 
#keep an eye and see how this progresses
tryCatch(BarbastellusALL <- BarbastellusALL%>%
  mutate(
    activity_level = case_when(
      passes >= q0 & passes < q1 ~ "low",
      passes >= q1 & passes < q2 ~ "low/moderate",
      passes >= q2 & passes < q3 ~ "moderate",
      passes >= q3 & passes < q4 ~ "moderate/high",
      passes >= q4 & passes < q5 ~ "high",
      passes >= q5 ~ "exceptional",
      
    )
  ), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#currently is the number of rows being compared to but really should be number of nights per location
tryCatch(BarbastellusALL$reference_range_size <- nrow(BarbastellusALL), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

print("Barbastellus percentiles done")

######################################################################################################################
### BARBASTELLA BARBASTELLUS###

BarbastellabarbastellusALL$percentile <- tryCatch((ecdf(BarbastellabarbastellusALL$passes)(BarbastellabarbastellusALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
BarbastellabarbastellusALL$percentile <- tryCatch((lapply(BarbastellabarbastellusALL$percentile, as.integer)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
BarbastellabarbastellusALL$percentile <- tryCatch((as.numeric(BarbastellabarbastellusALL$percentile)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
BarbastellabarbastellusALL$passes <- tryCatch((as.numeric(BarbastellabarbastellusALL$passes)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#make percentiles and make sure everything is numeric
#tryCatch((BarbastellabarbastellusALL$percentile <- ecdf(BarbastellabarbastellusALL$passes)(BarbastellusbarbastellusALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch((BarbastellabarbastellusALL$percentile <- lapply(BarbastellabarbastellusALL$percentile, as.integer)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch((BarbastellabarbastellusALL$percentile <- as.numeric(BarbastellabarbastellusALL$percentile)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch((BarbastellabarbastellusALL$passes <- as.numeric(BarbastellabarbastellusALL$passes)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

tryCatch((q0 <- quantile(BarbastellabarbastellusALL$passes, c(.0))), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch((q1 <- quantile(BarbastellabarbastellusALL$passes, c(.20))), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch((q2 <- quantile(BarbastellabarbastellusALL$passes, c(.40))), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch((q3 <- quantile(BarbastellabarbastellusALL$passes, c(.60))), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch((q4 <- quantile(BarbastellabarbastellusALL$passes, c(.80))), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch((q5 <- quantile(BarbastellabarbastellusALL$passes, c(.95))), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#Create percentile activity_level column - KEEP AN EYE ON Q0 AND Q1 at the moment they are both 1 so dataframe is always low/mderate with no
#low because all the low become low/moderate because is the same. This may be because there are loads of 1s in dataframe... 
#keep an eye and see how this progresses
tryCatch(BarbastellabarbastellusALL <- BarbastellabarbastellusALL%>%
           mutate(
             activity_level = case_when(
               passes >= q0 & passes < q1 ~ "low",
               passes >= q1 & passes < q2 ~ "low/moderate",
               passes >= q2 & passes < q3 ~ "moderate",
               passes >= q3 & passes < q4 ~ "moderate/high",
               passes >= q4 & passes < q5 ~ "high",
               passes >= q5 ~ "exceptional",
               
             )
           ), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#currently is the number of rows being compared to but really should be number of nights per location
tryCatch(BarbastellabarbastellusALL$reference_range_size <- nrow(BarbastellabarbastellusALL), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

print("Barbastellus barbastellus percentiles done")

######################################################################################################################
### EPTESICUS SEROTINUS ###

#make percentiles and make sure everything is numeric

EptesicusserotinusALL$percentile <- tryCatch((ecdf(EptesicusserotinusALL$passes)(EptesicusserotinusALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
EptesicusserotinusALL$percentile <- tryCatch((lapply(EptesicusserotinusALL$percentile, as.integer)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
EptesicusserotinusALL$percentile <- tryCatch((as.numeric(EptesicusserotinusALL$percentile)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
EptesicusserotinusALL$passes <- tryCatch((as.numeric(EptesicusserotinusALL$passes)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#tryCatch(EptesicusserotinusALL$percentile <- ecdf((EptesicusserotinusALL$passes)(EptesicusserotinusALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch(EptesicusserotinusALL$percentile <- lapply(EptesicusserotinusALL$percentile, as.integer), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch(EptesicusserotinusALL$percentile <- as.numeric(EptesicusserotinusALL$percentile), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch(EptesicusserotinusALL$passes <- as.numeric(EptesicusserotinusALL$passes), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

tryCatch(q0 <- quantile(EptesicusserotinusALL$passes, c(.0)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q1 <- quantile(EptesicusserotinusALL$passes, c(.20)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q2 <- quantile(EptesicusserotinusALL$passes, c(.40)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q3 <- quantile(EptesicusserotinusALL$passes, c(.60)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q4 <- quantile(EptesicusserotinusALL$passes, c(.80)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q5 <- quantile(EptesicusserotinusALL$passes, c(.95)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#Create percentile activity_level column - KEEP AN EYE ON Q0 AND Q1 at the moment they are both 1 so dataframe is always low/mderate with no
#low because all the low become low/moderate because is the same. This may be because there are loads of 1s in dataframe... 
#keep an eye and see how this progresses
tryCatch(EptesicusserotinusALL <- EptesicusserotinusALL%>%
           mutate(
             activity_level = case_when(
               passes >= q0 & passes < q1 ~ "low",
               passes >= q1 & passes < q2 ~ "low/moderate",
               passes >= q2 & passes < q3 ~ "moderate",
               passes >= q3 & passes < q4 ~ "moderate/high",
               passes >= q4 & passes < q5 ~ "high",
               passes >= q5 ~ "exceptional",
               
             )
           ), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#currently is the number of rows being compared to but really should be number of nights per location
tryCatch(EptesicusserotinusALL$reference_range_size <- nrow(EptesicusserotinusALL), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

print("Eptesicus serotinus percentiles done")

######################################################################################################################
### EPTESICUS ###

#make percentiles and make sure everything is numeric

EptesicusALL$percentile <- tryCatch((ecdf(EptesicusALL$passes)(EptesicusALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
EptesicusALL$percentile <- tryCatch((lapply(EptesicusALL$percentile, as.integer)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
EptesicusALL$percentile <- tryCatch((as.numeric(EptesicusALL$percentile)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
EptesicusALL$passes <- tryCatch((as.numeric(EptesicusALL$passes)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#tryCatch(EptesicusALL$percentile <- ecdf((EptesicusALL$passes)(EptesicusALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch(EptesicusALL$percentile <- lapply(EptesicusALL$percentile, as.integer), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch(EptesicusALL$percentile <- as.numeric(EptesicusALL$percentile), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch(EptesicusALL$passes <- as.numeric(EptesicusALL$passes), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

tryCatch(q0 <- quantile(EptesicusALL$passes, c(.0)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q1 <- quantile(EptesicusALL$passes, c(.20)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q2 <- quantile(EptesicusALL$passes, c(.40)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q3 <- quantile(EptesicusALL$passes, c(.60)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q4 <- quantile(EptesicusALL$passes, c(.80)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q5 <- quantile(EptesicusALL$passes, c(.95)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#Create percentile activity_level column - KEEP AN EYE ON Q0 AND Q1 at the moment they are both 1 so dataframe is always low/mderate with no
#low because all the low become low/moderate because is the same. This may be because there are loads of 1s in dataframe... 
#keep an eye and see how this progresses
tryCatch(EptesicusALL <- EptesicusALL%>%
           mutate(
             activity_level = case_when(
               passes >= q0 & passes < q1 ~ "low",
               passes >= q1 & passes < q2 ~ "low/moderate",
               passes >= q2 & passes < q3 ~ "moderate",
               passes >= q3 & passes < q4 ~ "moderate/high",
               passes >= q4 & passes < q5 ~ "high",
               passes >= q5 ~ "exceptional",
               
             )
           ), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#currently is the number of rows being compared to but really should be number of nights per location
tryCatch(EptesicusALL$reference_range_size <- nrow(EptesicusALL), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

print("Eptesicus percentiles done")

######################################################################################################################
 ### MYOTIS ALCATHOE ###
 
 #make percentiles and make sure everything is numeric

MyotisalcathoeALL$percentile <- tryCatch((ecdf(MyotisalcathoeALL$passes)(MyotisalcathoeALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
MyotisalcathoeALL$percentile <- tryCatch((lapply(MyotisalcathoeALL$percentile, as.integer)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
MyotisalcathoeALL$percentile <- tryCatch((as.numeric(MyotisalcathoeALL$percentile)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
MyotisalcathoeALL$passes <- tryCatch((as.numeric(MyotisalcathoeALL$passes)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

 #tryCatch(MyotisalcathoeALL$percentile <- ecdf((MyotisalcathoeALL$passes)(MyotisalcathoeALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 #tryCatch(MyotisalcathoeALL$percentile <- lapply(MyotisalcathoeALL$percentile, as.integer), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 #tryCatch(MyotisalcathoeALL$percentile <- as.numeric(MyotisalcathoeALL$percentile), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 #tryCatch(MyotisalcathoeALL$passes <- as.numeric(MyotisalcathoeALL$passes), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 tryCatch(q0 <- quantile(MyotisalcathoeALL$passes, c(.0)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q1 <- quantile(MyotisalcathoeALL$passes, c(.20)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q2 <- quantile(MyotisalcathoeALL$passes, c(.40)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q3 <- quantile(MyotisalcathoeALL$passes, c(.60)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q4 <- quantile(MyotisalcathoeALL$passes, c(.80)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q5 <- quantile(MyotisalcathoeALL$passes, c(.95)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 #Create percentile activity_level column - KEEP AN EYE ON Q0 AND Q1 at the moment they are both 1 so dataframe is always low/mderate with no
 #low because all the low become low/moderate because is the same. This may be because there are loads of 1s in dataframe... 
 #keep an eye and see how this progresses
 tryCatch(MyotisalcathoeALL <- MyotisalcathoeALL%>%
            mutate(
              activity_level = case_when(
                passes >= q0 & passes < q1 ~ "low",
                passes >= q1 & passes < q2 ~ "low/moderate",
                passes >= q2 & passes < q3 ~ "moderate",
                passes >= q3 & passes < q4 ~ "moderate/high",
                passes >= q4 & passes < q5 ~ "high",
                passes >= q5 ~ "exceptional",
                
              )
            ), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 #currently is the number of rows being compared to but really should be number of nights per location
 tryCatch(MyotisalcathoeALL$reference_range_size <- nrow(MyotisalcathoeALL), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

print("Myotis alcathoe percentiles done")

######################################################################################################################
######################################################################################################################
 ### MYOTIS BECHSTEINII ###
 
 #make percentiles and make sure everything is numeric

MyotisbechsteiniiALL$percentile <- tryCatch((ecdf(MyotisbechsteiniiALL$passes)(MyotisbechsteiniiALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
MyotisbechsteiniiALL$percentile <- tryCatch((lapply(MyotisbechsteiniiALL$percentile, as.integer)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
MyotisbechsteiniiALL$percentile <- tryCatch((as.numeric(MyotisbechsteiniiALL$percentile)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
MyotisbechsteiniiALL$passes <- tryCatch((as.numeric(MyotisbechsteiniiALL$passes)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})


# tryCatch(MyotisbechsteiniiALL$percentile <- ecdf((MyotisbechsteiniiALL$passes)(MyotisbechsteiniiALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# tryCatch(MyotisbechsteiniiALL$percentile <- lapply(MyotisbechsteiniiALL$percentile, as.integer), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# tryCatch(MyotisbechsteiniiALL$percentile <- as.numeric(MyotisbechsteiniiALL$percentile), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# tryCatch(MyotisbechsteiniiALL$passes <- as.numeric(MyotisbechsteiniiALL$passes), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 tryCatch(q0 <- quantile(MyotisbechsteiniiALL$passes, c(.0)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q1 <- quantile(MyotisbechsteiniiALL$passes, c(.20)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q2 <- quantile(MyotisbechsteiniiALL$passes, c(.40)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q3 <- quantile(MyotisbechsteiniiALL$passes, c(.60)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q4 <- quantile(MyotisbechsteiniiALL$passes, c(.80)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q5 <- quantile(MyotisbechsteiniiALL$passes, c(.95)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 #Create percentile activity_level column - KEEP AN EYE ON Q0 AND Q1 at the moment they are both 1 so dataframe is always low/mderate with no
 #low because all the low become low/moderate because is the same. This may be because there are loads of 1s in dataframe... 
 #keep an eye and see how this progresses
 tryCatch(MyotisbechsteiniiALL <- MyotisbechsteiniiALL%>%
            mutate(
              activity_level = case_when(
                passes >= q0 & passes < q1 ~ "low",
                passes >= q1 & passes < q2 ~ "low/moderate",
                passes >= q2 & passes < q3 ~ "moderate",
                passes >= q3 & passes < q4 ~ "moderate/high",
                passes >= q4 & passes < q5 ~ "high",
                passes >= q5 ~ "exceptional",
                
              )
            ), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 #currently is the number of rows being compared to but really should be number of nights per location
 tryCatch(MyotisbechsteiniiALL$reference_range_size <- nrow(MyotisbechsteiniiALL), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

print("Myotis bechsteinii percentiles done") 

 ######################################################################################################################
 ### MYOTIS BRANDTII ###
 
 #make percentiles and make sure everything is numeric

MyotisbrandtiiALL$percentile <- tryCatch((ecdf(MyotisbrandtiiALL$passes)(MyotisbrandtiiALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
MyotisbrandtiiALL$percentile <- tryCatch((lapply(MyotisbrandtiiALL$percentile, as.integer)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
MyotisbrandtiiALL$percentile <- tryCatch((as.numeric(MyotisbrandtiiALL$percentile)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
MyotisbrandtiiALL$passes <- tryCatch((as.numeric(MyotisbrandtiiALL$passes)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

# tryCatch(MyotisbrandtiiALL$percentile <- ecdf((MyotisbrandtiiALL$passes)(MyotisbrandtiiALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# tryCatch(MyotisbrandtiiALL$percentile <- lapply(MyotisbrandtiiALL$percentile, as.integer), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# tryCatch(MyotisbrandtiiALL$percentile <- as.numeric(MyotisbrandtiiALL$percentile), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# tryCatch(MyotisbrandtiiALL$passes <- as.numeric(MyotisbrandtiiALL$passes), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 tryCatch(q0 <- quantile(MyotisbrandtiiALL$passes, c(.0)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q1 <- quantile(MyotisbrandtiiALL$passes, c(.20)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q2 <- quantile(MyotisbrandtiiALL$passes, c(.40)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q3 <- quantile(MyotisbrandtiiALL$passes, c(.60)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q4 <- quantile(MyotisbrandtiiALL$passes, c(.80)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q5 <- quantile(MyotisbrandtiiALL$passes, c(.95)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 #Create percentile activity_level column - KEEP AN EYE ON Q0 AND Q1 at the moment they are both 1 so dataframe is always low/mderate with no
 #low because all the low become low/moderate because is the same. This may be because there are loads of 1s in dataframe... 
 #keep an eye and see how this progresses
 tryCatch(MyotisbrandtiiALL <- MyotisbrandtiiALL%>%
            mutate(
              activity_level = case_when(
                passes >= q0 & passes < q1 ~ "low",
                passes >= q1 & passes < q2 ~ "low/moderate",
                passes >= q2 & passes < q3 ~ "moderate",
                passes >= q3 & passes < q4 ~ "moderate/high",
                passes >= q4 & passes < q5 ~ "high",
                passes >= q5 ~ "exceptional",
                
              )
            ), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 #currently is the number of rows being compared to but really should be number of nights per location
 tryCatch(MyotisbrandtiiALL$reference_range_size <- nrow(MyotisbrandtiiALL), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

print("Myotis brandtii percentiles done") 

 ######################################################################################################################
  ### MYOTIS DAUBENTONII ###
 
 #make percentiles and make sure everything is numeric

MyotisdaubentoniiALL$percentile <- tryCatch((ecdf(MyotisdaubentoniiALL$passes)(MyotisdaubentoniiALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
MyotisdaubentoniiALL$percentile <- tryCatch((lapply(MyotisdaubentoniiALL$percentile, as.integer)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
MyotisdaubentoniiALL$percentile <- tryCatch((as.numeric(MyotisdaubentoniiALL$percentile)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
MyotisdaubentoniiALL$passes <- tryCatch((as.numeric(MyotisdaubentoniiALL$passes)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

# tryCatch(MyotisdaubentoniiALL$percentile <- ecdf((MyotisdaubentoniiALL$passes)(MyotisdaubentoniiALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# tryCatch(MyotisdaubentoniiALL$percentile <- lapply(MyotisdaubentoniiALL$percentile, as.integer), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# tryCatch(MyotisdaubentoniiALL$percentile <- as.numeric(MyotisdaubentoniiALL$percentile), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# tryCatch(MyotisdaubentoniiALL$passes <- as.numeric(MyotisdaubentoniiALL$passes), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 tryCatch(q0 <- quantile(MyotisdaubentoniiALL$passes, c(.0)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q1 <- quantile(MyotisdaubentoniiALL$passes, c(.20)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q2 <- quantile(MyotisdaubentoniiALL$passes, c(.40)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q3 <- quantile(MyotisdaubentoniiALL$passes, c(.60)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q4 <- quantile(MyotisdaubentoniiALL$passes, c(.80)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q5 <- quantile(MyotisdaubentoniiALL$passes, c(.95)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 #Create percentile activity_level column - KEEP AN EYE ON Q0 AND Q1 at the moment they are both 1 so dataframe is always low/mderate with no
 #low because all the low become low/moderate because is the same. This may be because there are loads of 1s in dataframe... 
 #keep an eye and see how this progresses
 tryCatch(MyotisdaubentoniiALL <- MyotisdaubentoniiALL%>%
            mutate(
              activity_level = case_when(
                passes >= q0 & passes < q1 ~ "low",
                passes >= q1 & passes < q2 ~ "low/moderate",
                passes >= q2 & passes < q3 ~ "moderate",
                passes >= q3 & passes < q4 ~ "moderate/high",
                passes >= q4 & passes < q5 ~ "high",
                passes >= q5 ~ "exceptional",
                
              )
            ), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 #currently is the number of rows being compared to but really should be number of nights per location
 tryCatch(MyotisdaubentoniiALL$reference_range_size <- nrow(MyotisdaubentoniiALL), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

print("Myotis daubentonii percentiles done") 

 ######################################################################################################################
 ### MYOTIS MYSTANICUS ###
 
 #make percentiles and make sure everything is numeric

MyotismystanicusALL$percentile <- tryCatch((ecdf(MyotismystanicusALL$passes)(MyotismystanicusALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
MyotismystanicusALL$percentile <- tryCatch((lapply(MyotismystanicusALL$percentile, as.integer)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
MyotismystanicusALL$percentile <- tryCatch((as.numeric(MyotismystanicusALL$percentile)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
MyotismystanicusALL$passes <- tryCatch((as.numeric(MyotismystanicusALL$passes)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

 #tryCatch(MyotismystanicusALL$percentile <- ecdf((MyotismystanicusALL$passes)(MyotismystanicusALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 #tryCatch(MyotismystanicusALL$percentile <- lapply(MyotismystanicusALL$percentile, as.integer), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 #tryCatch(MyotismystanicusALL$percentile <- as.numeric(MyotismystanicusALL$percentile), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 #tryCatch(MyotismystanicusALL$passes <- as.numeric(MyotismystanicusALL$passes), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 tryCatch(q0 <- quantile(MyotismystanicusALL$passes, c(.0)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q1 <- quantile(MyotismystanicusALL$passes, c(.20)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q2 <- quantile(MyotismystanicusALL$passes, c(.40)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q3 <- quantile(MyotismystanicusALL$passes, c(.60)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q4 <- quantile(MyotismystanicusALL$passes, c(.80)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q5 <- quantile(MyotismystanicusALL$passes, c(.95)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 #Create percentile activity_level column - KEEP AN EYE ON Q0 AND Q1 at the moment they are both 1 so dataframe is always low/mderate with no
 #low because all the low become low/moderate because is the same. This may be because there are loads of 1s in dataframe... 
 #keep an eye and see how this progresses
 tryCatch(MyotismystanicusALL <- MyotismystanicusALL%>%
            mutate(
              activity_level = case_when(
                passes >= q0 & passes < q1 ~ "low",
                passes >= q1 & passes < q2 ~ "low/moderate",
                passes >= q2 & passes < q3 ~ "moderate",
                passes >= q3 & passes < q4 ~ "moderate/high",
                passes >= q4 & passes < q5 ~ "high",
                passes >= q5 ~ "exceptional",
                
              )
            ), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 #currently is the number of rows being compared to but really should be number of nights per location
 tryCatch(MyotismystanicusALL$reference_range_size <- nrow(MyotismystanicusALL), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
print("Myotis mystanicus percentiles done")

 ######################################################################################################################
 ### MYOTIS NATTERERI ###

 #make percentiles and make sure everything is numeric

MyotisnattereriALL$percentile <- tryCatch((ecdf(MyotisnattereriALL$passes)(MyotisnattereriALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
MyotisnattereriALL$percentile <- tryCatch((lapply(MyotisnattereriALL$percentile, as.integer)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
MyotisnattereriALL$percentile <- tryCatch((as.numeric(MyotisnattereriALL$percentile)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
MyotisnattereriALL$passes <- tryCatch((as.numeric(MyotisnattereriALL$passes)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

 #tryCatch(MyotisnattereriALL$percentile <- ecdf((MyotisnattereriALL$passes)(MyotisnattereriALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 #tryCatch(MyotisnattereriALL$percentile <- lapply(MyotisnattereriALL$percentile, as.integer), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 #tryCatch(MyotisnattereriALL$percentile <- as.numeric(MyotisnattereriALL$percentile), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 #tryCatch(MyotisnattereriALL$passes <- as.numeric(MyotisnattereriALL$passes), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 tryCatch(q0 <- quantile(MyotisnattereriALL$passes, c(.0)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q1 <- quantile(MyotisnattereriALL$passes, c(.20)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q2 <- quantile(MyotisnattereriALL$passes, c(.40)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q3 <- quantile(MyotisnattereriALL$passes, c(.60)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q4 <- quantile(MyotisnattereriALL$passes, c(.80)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q5 <- quantile(MyotisnattereriALL$passes, c(.95)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 #Create percentile activity_level column - KEEP AN EYE ON Q0 AND Q1 at the moment they are both 1 so dataframe is always low/mderate with no
 #low because all the low become low/moderate because is the same. This may be because there are loads of 1s in dataframe... 
 #keep an eye and see how this progresses
 tryCatch(MyotisnattereriALL <- MyotisnattereriALL%>%
            mutate(
              activity_level = case_when(
                passes >= q0 & passes < q1 ~ "low",
                passes >= q1 & passes < q2 ~ "low/moderate",
                passes >= q2 & passes < q3 ~ "moderate",
                passes >= q3 & passes < q4 ~ "moderate/high",
                passes >= q4 & passes < q5 ~ "high",
                passes >= q5 ~ "exceptional",
                
              )
            ), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 #currently is the number of rows being compared to but really should be number of nights per location
 tryCatch(MyotisnattereriALL$reference_range_size <- nrow(MyotisnattereriALL), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

print("Myotis nattereri percentiles done")

 ######################################################################################################################
 ### MYOTIS ###
 
 #make percentiles and make sure everything is numeric

MyotisALL$percentile <- tryCatch((ecdf(MyotisALL$passes)(MyotisALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
MyotisALL$percentile <- tryCatch((lapply(MyotisALL$percentile, as.integer)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
MyotisALL$percentile <- tryCatch((as.numeric(MyotisALL$percentile)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
MyotisALL$passes <- tryCatch((as.numeric(MyotisALL$passes)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

 #tryCatch(MyotisALL$percentile <- ecdf((MyotisALL$passes)(MyotisALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 #tryCatch(MyotisALL$percentile <- lapply(MyotisALL$percentile, as.integer), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 #tryCatch(MyotisALL$percentile <- as.numeric(MyotisALL$percentile), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 #tryCatch(MyotisALL$passes <- as.numeric(MyotisALL$passes), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 tryCatch(q0 <- quantile(MyotisALL$passes, c(.0)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q1 <- quantile(MyotisALL$passes, c(.20)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q2 <- quantile(MyotisALL$passes, c(.40)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q3 <- quantile(MyotisALL$passes, c(.60)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q4 <- quantile(MyotisALL$passes, c(.80)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q5 <- quantile(MyotisALL$passes, c(.95)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 #Create percentile activity_level column - KEEP AN EYE ON Q0 AND Q1 at the moment they are both 1 so dataframe is always low/mderate with no
 #low because all the low become low/moderate because is the same. This may be because there are loads of 1s in dataframe... 
 #keep an eye and see how this progresses
 tryCatch(MyotisALL <- MyotisALL%>%
            mutate(
              activity_level = case_when(
                passes >= q0 & passes < q1 ~ "low",
                passes >= q1 & passes < q2 ~ "low/moderate",
                passes >= q2 & passes < q3 ~ "moderate",
                passes >= q3 & passes < q4 ~ "moderate/high",
                passes >= q4 & passes < q5 ~ "high",
                passes >= q5 ~ "exceptional",
                
              )
            ), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 #currently is the number of rows being compared to but really should be number of nights per location
 tryCatch(MyotisALL$reference_range_size <- nrow(MyotisALL), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

print("Myotis done") 

######################################################################################################################
 ### NYCTALUS LEISLERI ###
 
 #make percentiles and make sure everything is numeric

NyctalusleisleriALL$percentile <- tryCatch((ecdf(NyctalusleisleriALL$passes)(NyctalusleisleriALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
NyctalusleisleriALL$percentile <- tryCatch((lapply(NyctalusleisleriALL$percentile, as.integer)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
NyctalusleisleriALL$percentile <- tryCatch((as.numeric(NyctalusleisleriALL$percentile)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
NyctalusleisleriALL$passes <- tryCatch((as.numeric(NyctalusleisleriALL$passes)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

# tryCatch(NyctalusleisleriALL$percentile <- ecdf((NyctalusleisleriALL$passes)(NyctalusleisleriALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# tryCatch(NyctalusleisleriALL$percentile <- lapply(NyctalusleisleriALL$percentile, as.integer), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# tryCatch(NyctalusleisleriALL$percentile <- as.numeric(NyctalusleisleriALL$percentile), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# tryCatch(NyctalusleisleriALL$passes <- as.numeric(NyctalusleisleriALL$passes), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 tryCatch(q0 <- quantile(NyctalusleisleriALL$passes, c(.0)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q1 <- quantile(NyctalusleisleriALL$passes, c(.20)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q2 <- quantile(NyctalusleisleriALL$passes, c(.40)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q3 <- quantile(NyctalusleisleriALL$passes, c(.60)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q4 <- quantile(NyctalusleisleriALL$passes, c(.80)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q5 <- quantile(NyctalusleisleriALL$passes, c(.95)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 #Create percentile activity_level column - KEEP AN EYE ON Q0 AND Q1 at the moment they are both 1 so dataframe is always low/mderate with no
 #low because all the low become low/moderate because is the same. This may be because there are loads of 1s in dataframe... 
 #keep an eye and see how this progresses
 tryCatch(NyctalusleisleriALL <- NyctalusleisleriALL%>%
            mutate(
              activity_level = case_when(
                passes >= q0 & passes < q1 ~ "low",
                passes >= q1 & passes < q2 ~ "low/moderate",
                passes >= q2 & passes < q3 ~ "moderate",
                passes >= q3 & passes < q4 ~ "moderate/high",
                passes >= q4 & passes < q5 ~ "high",
                passes >= q5 ~ "exceptional",
                
              )
            ), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 #currently is the number of rows being compared to but really should be number of nights per location
 tryCatch(NyctalusleisleriALL$reference_range_size <- nrow(NyctalusleisleriALL), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

print("Nyctalus leisleri percentiles done") 

 ######################################################################################################################
 ### NYCTALUS NOCTULA ###
 
 #make percentiles and make sure everything is numeric

NyctalusnoctulaALL$percentile <- tryCatch((ecdf(NyctalusnoctulaALL$passes)(NyctalusnoctulaALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
NyctalusnoctulaALL$percentile <- tryCatch((lapply(NyctalusnoctulaALL$percentile, as.integer)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
NyctalusnoctulaALL$percentile <- tryCatch((as.numeric(NyctalusnoctulaALL$percentile)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
NyctalusnoctulaALL$passes <- tryCatch((as.numeric(NyctalusnoctulaALL$passes)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

# tryCatch(NyctalusnoctulaALL$percentile <- ecdf((NyctalusnoctulaALL$passes)(NyctalusnoctulaALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# tryCatch(NyctalusnoctulaALL$percentile <- lapply(NyctalusnoctulaALL$percentile, as.integer), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# tryCatch(NyctalusnoctulaALL$percentile <- as.numeric(NyctalusnoctulaALL$percentile), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# tryCatch(NyctalusnoctulaALL$passes <- as.numeric(NyctalusnoctulaALL$passes), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 tryCatch(q0 <- quantile(NyctalusnoctulaALL$passes, c(.0)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q1 <- quantile(NyctalusnoctulaALL$passes, c(.20)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q2 <- quantile(NyctalusnoctulaALL$passes, c(.40)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q3 <- quantile(NyctalusnoctulaALL$passes, c(.60)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q4 <- quantile(NyctalusnoctulaALL$passes, c(.80)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q5 <- quantile(NyctalusnoctulaALL$passes, c(.95)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 #Create percentile activity_level column - KEEP AN EYE ON Q0 AND Q1 at the moment they are both 1 so dataframe is always low/mderate with no
 #low because all the low become low/moderate because is the same. This may be because there are loads of 1s in dataframe... 
 #keep an eye and see how this progresses
 tryCatch(NyctalusnoctulaALL <- NyctalusnoctulaALL%>%
            mutate(
              activity_level = case_when(
                passes >= q0 & passes < q1 ~ "low",
                passes >= q1 & passes < q2 ~ "low/moderate",
                passes >= q2 & passes < q3 ~ "moderate",
                passes >= q3 & passes < q4 ~ "moderate/high",
                passes >= q4 & passes < q5 ~ "high",
                passes >= q5 ~ "exceptional",
                
              )
            ), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 #currently is the number of rows being compared to but really should be number of nights per location
 tryCatch(NyctalusnoctulaALL$reference_range_size <- nrow(NyctalusnoctulaALL), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

print("Nyctalus noctula done") 

 ######################################################################################################################
 ### NYCTALUS ###
 
 #make percentiles and make sure everything is numeric

NyctalusALL$percentile <- tryCatch((ecdf(NyctalusALL$passes)(NyctalusALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
NyctalusALL$percentile <- tryCatch((lapply(NyctalusALL$percentile, as.integer)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
NyctalusALL$percentile <- tryCatch((as.numeric(NyctalusALL$percentile)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
NyctalusALL$passes <- tryCatch((as.numeric(NyctalusALL$passes)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

# tryCatch(NyctalusALL$percentile <- ecdf((NyctalusALL$passes)(NyctalusALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# tryCatch(NyctalusALL$percentile <- lapply(NyctalusALL$percentile, as.integer), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# tryCatch(NyctalusALL$percentile <- as.numeric(NyctalusALL$percentile), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# tryCatch(NyctalusALL$passes <- as.numeric(NyctalusALL$passes), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 tryCatch(q0 <- quantile(NyctalusALL$passes, c(.0)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q1 <- quantile(NyctalusALL$passes, c(.20)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q2 <- quantile(NyctalusALL$passes, c(.40)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q3 <- quantile(NyctalusALL$passes, c(.60)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q4 <- quantile(NyctalusALL$passes, c(.80)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q5 <- quantile(NyctalusALL$passes, c(.95)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 #Create percentile activity_level column - KEEP AN EYE ON Q0 AND Q1 at the moment they are both 1 so dataframe is always low/mderate with no
 #low because all the low become low/moderate because is the same. This may be because there are loads of 1s in dataframe... 
 #keep an eye and see how this progresses
 tryCatch(NyctalusALL <- NyctalusALL%>%
            mutate(
              activity_level = case_when(
                passes >= q0 & passes < q1 ~ "low",
                passes >= q1 & passes < q2 ~ "low/moderate",
                passes >= q2 & passes < q3 ~ "moderate",
                passes >= q3 & passes < q4 ~ "moderate/high",
                passes >= q4 & passes < q5 ~ "high",
                passes >= q5 ~ "exceptional",
                
              )
            ), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 #currently is the number of rows being compared to but really should be number of nights per location
 tryCatch(NyctalusALL$reference_range_size <- nrow(NyctalusALL), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

print("Nyctalus percentiles done") 

 ######################################################################################################################
 ### NYCTALOID ###
 
 #make percentiles and make sure everything is numeric

NyctaloidALL$percentile <- tryCatch((ecdf(NyctaloidALL$passes)(NyctaloidALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
NyctaloidALL$percentile <- tryCatch((lapply(NyctaloidALL$percentile, as.integer)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
NyctaloidALL$percentile <- tryCatch((as.numeric(NyctaloidALL$percentile)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
NyctaloidALL$passes <- tryCatch((as.numeric(NyctaloidALL$passes)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

# tryCatch(NyctaloidALL$percentile <- ecdf((NyctaloidALL$passes)(NyctaloidALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# tryCatch(NyctaloidALL$percentile <- lapply(NyctaloidALL$percentile, as.integer), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# tryCatch(NyctaloidALL$percentile <- as.numeric(NyctaloidALL$percentile), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# tryCatch(NyctaloidALL$passes <- as.numeric(NyctaloidALL$passes), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 tryCatch(q0 <- quantile(NyctaloidALL$passes, c(.0)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q1 <- quantile(NyctaloidALL$passes, c(.20)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q2 <- quantile(NyctaloidALL$passes, c(.40)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q3 <- quantile(NyctaloidALL$passes, c(.60)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q4 <- quantile(NyctaloidALL$passes, c(.80)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q5 <- quantile(NyctaloidALL$passes, c(.95)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 #Create percentile activity_level column - KEEP AN EYE ON Q0 AND Q1 at the moment they are both 1 so dataframe is always low/mderate with no
 #low because all the low become low/moderate because is the same. This may be because there are loads of 1s in dataframe... 
 #keep an eye and see how this progresses
 tryCatch(NyctaloidALL <- NyctaloidALL%>%
            mutate(
              activity_level = case_when(
                passes >= q0 & passes < q1 ~ "low",
                passes >= q1 & passes < q2 ~ "low/moderate",
                passes >= q2 & passes < q3 ~ "moderate",
                passes >= q3 & passes < q4 ~ "moderate/high",
                passes >= q4 & passes < q5 ~ "high",
                passes >= q5 ~ "exceptional",
                
              )
            ), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 #currently is the number of rows being compared to but really should be number of nights per location
 tryCatch(NyctaloidALL$reference_range_size <- nrow(NyctaloidALL), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

print("Nyctaloid percentiles done") 

 ######################################################################################################################
 ### PIPITRELLUS NATHUSII ###
 
 #make percentiles and make sure everything is numeric

PipistrellusnathusiiALL$percentile <- tryCatch((ecdf(PipistrellusnathusiiALL$passes)(PipistrellusnathusiiALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
PipistrellusnathusiiALL$percentile <- tryCatch((lapply(PipistrellusnathusiiALL$percentile, as.integer)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
PipistrellusnathusiiALL$percentile <- tryCatch((as.numeric(PipistrellusnathusiiALL$percentile)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
PipistrellusnathusiiALL$passes <- tryCatch((as.numeric(PipistrellusnathusiiALL$passes)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

# tryCatch(PipistrellusnathusiiALL$percentile <- ecdf((PipistrellusnathusiiALL$passes)(PipistrellusnathusiiALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# tryCatch(PipistrellusnathusiiALL$percentile <- lapply(PipistrellusnathusiiALL$percentile, as.integer), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# tryCatch(PipistrellusnathusiiALL$percentile <- as.numeric(PipistrellusnathusiiALL$percentile), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# tryCatch(PipistrellusnathusiiALL$passes <- as.numeric(PipistrellusnathusiiALL$passes), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 tryCatch(q0 <- quantile(PipistrellusnathusiiALL$passes, c(.0)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q1 <- quantile(PipistrellusnathusiiALL$passes, c(.20)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q2 <- quantile(PipistrellusnathusiiALL$passes, c(.40)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q3 <- quantile(PipistrellusnathusiiALL$passes, c(.60)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q4 <- quantile(PipistrellusnathusiiALL$passes, c(.80)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q5 <- quantile(PipistrellusnathusiiALL$passes, c(.95)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 #Create percentile activity_level column - KEEP AN EYE ON Q0 AND Q1 at the moment they are both 1 so dataframe is always low/mderate with no
 #low because all the low become low/moderate because is the same. This may be because there are loads of 1s in dataframe... 
 #keep an eye and see how this progresses
 tryCatch(PipistrellusnathusiiALL <- PipistrellusnathusiiALL%>%
            mutate(
              activity_level = case_when(
                passes >= q0 & passes < q1 ~ "low",
                passes >= q1 & passes < q2 ~ "low/moderate",
                passes >= q2 & passes < q3 ~ "moderate",
                passes >= q3 & passes < q4 ~ "moderate/high",
                passes >= q4 & passes < q5 ~ "high",
                passes >= q5 ~ "exceptional",
                
              )
            ), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 #currently is the number of rows being compared to but really should be number of nights per location
 tryCatch(PipistrellusnathusiiALL$reference_range_size <- nrow(PipistrellusnathusiiALL), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

print("Pipistrellus nathusii percentiles done") 

 ######################################################################################################################
 ### PIPISTRELLUS PIPISTRELLUS ##
 
 #make percentiles and make sure everything is numeric

PipistrelluspipistrellusALL$percentile <- tryCatch((ecdf(PipistrelluspipistrellusALL$passes)(PipistrelluspipistrellusALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
PipistrelluspipistrellusALL$percentile <- tryCatch((lapply(PipistrelluspipistrellusALL$percentile, as.integer)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
PipistrelluspipistrellusALL$percentile <- tryCatch((as.numeric(PipistrelluspipistrellusALL$percentile)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
PipistrelluspipistrellusALL$passes <- tryCatch((as.numeric(PipistrelluspipistrellusALL$passes)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

 #tryCatch(PipistrelluspipistrellusALL$percentile <- ecdf((PipistrelluspipistrellusALL$passes)(PipistrelluspipistrellusALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 #tryCatch(PipistrelluspipistrellusALL$percentile <- lapply(PipistrelluspipistrellusALL$percentile, as.integer), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 #tryCatch(PipistrelluspipistrellusALL$percentile <- as.numeric(PipistrelluspipistrellusALL$percentile), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 #tryCatch(PipistrelluspipistrellusALL$passes <- as.numeric(PipistrelluspipistrellusALL$passes), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 tryCatch(q0 <- quantile(PipistrelluspipistrellusALL$passes, c(.0)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q1 <- quantile(PipistrelluspipistrellusALL$passes, c(.20)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q2 <- quantile(PipistrelluspipistrellusALL$passes, c(.40)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q3 <- quantile(PipistrelluspipistrellusALL$passes, c(.60)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q4 <- quantile(PipistrelluspipistrellusALL$passes, c(.80)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q5 <- quantile(PipistrelluspipistrellusALL$passes, c(.95)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 #Create percentile activity_level column - KEEP AN EYE ON Q0 AND Q1 at the moment they are both 1 so dataframe is always low/mderate with no
 #low because all the low become low/moderate because is the same. This may be because there are loads of 1s in dataframe... 
 #keep an eye and see how this progresses
 tryCatch(PipistrelluspipistrellusALL <- PipistrelluspipistrellusALL%>%
            mutate(
              activity_level = case_when(
                passes >= q0 & passes < q1 ~ "low",
                passes >= q1 & passes < q2 ~ "low/moderate",
                passes >= q2 & passes < q3 ~ "moderate",
                passes >= q3 & passes < q4 ~ "moderate/high",
                passes >= q4 & passes < q5 ~ "high",
                passes >= q5 ~ "exceptional",
                
              )
            ), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 #currently is the number of rows being compared to but really should be number of nights per location
 tryCatch(PipistrelluspipistrellusALL$reference_range_size <- nrow(PipistrelluspipistrellusALL), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

print("Pipistrellus pipistrellus percentiles done") 

 #######################################################################################################
 
### PIPISTRELLUS PYGMAEUS ###
 
 #make percentiles and make sure everything is numeric

PipistrelluspygmaeusALL$percentile <- tryCatch((ecdf(PipistrelluspygmaeusALL$passes)(PipistrelluspygmaeusALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
PipistrelluspygmaeusALL$percentile <- tryCatch((lapply(PipistrelluspygmaeusALL$percentile, as.integer)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
PipistrelluspygmaeusALL$percentile <- tryCatch((as.numeric(PipistrelluspygmaeusALL$percentile)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
PipistrelluspygmaeusALL$passes <- tryCatch((as.numeric(PipistrelluspygmaeusALL$passes)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# tryCatch(PipistrelluspygmaeusALL$percentile <- ecdf((PipistrelluspygmaeusALL$passes)(PipistrelluspygmaeusALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# tryCatch(PipistrelluspygmaeusALL$percentile <- lapply(PipistrelluspygmaeusALL$percentile, as.integer), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# tryCatch(PipistrelluspygmaeusALL$percentile <- as.numeric(PipistrelluspygmaeusALL$percentile), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# tryCatch(PipistrelluspygmaeusALL$passes <- as.numeric(PipistrelluspygmaeusALL$passes), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
 tryCatch(q0 <- quantile(PipistrelluspygmaeusALL$passes, c(.0)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q1 <- quantile(PipistrelluspygmaeusALL$passes, c(.20)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q2 <- quantile(PipistrelluspygmaeusALL$passes, c(.40)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q3 <- quantile(PipistrelluspygmaeusALL$passes, c(.60)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q4 <- quantile(PipistrelluspygmaeusALL$passes, c(.80)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 tryCatch(q5 <- quantile(PipistrelluspygmaeusALL$passes, c(.95)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 #Create percentile activity_level column - KEEP AN EYE ON Q0 AND Q1 at the moment they are both 1 so dataframe is always low/mderate with no
 #low because all the low become low/moderate because is the same. This may be because there are loads of 1s in dataframe... 
 #keep an eye and see how this progresses
 tryCatch(PipistrelluspygmaeusALL <- PipistrelluspygmaeusALL%>%
            mutate(
              activity_level = case_when(
                passes >= q0 & passes < q1 ~ "low",
                passes >= q1 & passes < q2 ~ "low/moderate",
                passes >= q2 & passes < q3 ~ "moderate",
                passes >= q3 & passes < q4 ~ "moderate/high",
                passes >= q4 & passes < q5 ~ "high",
                passes >= q5 ~ "exceptional",
                
              )
            ), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 #currently is the number of rows being compared to but really should be number of nights per location
 tryCatch(PipistrelluspygmaeusALL$reference_range_size <- nrow(PipistrelluspygmaeusALL), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

 
 ######################################################################################################################
### PIPISTRELLUS ###

#make percentiles and make sure everything is numeric

PipistrellusALL$percentile <- tryCatch((ecdf(PipistrellusALL$passes)(PipistrellusALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
PipistrellusALL$percentile <- tryCatch((lapply(PipistrellusALL$percentile, as.integer)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
PipistrellusALL$percentile <- tryCatch((as.numeric(PipistrellusALL$percentile)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
PipistrellusALL$passes <- tryCatch((as.numeric(PipistrellusALL$passes)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#tryCatch(PipistrellusALL$percentile <- ecdf((PipistrellusALL$passes)(PipistrellusALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch(PipistrellusALL$percentile <- lapply(PipistrellusALL$percentile, as.integer), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch(PipistrellusALL$percentile <- as.numeric(PipistrellusALL$percentile), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch(PipistrellusALL$passes <- as.numeric(PipistrellusALL$passes), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

tryCatch(q0 <- quantile(PipistrellusALL$passes, c(.0)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q1 <- quantile(PipistrellusALL$passes, c(.20)))
tryCatch(q2 <- quantile(PipistrellusALL$passes, c(.40)))
tryCatch(q3 <- quantile(PipistrellusALL$passes, c(.60)))
tryCatch(q4 <- quantile(PipistrellusALL$passes, c(.80)))
tryCatch(q5 <- quantile(PipistrellusALL$passes, c(.95)))

#Create percentile activity_level column - KEEP AN EYE ON Q0 AND Q1 at the moment they are both 1 so dataframe is always low/mderate with no
#low because all the low become low/moderate because is the same. This may be because there are loads of 1s in dataframe... 
#keep an eye and see how this progresses
tryCatch(PipistrellusALL <- PipistrellusALL%>%
  mutate(
    activity_level = case_when(
      passes >= q0 & passes < q1 ~ "low",
      passes >= q1 & passes < q2 ~ "low/moderate",
      passes >= q2 & passes < q3 ~ "moderate",
      passes >= q3 & passes < q4 ~ "moderate/high",
      passes >= q4 & passes < q5 ~ "high",
      passes >= q5 ~ "exceptional",
      
    )
  ), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#currently is the number of rows being compared to but really should be number of nights per location
tryCatch(PipistrellusALL$reference_range_size <- nrow(PipistrellusALL), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

print("Pipistrellus percentiles done")

######################################################################################################################
### PLECOTUS AURITUS ###

#make percentiles and make sure everything is numeric

PlecotusauritusALL$percentile <- tryCatch((ecdf(PlecotusauritusALL$passes)(PlecotusauritusALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
PlecotusauritusALL$percentile <- tryCatch((lapply(PlecotusauritusALL$percentile, as.integer)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
PlecotusauritusALL$percentile <- tryCatch((as.numeric(PlecotusauritusALL$percentile)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
PlecotusauritusALL$passes <- tryCatch((as.numeric(PlecotusauritusALL$passes)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#note auritus is mispelled in here and will need to corrcet if ever unhash
#tryCatch(PlecotusauritisALL$percentile <- ecdf((PlecotusauritisALL$passes)(PlecotusauritisALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch(PlecotusauritisALL$percentile <- lapply(PlecotusauritisALL$percentile, as.integer), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch(PlecotusauritisALL$percentile <- as.numeric(PlecotusauritisALL$percentile), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch(PlecotusauritisALL$passes <- as.numeric(PlecotusauritisALL$passes), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

tryCatch(q0 <- quantile(PlecotusauritusALL$passes, c(.0)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q1 <- quantile(PlecotusauritusALL$passes, c(.20)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q2 <- quantile(PlecotusauritusALL$passes, c(.40)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q3 <- quantile(PlecotusauritusALL$passes, c(.60)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q4 <- quantile(PlecotusauritusALL$passes, c(.80)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q5 <- quantile(PlecotusauritusALL$passes, c(.95)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#Create percentile activity_level column - KEEP AN EYE ON Q0 AND Q1 at the moment they are both 1 so dataframe is always low/mderate with no
#low because all the low become low/moderate because is the same. This may be because there are loads of 1s in dataframe... 
#keep an eye and see how this progresses
tryCatch(PlecotusauritusALL <- PlecotusauritusALL%>%
           mutate(
             activity_level = case_when(
               passes >= q0 & passes < q1 ~ "low",
               passes >= q1 & passes < q2 ~ "low/moderate",
               passes >= q2 & passes < q3 ~ "moderate",
               passes >= q3 & passes < q4 ~ "moderate/high",
               passes >= q4 & passes < q5 ~ "high",
               passes >= q5 ~ "exceptional",
               
             )
           ), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#currently is the number of rows being compared to but really should be number of nights per location
tryCatch(PlecotusauritusALL$reference_range_size <- nrow(PlecotusauritusALL), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

print("Plecotus auritus percentiles done")

######################################################################################################################
### PLECOTUS AUSTRIACUS ###

#make percentiles and make sure everything is numeric

PlecotusaustriacusALL$percentile <- tryCatch((ecdf(PlecotusaustriacusALL$passes)(PlecotusaustriacusALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
PlecotusaustriacusALL$percentile <- tryCatch((lapply(PlecotusaustriacusALL$percentile, as.integer)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
PlecotusaustriacusALL$percentile <- tryCatch((as.numeric(PlecotusaustriacusALL$percentile)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
PlecotusaustriacusALL$passes <- tryCatch((as.numeric(PlecotusaustriacusALL$passes)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#tryCatch(PlecotusaustriacusALL$percentile <- ecdf((PlecotusaustriacusALL$passes)(PlecotusaustriacusALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch(PlecotusaustriacusALL$percentile <- lapply(PlecotusaustriacusALL$percentile, as.integer), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch(PlecotusaustriacusALL$percentile <- as.numeric(PlecotusaustriacusALL$percentile), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch(PlecotusaustriacusALL$passes <- as.numeric(PlecotusaustriacusALL$passes), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

tryCatch(q0 <- quantile(PlecotusaustriacusALL$passes, c(.0)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q1 <- quantile(PlecotusaustriacusALL$passes, c(.20)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q2 <- quantile(PlecotusaustriacusALL$passes, c(.40)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q3 <- quantile(PlecotusaustriacusALL$passes, c(.60)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q4 <- quantile(PlecotusaustriacusALL$passes, c(.80)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q5 <- quantile(PlecotusaustriacusALL$passes, c(.95)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#Create percentile activity_level column - KEEP AN EYE ON Q0 AND Q1 at the moment they are both 1 so dataframe is always low/mderate with no
#low because all the low become low/moderate because is the same. This may be because there are loads of 1s in dataframe... 
#keep an eye and see how this progresses
tryCatch(PlecotusaustriacusALL <- PlecotusaustriacusALL%>%
           mutate(
             activity_level = case_when(
               passes >= q0 & passes < q1 ~ "low",
               passes >= q1 & passes < q2 ~ "low/moderate",
               passes >= q2 & passes < q3 ~ "moderate",
               passes >= q3 & passes < q4 ~ "moderate/high",
               passes >= q4 & passes < q5 ~ "high",
               passes >= q5 ~ "exceptional",
               
             )
           ), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#currently is the number of rows being compared to but really should be number of nights per location
tryCatch(PlecotusaustriacusALL$reference_range_size <- nrow(PlecotusaustriacusALL), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

print("Plecotus austriacus percentiles done")

######################################################################################################################
### PLECOTUS ###

#make percentiles and make sure everything is numeric

PlecotusALL$percentile <- tryCatch((ecdf(PlecotusALL$passes)(PlecotusALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
PlecotusALL$percentile <- tryCatch((lapply(PlecotusALL$percentile, as.integer)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
PlecotusALL$percentile <- tryCatch((as.numeric(PlecotusALL$percentile)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
PlecotusALL$passes <- tryCatch((as.numeric(PlecotusALL$passes)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#tryCatch(PlecotusALL$percentile <- ecdf((PlecotusALL$passes)(PlecotusALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch(PlecotusALL$percentile <- lapply(PlecotusALL$percentile, as.integer), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch(PlecotusALL$percentile <- as.numeric(PlecotusALL$percentile), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch(PlecotusALL$passes <- as.numeric(PlecotusALL$passes), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

tryCatch(q0 <- quantile(PlecotusALL$passes, c(.0)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q1 <- quantile(PlecotusALL$passes, c(.20)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q2 <- quantile(PlecotusALL$passes, c(.40)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q3 <- quantile(PlecotusALL$passes, c(.60)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q4 <- quantile(PlecotusALL$passes, c(.80)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q5 <- quantile(PlecotusALL$passes, c(.95)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#Create percentile activity_level column - KEEP AN EYE ON Q0 AND Q1 at the moment they are both 1 so dataframe is always low/mderate with no
#low because all the low become low/moderate because is the same. This may be because there are loads of 1s in dataframe... 
#keep an eye and see how this progresses
tryCatch(PlecotusALL <- PlecotusALL%>%
           mutate(
             activity_level = case_when(
               passes >= q0 & passes < q1 ~ "low",
               passes >= q1 & passes < q2 ~ "low/moderate",
               passes >= q2 & passes < q3 ~ "moderate",
               passes >= q3 & passes < q4 ~ "moderate/high",
               passes >= q4 & passes < q5 ~ "high",
               passes >= q5 ~ "exceptional",
               
             )
           ), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#currently is the number of rows being compared to but really should be number of nights per location
tryCatch(PlecotusALL$reference_range_size <- nrow(PlecotusALL), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

print("Plecotus percentiles done")

######################################################################################################################
### RHINOLOPHUS FERRUMEQUINUM ###

#make percentiles and make sure everything is numeric

RhinolophusferrumequinumALL$percentile <- tryCatch((ecdf(RhinolophusferrumequinumALL$passes)(RhinolophusferrumequinumALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
RhinolophusferrumequinumALL$percentile <- tryCatch((lapply(RhinolophusferrumequinumALL$percentile, as.integer)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
RhinolophusferrumequinumALL$percentile <- tryCatch((as.numeric(RhinolophusferrumequinumALL$percentile)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
RhinolophusferrumequinumALL$passes <- tryCatch((as.numeric(RhinolophusferrumequinumALL$passes)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#tryCatch(RhinolophusferrumequinumALL$percentile <- ecdf((RhinolophusferrumequinumALL$passes)(RhinolophusferrumequinumALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch(RhinolophusferrumequinumALL$percentile <- lapply(RhinolophusferrumequinumALL$percentile, as.integer), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch(RhinolophusferrumequinumALL$percentile <- as.numeric(RhinolophusferrumequinumALL$percentile), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch(RhinolophusferrumequinumALL$passes <- as.numeric(RhinolophusferrumequinumALL$passes), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

tryCatch(q0 <- quantile(RhinolophusferrumequinumALL$passes, c(.0)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q1 <- quantile(RhinolophusferrumequinumALL$passes, c(.20)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q2 <- quantile(RhinolophusferrumequinumALL$passes, c(.40)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q3 <- quantile(RhinolophusferrumequinumALL$passes, c(.60)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q4 <- quantile(RhinolophusferrumequinumALL$passes, c(.80)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q5 <- quantile(RhinolophusferrumequinumALL$passes, c(.95)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#Create percentile activity_level column - KEEP AN EYE ON Q0 AND Q1 at the moment they are both 1 so dataframe is always low/mderate with no
#low because all the low become low/moderate because is the same. This may be because there are loads of 1s in dataframe... 
#keep an eye and see how this progresses
tryCatch(RhinolophusferrumequinumALL <- RhinolophusferrumequinumALL%>%
           mutate(
             activity_level = case_when(
               passes >= q0 & passes < q1 ~ "low",
               passes >= q1 & passes < q2 ~ "low/moderate",
               passes >= q2 & passes < q3 ~ "moderate",
               passes >= q3 & passes < q4 ~ "moderate/high",
               passes >= q4 & passes < q5 ~ "high",
               passes >= q5 ~ "exceptional",
               
             )
           ), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#currently is the number of rows being compared to but really should be number of nights per location
tryCatch(RhinolophusferrumequinumALL$reference_range_size <- nrow(RhinolophusferrumequinumALL), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

print("Rhinolophus ferrumequinum percentiles done")

######################################################################################################################
### RHINOLOPHUS HIPPOSIDEROS ###

#make percentiles and make sure everything is numeric

RhinolophushipposiderosALL$percentile <- tryCatch((ecdf(RhinolophushipposiderosALL$passes)(RhinolophushipposiderosALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
RhinolophushipposiderosALL$percentile <- tryCatch((lapply(RhinolophushipposiderosALL$percentile, as.integer)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
RhinolophushipposiderosALL$percentile <- tryCatch((as.numeric(RhinolophushipposiderosALL$percentile)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
RhinolophushipposiderosALL$passes <- tryCatch((as.numeric(RhinolophushipposiderosALL$passes)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#tryCatch(RhinolophushipposiderosALL$percentile <- ecdf((RhinolophushipposiderosALL$passes)(RhinolophushipposiderosALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch(RhinolophushipposiderosALL$percentile <- lapply(RhinolophushipposiderosALL$percentile, as.integer), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch(RhinolophushipposiderosALL$percentile <- as.numeric(RhinolophushipposiderosALL$percentile), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch(RhinolophushipposiderosALL$passes <- as.numeric(RhinolophushipposiderosALL$passes), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

tryCatch(q0 <- quantile(RhinolophushipposiderosALL$passes, c(.0)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q1 <- quantile(RhinolophushipposiderosALL$passes, c(.20)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q2 <- quantile(RhinolophushipposiderosALL$passes, c(.40)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q3 <- quantile(RhinolophushipposiderosALL$passes, c(.60)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q4 <- quantile(RhinolophushipposiderosALL$passes, c(.80)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q5 <- quantile(RhinolophushipposiderosALL$passes, c(.95)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#Create percentile activity_level column - KEEP AN EYE ON Q0 AND Q1 at the moment they are both 1 so dataframe is always low/mderate with no
#low because all the low become low/moderate because is the same. This may be because there are loads of 1s in dataframe... 
#keep an eye and see how this progresses
tryCatch(RhinolophushipposiderosALL <- RhinolophushipposiderosALL%>%
           mutate(
             activity_level = case_when(
               passes >= q0 & passes < q1 ~ "low",
               passes >= q1 & passes < q2 ~ "low/moderate",
               passes >= q2 & passes < q3 ~ "moderate",
               passes >= q3 & passes < q4 ~ "moderate/high",
               passes >= q4 & passes < q5 ~ "high",
               passes >= q5 ~ "exceptional",
               
             )
           ), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#currently is the number of rows being compared to but really should be number of nights per location
tryCatch(RhinolophushipposiderosALL$reference_range_size <- nrow(RhinolophushipposiderosALL), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

print("Rhinolophus hipposideros percentiles done")

######################################################################################################################
### RHINOLOPHUS ###

#make percentiles and make sure everything is numeric

RhinolophusALL$percentile <- tryCatch((ecdf(RhinolophusALL$passes)(RhinolophusALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
RhinolophusALL$percentile <- tryCatch((lapply(RhinolophusALL$percentile, as.integer)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
RhinolophusALL$percentile <- tryCatch((as.numeric(RhinolophusALL$percentile)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
RhinolophusALL$passes <- tryCatch((as.numeric(RhinolophusALL$passes)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#tryCatch(RhinolophusALL$percentile <- ecdf((RhinolophusALL$passes)(RhinolophusALL$passes)*100), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch(RhinolophusALL$percentile <- lapply(RhinolophusALL$percentile, as.integer), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch(RhinolophusALL$percentile <- as.numeric(RhinolophusALL$percentile), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#tryCatch(RhinolophusALL$passes <- as.numeric(RhinolophusALL$passes), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

tryCatch(q0 <- quantile(RhinolophusALL$passes, c(.0)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q1 <- quantile(RhinolophusALL$passes, c(.20)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q2 <- quantile(RhinolophusALL$passes, c(.40)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q3 <- quantile(RhinolophusALL$passes, c(.60)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q4 <- quantile(RhinolophusALL$passes, c(.80)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(q5 <- quantile(RhinolophusALL$passes, c(.95)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#Create percentile activity_level column - KEEP AN EYE ON Q0 AND Q1 at the moment they are both 1 so dataframe is always low/mderate with no
#low because all the low become low/moderate because is the same. This may be because there are loads of 1s in dataframe... 
#keep an eye and see how this progresses
tryCatch(RhinolophusALL <- RhinolophusALL%>%
           mutate(
             activity_level = case_when(
               passes >= q0 & passes < q1 ~ "low",
               passes >= q1 & passes < q2 ~ "low/moderate",
               passes >= q2 & passes < q3 ~ "moderate",
               passes >= q3 & passes < q4 ~ "moderate/high",
               passes >= q4 & passes < q5 ~ "high",
               passes >= q5 ~ "exceptional",
               
             )
           ), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#currently is the number of rows being compared to but really should be number of nights per location
tryCatch(RhinolophusALL$reference_range_size <- nrow(RhinolophusALL), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

print("Rhinolophus percentiles done")

#######################################################################################################

#remove anything don't need to save space
#rm(q0, q1, q2, q3, q4, q5)

#print("quartiles removed")

#######################################################################################################

#creating a blank dataframe, then will use tryCatch to try and rbind each piece of the proforma back in
Bprep <- data.frame(name=character(),
                    email=character(),
                    site_name=character(),
                    location_name=character(),
                    county=character(),
                    region=character(),
                    country=character(),
                    latlon=character(),
                    lat=numeric(),
                    lon=numeric(),
                    date=character(),
                    day=integer(),
                    month=integer(),
                    year=integer(),
                    time=character(),
                    species=character(),
                    number_of_bats=integer(),
                    sensitivity=character(),
                    public_date=character(),
                    pass_definition=character(),
                    detector_make=character(),
                    detector_model=character(),
                    detector_height_m=numeric(),
                    roost_within_25m=logical(),
                    activity_elevated_by_roost=logical(),
                    linear_feature_adjacent=character(),
                    linear_feature_25m=character(),
                    anthropogenic_feature_adjacent=character(),
                    anthropogenic_feature_25m=character(),
                    temperature_c=numeric(),
                    rainfall=character(),
                    wind_speed_mph=integer(),
                    method_of_classification=character(),
                    analysis_software_used=character(),
                    notes=character(),
                    proformaORdf = character(),
                    timestamp = character(),
                    passes = numeric(),
                    #max=numeric(),
                    #median=numeric(),
                    percentile=numeric(),
                    activity_level=character(),
                    reference_range_size = integer(),
                    stringsAsFactors=FALSE)

print("blank data frame made")

#need to rbind each back one at a time, alltogether accumulates all the data step by step 
#BarbasteltryCatch(rbind(`Barbastellus`, `Barbastellusdf`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
Bprep <- tryCatch(rbind(`Bprep`, `BarbastellusALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) 
tryCatch(rm(`Barbastellusdf`, `BarbastellusALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) 
Bprep <- Bprep %>%
  filter(proformaORdf == "proforma")
print("Barbastellus added, df and ALL removed")

Bprep <- tryCatch(rbind(`Bprep`, `BarbastellabarbastellusALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(rm(`Barbastella barbastellusdf`, `BarbastellabarbastellusALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
Bprep <- Bprep %>%
  filter(proformaORdf == "proforma")
print("Barbastellus basbastellus added, df and ALL removed")

Bprep <- tryCatch(rbind(`Bprep`, `EptesicusserotinusALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(rm(`Eptesicus serotinusdf`, `EptesicusserotinusALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
Bprep <- Bprep %>%
  filter(proformaORdf == "proforma")
print("Eptesicus serotinus added, df and ALL  removed")

Bprep <- tryCatch(rbind(`Bprep`, `EptesicusALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(rm(`Eptesicus df`, `EptesicusALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
Bprep <- Bprep %>%
  filter(proformaORdf == "proforma")
print("Eptesicus added, df and ALL removed")

Bprep <- tryCatch(rbind(`Bprep`, `MyotisalcathoeALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(rm(`Myotis alcathoedf`, `MyotisalcathoeALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
Bprep <- Bprep %>%
  filter(proformaORdf == "proforma")
print("Myotis alcathoe added, df and ALL removed")

Bprep <- tryCatch(rbind(`Bprep`, `MyotisbechsteiniiALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(rm(`Myotis bechsteiniidf`, `MyotisbechsteiniiALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
Bprep <- Bprep %>%
  filter(proformaORdf == "proforma")
print("Myotis bechsteinii added, df and ALL removed")

Bprep <- tryCatch(rbind(`Bprep`, `MyotisbrandtiiALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(rm(`Myotis brandtiidf`, `MyotisbrandtiiALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
Bprep <- Bprep %>%
  filter(proformaORdf == "proforma")
print("Myotis brandtii added, df and ALL removed")

Bprep <- tryCatch(rbind(`Bprep`, `MyotisdaubentoniiALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(rm(`Myotis daubentoniidf`, `MyotisdaubentoniiALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
Bprep <- Bprep %>%
  filter(proformaORdf == "proforma")
print("Myotis daubentonii added, df and ALL removed")

Bprep <- tryCatch(rbind(`Bprep`, `MyotismystanicusALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(rm(`Myotis mystanicusdf`, `MyotismystanicusALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
Bprep <- Bprep %>%
  filter(proformaORdf == "proforma")
print("Myotis mystanicus added, df and ALL removed")

Bprep <- tryCatch(rbind(`Bprep`, `MyotisnattereriALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(rm(`Myotis nattereridf`, `MyotisnattereriALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
Bprep <- Bprep %>%
  filter(proformaORdf == "proforma")
print("Myotis nattereri added, df and ALL removed")

Bprep <- tryCatch(rbind(`Bprep`, `MyotisALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(rm(`Myotisdf`, `MyotisALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
Bprep <- Bprep %>%
  filter(proformaORdf == "proforma")
print("Myotis added, df and ALL removed")

Bprep <- tryCatch(rbind(`Bprep`, `NyctalusleisleriALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(rm(`Nyctalus leisleridf`, `NyctalusleisleriALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
Bprep <- Bprep %>%
  filter(proformaORdf == "proforma")
print("Nyctalus leisleri added, df and ALL removed")

Bprep <- tryCatch(rbind(`Bprep`, `NyctalusnoctulaALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(rm(`Nyctalus noctuladf`, `NyctalusnoctulaALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
Bprep <- Bprep %>%
  filter(proformaORdf == "proforma")
print("Nyctalus noctula added, df and ALL removed")

Bprep <- tryCatch(rbind(`Bprep`, `NyctalusALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(rm(`Nyctalusdf`, `NyctalusALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
Bprep <- Bprep %>%
  filter(proformaORdf == "proforma")
print("Nyctalus added")

Bprep <- tryCatch(rbind(`Bprep`, `NyctaloidALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(rm(`Nyctaloiddf`, `NyctaloidALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
Bprep <- Bprep %>%
  filter(proformaORdf == "proforma")
print("Nyctaloid added, df and ALL removed")

Bprep <- tryCatch(rbind(`Bprep`, `PipistrellusnathusiiALL`), error=function(e) {cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(rm(`Pipistrellus nathusiidf`, `PipistrellusnathusiiALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
Bprep <- Bprep %>%
  filter(proformaORdf == "proforma")
print("Pipistrellus nathusii added, df and ALL removed")

Bprep <- tryCatch(rbind(`Bprep`, `PipistrelluspipistrellusALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(rm(`Pipistrellus pipistrellusdf`, `PipistrelluspipistrellusALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
Bprep <- Bprep %>%
  filter(proformaORdf == "proforma")
print("Pipistrellus pipistrellus added, df and ALL removed")

Bprep <- tryCatch(rbind(`Bprep`, `PipistrelluspygmaeusALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(rm(`Pipistrellus pygmaeusdf`, `PipistrelluspygmaeusALL`),  error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
Bprep <- Bprep %>%
  filter(proformaORdf == "proforma")
print("Pipistrellus pygmaeus added, df and ALL removed")

Bprep <- tryCatch(rbind(`Bprep`, `PipistrellusALL`),  error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(rm(`Pipistrellusdf`, `PipistrellusALL`),  error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
Bprep <- Bprep %>%
  filter(proformaORdf == "proforma")
print("Pipistrellus added, df and ALL removed")

Bprep <- tryCatch(rbind(`Bprep`, `PlecotusauritusALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(rm(`Plecotus auritusdf`, `PlecotusauritusALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
Bprep <- Bprep %>%
  filter(proformaORdf == "proforma")
print("Plecotus auritus added, df and ALL removed")

Bprep <- tryCatch(rbind(`Bprep`, `PlecotusaustriacusALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(rm(`Plecotus austriacusdf`, `PlecotusaustriacusALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
Bprep <- Bprep %>%
  filter(proformaORdf == "proforma")
print("Plecotus austriacus added, df and ALL removed")

Bprep <- tryCatch(rbind(`Bprep`, `PlecotusALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(rm(`Plecotusdf`, `PlecotusALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
Bprep <- Bprep %>%
  filter(proformaORdf == "proforma")
print("Plecotus added")

Bprep <- tryCatch(rbind(`Bprep`, `RhinolophusferrumequinumALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(rm(`Rhinolophus ferrumequinumdf`, `RhinolophusferrumequinumALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
Bprep <- Bprep %>%
  filter(proformaORdf == "proforma")
print("Rhinolophus ferrumequinum added, df and ALL removed")

Bprep <- tryCatch(rbind(`Bprep`, `RhinolophushipposiderosALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(rm(`Rhinolophus hipposiderosdf`, `RhinolophushipposiderosALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
Bprep <- Bprep %>%
  filter(proformaORdf == "proforma")
print("Rhinolophus hipposideros added, df and ALL removed")

Bprep <- tryCatch(rbind(`Bprep`, `RhinolophusALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch(rm(`Rhinolophusdf`, `RhinolophusALL`), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
print("Rhinolophus added, df and ALL removed")

print("added all species into bprep")

#make a proforma and df by filtering alltogether by whether has come from the proforma or df originally. Names need to match what
#normal ecobat markdown uses because all this will be going at the top of that file.

Bprep <- Bprep %>%
  filter(proformaORdf == "proforma") %>%
  ungroup()

Bprep <- Bprep %>%
  select(location_name, lat, lon, date, time, species, passes, percentile, activity_level, pass_definition,
         detector_make, detector_model, detector_height_m, roost_within_25m, activity_elevated_by_roost,
         linear_feature_adjacent, linear_feature_25m, anthropogenic_feature_adjacent, anthropogenic_feature_25m, temperature_c,
         wind_speed_mph, method_of_classification, analysis_software_used, site_name, reference_range_size)

Bprep$activity_level <- as.factor(Bprep$activity_level)

#for when testing locally and want to see how it reads into Nightly.rmd
#write.csv(Bprep, "Bprep.csv" )

#run the rmarkdown nightly file
#rmarkdown::render("Nightly.Rmd")
