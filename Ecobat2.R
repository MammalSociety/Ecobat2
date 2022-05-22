#testing
# 
# dataa <- "../ecobat_files/proforma1.csv" #tells R Markdown where it can find data
# datab <- read.csv("../ecobat_files/proforma1.csv", header=TRUE) #tells Rmd where to read the data from
# author <- "simon" #tells Rmd what to use as Author
# sitename <- "blahblah" #tells Rmd what to use as Site Name
# geofilter <- "All data"
# timefilter <- "All Data"
# save <- "Already uploaded records, do NOT save to database"
# token <- readRDS("droptoken.rds")
# # Set up parameters to pass to Rmd document
# params <- list(n = datab, 
#                 Author = author, 
#                 SiteName = sitename,
#                 GeoFilter = geofilter,
#                 TimeFilter = timefilter,
#                 Save = save,
#                 Token = token)



######## PRE MARKDOWN SCRIPT ###########
## AUTHOR: Charlie Le Marquand
## DATE LAST EDITED: 15/02/2022
#######################################################################################################################

#NEXT STEPS:
#When all done, everything working and it's online, go through and delete all unnecessary hashed out code
#Make sure all the print line numbers refer to the actual correct lines - use these for tracking errors when line # is not marked

#######################################################################################################################

#library(tidyr)
#library(dplyr)
#library(tidyverse)
#library(rmarkdown)
#library(rsconnect) #for connecting to shiny.io
#library(tinytex) #for ccreating the pdf
#library(rdrop2) #for connecting to dropbox
#library(lubridate)

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

lobstr::obj_size(dataframe1)


#dataframe1 <- sample_n(dataframe1,100000)


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
proforma <- proforma %>% dplyr::rename(.,
  "name" = "Name.of.User",
  "email"="Contact.Email.of.User",
  "site_name"="Site.name",
  "location_name" = "Detector.identity",
  "lat"="Lat",
  "lon"= "Lon",
  "date"= "Date.start",
  "time" ="Pass.Time",
  "species" = "Species...Taxa.taxon.list",
  "number_of_bats" = "Number.of.bats",
  "sensitivity"= "Sensitivity",
  "public_date"= "When.can.your.data.be.made.public.",
  "pass_definition" = "Pass.Definition",
  "detector_make"= "Detector.Make",
  "detector_model"= "Detector.model",
  "detector_height_m"= "Detector.height.m",
  "roost_within_25m"= "Roost.within.25m",
  "activity_elevated_by_roost" = "Activity.elevated.by.roost",
  "linear_feature_adjacent" = "Linear.feature.adjacent",
  "linear_feature_25m" = "Linear.feature.25m",
  "anthropogenic_feature_adjacent" = "Anthropogenic.feature.adjacent",
  "anthropogenic_feature_25m" = "Anthropogenic.feature.25m",
  "temperature_c"= "Temperature.c",
  "rainfall" = "Rainfall",
  "wind_speed_mph" = "Wind.speed.mph",
  "method_of_classification" = "Method.of.classification",
  "analysis_software_used" = "Analysis.software.used",
  "county" = "County",
  "region" = "Region",
  "country" = "Country",
  "notes" = "Notes"
)

#set data types
proforma <- proforma %>% mutate(
  name = as.character(name),
  email=as.character(email),
  site_name=as.character(site_name),
  location_name = as.character(location_name),
  lat=as.numeric(lat),
  lon= as.numeric(lon),
  date= as.character(date),
  time =as.character(time),
  species = as.character(species), #is character in dataframe1 but factor in proforma
  number_of_bats = as.integer(number_of_bats),
  sensitivity= as.character(sensitivity) ,
  public_date= as.character(public_date), 
  pass_definition = as.character(pass_definition) ,
  detector_make= as.character(detector_make),
  detector_model=  as.character(detector_model),
  detector_height_m= as.character(detector_height_m), #is character in dataframe1 but integer in proforma
  roost_within_25m= as.character(roost_within_25m), #is character in dataframe1 and logical in proforma
  activity_elevated_by_roost = as.character(activity_elevated_by_roost), #is character in dataframe1 and logical in proforma
  linear_feature_adjacent = as.character(linear_feature_adjacent),
  linear_feature_25m = as.character(linear_feature_25m),
  anthropogenic_feature_adjacent = as.character(anthropogenic_feature_adjacent),
  anthropogenic_feature_25m = as.character(anthropogenic_feature_25m),
  temperature_c= as.numeric(temperature_c), #is numeric in dataframe1 and logical in proforma
  rainfall = as.character(rainfall), #is character in dataframe1 and logical in proforma
  wind_speed_mph = as.integer(wind_speed_mph), #is integer in dataframe1 and logical in proforma
  method_of_classification = as.character(method_of_classification),
  analysis_software_used = as.character(analysis_software_used),
  county = as.character(county),
  region = as.character(region),
  country = as.character(country),
  notes = as.character(notes) #is character in dataframe1 and logical in proforma
)

dataframe1 <- dataframe1 %>% mutate(
  name = as.character(name),
  email=as.character(email),
  site_name=as.character(site_name),
  location_name = as.character(location_name),
  lat=as.numeric(lat),
  lon= as.numeric(lon),
  date= as.character(date),
  time =as.character(time),
  species = as.character(species), #is character in dataframe1 but factor in proforma
  number_of_bats = as.integer(number_of_bats),
  sensitivity= as.character(sensitivity) ,
  public_date= as.character(public_date), 
  pass_definition = as.character(pass_definition) ,
  detector_make= as.character(detector_make),
  detector_model=  as.character(detector_model),
  detector_height_m= as.character(detector_height_m), #is character in dataframe1 but integer in proforma
  roost_within_25m= as.character(roost_within_25m), #is character in dataframe1 and logical in proforma
  activity_elevated_by_roost = as.character(activity_elevated_by_roost), #is character in dataframe1 and logical in proforma
  linear_feature_adjacent = as.character(linear_feature_adjacent),
  linear_feature_25m = as.character(linear_feature_25m),
  anthropogenic_feature_adjacent = as.character(anthropogenic_feature_adjacent),
  anthropogenic_feature_25m = as.character(anthropogenic_feature_25m),
  temperature_c= as.numeric(temperature_c), #is numeric in dataframe1 and logical in proforma
  rainfall = as.character(rainfall), #is character in dataframe1 and logical in proforma
  wind_speed_mph = as.integer(wind_speed_mph), #is integer in dataframe1 and logical in proforma
  method_of_classification = as.character(method_of_classification),
  analysis_software_used = as.character(analysis_software_used),
  county = as.character(county),
  region = as.character(region),
  country = as.character(country),
  notes = as.character(notes) #is character in dataframe1 and logical in proforma
)


print("proforma names changed and classes fixed")

#merge lat lon column to use as location for everything else
proforma <- tidyr::unite(proforma, latlon, c(lat, lon), remove=FALSE)
proforma$latlon <- as.character(proforma$latlon)
dataframe1$latlon <- as.character(dataframe1$latlon)

proforma <- proforma %>%
  dplyr::filter(number_of_bats > 0)

proforma <- proforma %>%
  dplyr::group_by(latlon, species, date) %>%
  dplyr::mutate(passes=sum(number_of_bats)) # should this be summarise?

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
if (save == "New records, save to database") {
  newmaster <- rbind(dataframe1, proforma)
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
  
 } else {
   print("data has already been added, data has not been saved to database")
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

print(paste0("MEMORY USAGE: ",mem_used()))

#######################################################################################################################  

#create a list of strings containing each of the species levels in the proforma NOTE has to be done to a dataframe before you've inserted all the 
#potential levels (may find you don't need the levels in the end)
listproformaspecies <- unique(proforma$species) #come from proforma

listproformaspecies <- as.character(listproformaspecies)
proforma$species <- as.factor(proforma$species)


sp_dfs <- sp_dfs2 <- sp_df_all <- list()

for (species_i in listproformaspecies) {
  sp_dfs[[species_i]] <- proforma %>%
    #group by location and night (must be date started) and sum passes
    dplyr::group_by(location_name, date) %>%
    dplyr::mutate(passes = sum(number_of_bats))
  
  sp_df_all[[species_i]] <- sp_dfs[[species_i]]
  
  if(exists("dataframe2")){
    sp_dfs2[[species_i]] <-dataframe2 %>% 
      filter(species == species_i)
    
    sp_df_all[[species_i]] <- rbind(sp_dfs[[species_i]],sp_dfs2[[species_i]])
    
  }
}

#Don't think we need df and proforma again so remove them now to save space
rm(proforma, dataframe2, onerow, firstmonth, lastmonth, premonth, postmonth)


#loop through each species
for (species_i in listproformaspecies) {
  
  #calculate quantiles
  percentiles_sp_i <- ecdf(sp_df_all[[species_i]]$passes)(sp_df_all[[species_i]]$passes)*100
  quantiles <- quantile(sp_df_all[[species_i]]$passes,c(0,0.2,0.4,0.6,0.8,0.95)) %>% as.numeric()
  
  sp_df_all[[species_i]]$percentile <- percentiles_sp_i %>% as.integer()
  
  #
  sp_df_all[[species_i]] <- sp_df_all[[species_i]] %>% mutate(
    activity_level = case_when(
      passes >= quantiles[1] & passes < quantiles[2] ~ "low",
      passes >= quantiles[2] & passes < quantiles[3] ~ "low/moderate",
      passes >= quantiles[3] & passes < quantiles[4] ~ "moderate",
      passes >= quantiles[4] & passes < quantiles[5] ~ "moderate/high",
      passes >= quantiles[5] & passes < quantiles[6] ~ "high",
      passes >= quantiles[6] ~ "exceptional",
      
    )
  )
  
  sp_df_all[[species_i]]$reference_range_size <- nrow(sp_df_all[[species_i]])
  
}


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




for (species_i in listproformaspecies) {
  Bprep <- rbind(Bprep,sp_df_all[[species_i]])
  
  Bprep <- Bprep %>%
    filter(proformaORdf == "proforma")
  
}


print("blank data frame made")
# 
# #need to rbind each back one at a time, alltogether accumulates all the data step by step 

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

print(paste0("MEMORY USAGE: ",mem_used()))
print("reached end of Ecobat2.R")
