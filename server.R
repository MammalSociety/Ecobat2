#load packages - not sure if necessary
library(rsconnect)
library(shiny) 
library(rmarkdown) 
library(ggplot2)
library(knitr)
library(pander)
library(dplyr)
library(tidyr)
library(rcompanion)
library(suncalc)
library(ggforce)
library(janitor)
library(rdrop2)
library(shinyjs)
library(tinytex) #need for creating a pdf

# required in Ecobat2.R
library(tidyverse)
library(rsconnect) #for connecting to shiny.io
library(rdrop2) #for connecting to dropbox
library(lubridate)

# Required Nightly.Rmd
library(plyr) # # percentiles code uses plyr as its older
library(suncalc) # Sunrise/ sunset times
library(gtable) #since doing ggforce fix it now wants this package
library(reshape2) # wide/short data format conversions
library(RColorBrewer) #colourblind friendly scales on figures
library(png)
library(grid)

library(lobstr) # for checking memory

#drop_auth(rdstoken = "droptoken.rds")

#for if ever need to make a new token
# token <- drop_auth()
# saveRDS(token, "droptoken.rds")
# token <- readRDS("droptoken.rds")

options(shiny.maxRequestSize=50*1024^2)
# the default file size limit is 50MB, the above code ups it to 50MB

#print some session info
sessionInfo()
mem_used()
system('grep MemTotal /proc/meminfo')

shinyServer(function(input, output) {

  output$contents <- renderTable({
    
  data2 <- reactive({
  # input$file1 will be NULL initially. After the user selects and uploads a file, it will be a data frame with 'name', 'size', 'type', and 'datapath' columns. The 'datapath' column will contain the local filenames where the data can be found.
      
      data1 <- input$file #grabs uploaded file
      
      if (is.null(data1)) #if no data has been uploaded, main panel is empty
        return(NULL)
      
      
      data2 <- read.csv(data1$datapath, header=TRUE)
      data2
    })
    
    output$contents <- renderTable({
    head(data2())
      
    })
    
  })

  # this reactive output tells Shiny that once data has been uploaded it should display a   download button for the
  # Rmd report. If no data has been uploaded then the button will not appear.
  output$ui.download <- renderUI({
    if (is.null(input$file)) return() #if no data uploaded button is not present
    downloadButton("report", "Generate report")}) #if data uploaded button appears
  
  output$ui.download.helper <- renderUI({
    if (is.null(input$file)) return() #if no data uploaded, helper text associated with      the download button not present
    h5(helpText("Report may take a few minutes to generate; please be patient."))})
  #if data uploaded, helper text associated with the download button appears
  
  output$report <- downloadHandler(
    filename = "Ecobat Nightly Report.pdf", #we want word document output
    
    content = function(file) {
      withProgress(message = "Collating data...", value = 0, {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "Nightly.Rmd")
        file.copy("Nightly.Rmd", tempReport, overwrite = TRUE)
  
        #**CRUCIAL CODE
        # this code is responsible for passing the data to Rmd.
        dataa <- input$file #tells R Markdown where it can find data
        datab <- read.csv(dataa$datapath, header=TRUE) #tells Rmd where to read the data from
        author <- input$Author #tells Rmd what to use as Author
        sitename <- input$SiteName #tells Rmd what to use as Site Name
        geofilter <- input$GeoFilter
        timefilter <- input$TimeFilter
        save <- input$Save
        token <- readRDS("droptoken.rds")
        #**END CRUCIAL CODE
        
        print(str(datab))
        
        # Set up parameters to pass to Rmd document
        params <<- list(n = datab, 
                       Author = author, 
                       SiteName = sitename,
                       GeoFilter = geofilter,
                       TimeFilter = timefilter,
                       Save = save,
                       Token = token)
        
        incProgress(0.3, "Pre-processing, this may take a minute...")
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        #source("Ecobat2.R", local = knitr::knit_global()) 
        source("Ecobat2.R") 
        
        incProgress(0.4, "Building report, this may take a minute...")
        
        rmarkdown::render(file.path(tempdir(), "Nightly.Rmd"),
                          output_file = file,
                          params = params#,
                          #envir = new.env(parent = globalenv())
                          )
        
        incProgress(1, "Report complete")
                          
      })
    })  
  
})

