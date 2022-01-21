#load packages - not sure if necessary
library(shiny)
library(rmarkdown) 
library(ggplot2)
library(knitr)
library(pander)
library(dplyr)
library(tidyr)
library(rcompanion)

ui<-(fluidPage( #fluidpage means page width determined by window size
  titlePanel("*Within Night* Bat Activity Analysis"), #title of Shiny app
  sidebarLayout( #we want the layout with a sidebar and a main panel
    sidebarPanel( #options for sidepanel
      
      #*Input() functions,
      #*Output() functions
      #add Ecobat logo at the top centre
      h5(img(src= "ecobat-logo.png", heigth=100, width=100, style="display: block; margin-left: auto; margin-right: auto;")),
      
      fileInput(inputId = "file", label = "Upload CSV file", #upload field
                multiple = FALSE, #can only upload one file at a time
                accept = c( #these are the kinds of files it will accept
                  "text/csv",
                  "text/comma-separated-values, text/plain",
                  ".csv")),
      
      helpText("Max. file size is 50MB"), #input decorations
      buttonLabel = "Browse...",
      placeholder = "No file selected",
      
      tags$br(), #adds a gap between two sections
      
      textInput(inputId = "Author", label = "Insert Author Name"), #input field for author name
      textInput(inputId = "SiteName", label = "Insert Site Name"), #input field for site name
      selectInput("GeoFilter", "Geographic Filter", c("All Data", "County", "Region", "Country")),
      selectInput("TimeFilter", "Time Filter", c("All Data", "+/- 1 month from survey start date")),
      selectInput("Save", "Save Options", c("Please Select", "New records, save to database","Already uploaded records, do NOT save to database")),
      
      tags$hr(), #adds a horizontal line
      uiOutput("ui.download"), # instead of conditionalPanel
      uiOutput("ui.download.helper"), #these two lines of code enable conditional appearance of download button
      #and helper text conditional on data having been uploaded
      tags$hr(), #adds a horizontal line
      h5(img(src= "TMS_logo.png", heigth=100, width=100, style="display: block; margin-left: auto; margin-right: auto;")) #gets it to display Ecobat logo
    ),
    mainPanel(
      uiOutput('contents') #display in main panel dependent on 'contents' in server.R
    )
  )
))
