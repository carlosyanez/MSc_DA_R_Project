#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
source("functions.R")


############################### 
### LOAD PACKAGES 

packages <- c("shinyWidgets")

loaded_packages <- paste0(search(),sep=" ",collapse = "")
packages <- tibble(package = packages)
packages <- packages %>% mutate(loaded=str_detect(loaded_packages, package, negate = FALSE)) %>% pull(package)

if(length(packages)>0 ){
    for(i in 1:length(packages)){
        result <- require(packages[i],character.only = TRUE)
        if(!result){
            install.packages(packages[i])
            library(packages[i],character.only = TRUE)
        }
    }
}
rm(packages,i,result)

#keys for selectors
sites <- read_csv("Data/Sites.csv")
sites <- sites %>% arrange(Site_Name)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Weather Report for Selected Stations in the UK"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
             pickerInput("locInput","Location", choices=sites$Site_Name,
                         options = pickerOptions(actionsBox = TRUE ,
                                         maxOptions=5,
                                        maxOptionsText="Only 5 locations allowed"
                                        ),
                         selected = sites[1,]$Site_Name,multiple = T),
             pickerInput("measInput","Measurement",choices=meas_key$key,options = list(style = "btn-primary"),selected = "Air Temperature"),
             pickerInput("statInput","Statistic",choices=stat_key$key,options = list(style = "btn-primary"),selected = "Averages"),
             pickerInput("periodInput","Period",choices=period_key$key,options = list(style = "btn-primary"),selected = "Monthly"),
             pickerInput("tlInput","Time Axis",choices=tl_key$key,options = list(style = "btn-primary"),selected = "Calendar Date"),
             leafletOutput("LocationMap")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
            tabPanel("Summary", 
                     br(),
                     fluidRow(
                             column(4,""),
                             column(4,downloadButton('downloadReportButton', 'Download Word Report')),
                             column(4,downloadButton('downloadCSVButton', 'Download CSV File'))
                                          
                     ),
                     br(),
                     girafeOutput("SummaryPlot"),
                     br(),
                     h2("Average Measurements from the last seven days"),
                     br(),
                    uiOutput("SummaryTable")),
            tabPanel("Hutton Criteria",
                     br(),girafeOutput("HuttonPlot"))
            
        ))
    )
))
