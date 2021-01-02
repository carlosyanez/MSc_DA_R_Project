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

packages <- c("shinyWidgets","shinythemes")
#shinythemes
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
   theme = shinytheme("flatly"),

    tags$head(
        tags$style(HTML("
                    @import url('//fonts.googleapis.com/css2?family=Roboto&display=swap');
                    "))
    ),

    # Application title
    titlePanel(fluidRow(
                        column(11,img(src="day-cloud-snow.png",height = 40, width = 40),"Weather Report for Selected Stations in the UK"), # https://uxwing.com/day-cloud-snow-icon/ - no attribution required
                        column(1,downloadBttn('downloadReportButton', label='Download Word Report',style='material-flat',color='primary',size='xs'))
                        #column(,downloadButton('downloadCSVButton', 'Download CSV File'))               ### DT already has a inbuilt download button
        
                )
               ),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
             pickerInput("locInput","Location", choices=sites$Site_Name,
                         options = pickerOptions(actionsBox = TRUE ,
                                         maxOptions=5,
                                        maxOptionsText="Only 5 locations allowed",
                                        style = "btn-primary"
                                        ),
                         selected = sites[1,]$Site_Name,multiple = T),
             pickerInput("measInput","Measurement",choices=meas_key$key,selected = "Air Temperature"),
             pickerInput("periodInput","Period",choices=period_key$key,selected = "Monthly"),
             pickerInput("statInput","Statistic",choices=c("Averages"),selected = "Averages"),
             pickerInput("tlInput","Time Axis",choices=c("Calendar Date"),selected = "Calendar Date"),
             leafletOutput("LocationMap", width = "100%", height = 500)
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "pills",
            tabPanel("Summary", 
                     h3("Summary Chart"),
                     girafeOutput("SummaryPlot",width="100%",height = "50%"),
                     h3("Average Measurements from the last seven days"),
                     DTOutput("SummaryTable",width="100%")),
            tabPanel("Hutton Criteria",
                     h3("Hutton Criteria")
                     ,girafeOutput("HuttonPlot"))
            
        ))
    )
))
