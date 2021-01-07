##

library(shiny)
library(tidyverse)
source("functions.R")
title_text <- "Weather Report for Selected Stations in the UK"

############################### 
### LOAD PACKAGES 

packages <- c("shinyWidgets",   # "nicer" filter widgets - http://shinyapps.dreamrs.fr/shinyWidgets/
              "shinythemes")    #  ready to use bootstrap themes for shiny - 

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


shinyUI(fluidPage(
   theme = shinytheme("flatly"),
    title=title_text,
    tags$head(
        tags$style(HTML("
                    @import url('//fonts.googleapis.com/css2?family=Roboto&display=swap');
                    "))
    ),

    # Application title
    titlePanel(fluidRow(
                        column(11,img(src="day-cloud-snow.png",height = 40, width = 40),title_text), # https://uxwing.com/day-cloud-snow-icon/ - no attribution required
                        column(1,downloadBttn('downloadReportButton', label='Download Word Report',style='material-flat',color='primary',size='xs'))
                )
               ),

    # Sidebar with filters and map
    sidebarLayout(
        sidebarPanel(
             pickerInput("locInput","Location", choices=sites$Site_Name,
                         options = pickerOptions(actionsBox = FALSE ,
                                         maxOptions=5,
                                        maxOptionsText="Only 5 locations allowed",
                                        style = "btn-primary"
                                        ),
                         selected = sites[1,]$Site_Name,multiple = T),
             pickerInput("measInput","Measurement",choices=meas_key$key,selected = "Air Temperature"),
             pickerInput("periodInput","Period",choices=period_key$key,selected = "Monthly"),
             pickerInput("statInput","Statistic",choices=c("Averages"),selected = "Averages"),
             pickerInput("tlInput","Time Axis",choices=c("Calendar Date"),selected = "Calendar Date"),
             leafletOutput("LocationMap", width = "100%", height = 325)
            
        ),

        # Main panel with tabs with plots and table
        mainPanel(
            tabsetPanel(type = "pills",
            tabPanel("Summary Plot", 
                     h2("Summary Plot"),
                     br(),
                     br(),
                     girafeOutput("SummaryPlot",width="100%")),
            tabPanel("Last 7 Days",
                     h2("Average Measurements from the last seven days"),
                     br(),
                     downloadBttn('downloadCSVButton', label='Download 7 day Summary (CSV)',style='material-flat',color='primary',size='s'),
                     br(),
                     br(),
                     DTOutput("SummaryTable",width="100%")),
            tabPanel("Hutton Criteria",
                     h2("Hutton Criteria"),
                     br(),
                     br(),
                     girafeOutput("HuttonPlot",width="100%")),
            tabPanel("Disclaimer",
                     h2("Disclaimer"),
                     includeHTML("about.html"))
            
        ))
    )
))
