############################################################################################################
########################### ODL Masters in Data Analytics - University of Glasgow ########################## 
###########################  R Programming T1 2020/2021                           ########################## 
###########################  Assignment 4 (PROJECT) - Due Date 25 Jan 2021        ##########################
#####  AUTHOR: Carlos YÁÑEZ SANTIBÁÑEZ   ###################################################################

library(shiny)
library(tidyverse)

source("functions.R")


sites <- read_csv("Data/Sites.csv")  #### loaded in UI


shinyServer(function(input, output,session) {
      
    site_names <- paste(sites$Site_Name,collapse = ", ")
    values <- reactiveValues()
    values$station_data <- 1
   
    
    toListenPlot <- reactive({
        list(input$locInput,
             input$measInput,
             input$statInput,
             input$periodInput,
             input$tlInput)
    })
    
    
    ### Select Sites
    observeEvent(input$locInput,{
         message(input$locInput)
         message(length(input$locInput))
            if(length(input$locInput)>0){
                #load and process data
                
                values$sites <- sites %>% filter(Site_Name %in% input$locInput)
                values$station_data<- data_loader(values$sites, values$station_data)
                values$processed_data <- aggregate_data(values$station_data)
                
                #save data in "transfer" file to speed up word report
                # saveRDS(values$processed_data,"processed_data.RDS")  # decided not to use!
            }
            
    })
    
    ### Select variables
    observeEvent(toListenPlot(),{
        if(length(input$locInput)>0){
            values$meas   <- meas_key[which(meas_key$key==input$measInput),]$value
            values$stat    <- stat_key[which(stat_key$key==input$statInput),]$value
            values$period  <- period_key[which(period_key$key==input$periodInput),]$value      
            values$tl      <- tl_key[which(tl_key$key==input$tlInput),]$value  
            values$pS<- plot_data(values$processed_data,
                                  values$period,
                                  values$stat,
                                  values$meas,
                                  values$tl,
                                  interactive_flag=TRUE)
        }
        
    message(isolate(session$clientData$url_hostname))    
    })
    
    ###Change filter options based on period 
    
    observeEvent(input$periodInput,{
        
        if(input$periodInput=="Daily"){
            choice_values_stat <- stat_key %>% filter(daily_filter) %>% select(key) %>% pull(.)
            choice_values_tl <-   tl_key %>% filter(daily_filter) %>% select(key) %>% pull(.)
            
        }
        
        if(input$periodInput=="Monthly"){
            choice_values_stat <- stat_key %>% filter(monthly_filter) %>% select(key) %>% pull(.)
            choice_values_tl <-   tl_key %>% filter(monthly_filter) %>% select(key) %>% pull(.)
            
        }
        
        if(input$periodInput=="Raw Data"){
            choice_values_stat <- stat_key %>% filter(raw_filter) %>% select(key) %>% pull(.)
            choice_values_tl <-   tl_key %>% filter(raw_filter) %>% select(key) %>% pull(.)
            
        }
        
        updatePickerInput(
            session,
            "statInput",
            choices = choice_values_stat,
        )
        
        updatePickerInput(
            session,
            "tlInput",
            choices = choice_values_tl,
        )
        
    })
    
    

#### SUMMARY TABLE
    output$SummaryTable <- renderDT({

        seven_day_DT(values$processed_data) 

    })

#### SUMMARY PLOT    
    output$SummaryPlot <- renderGirafe({
        girafe(ggobj = values$pS, width_svg = 8, height_svg = 4,
               options = list(
                   opts_hover_inv(css = "opacity:0.1;"),
                   opts_hover(css = "stroke-width:2;")
               ))
    })
    
#### HUTTON PLOT    
    output$HuttonPlot <- renderGirafe({
        pH <- hutton_plot(values$processed_data,interactive_flag=TRUE)
        girafe(ggobj = pH, width_svg = 8, height_svg = 4,
               options = list(
                   opts_hover_inv(css = "opacity:0.1;"),
                   opts_hover(css = "stroke-width:2;")
               ))
    })    

####LOCATION MAP (WITH LEAFLET)
    output$LocationMap <- renderLeaflet({
        location_map(values$sites)
        
        
    })    
####DOWNLOAD WORD REPORT
    
    output$downloadReportButton <- downloadHandler(
        filename = "report.docx",
        content = function(file){
        render("Word_Report.Rmd", 
               output_file=file, params=list(site_selection=input$locInput,domain=session$clientData$url_hostname))
        # ˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆ
        # input$a now available as params$a
        # in rmarkdown
        # (remember to define parameters in
        # header of Rmd file)
        }
    )
####DOWNLOAD CSV FILE
    
    output$downloadCSVButton <- downloadHandler(
        filename = function() {
            site_names <- paste(values$sites$Site_Name,collapse = "_")
            paste("summary",site_names, ".csv", sep = "")
        },
        content = function(file) {
            write.csv(seven_day_dataset(values$processed_data,4), file, row.names = TRUE)
        }
    )
    
    
})
