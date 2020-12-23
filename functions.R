############################### FUNCTIONS USED ACROSS SHINY AND MARKDOWN ############################### 
### !!!!! WARNING !!!!! THE FOLLOWING SCRIPT WILL INSTALL PACKAGES


############################### 
### LOAD PACKAGES 

packages <- c("lubridate","leaflet","maps","paletteer","ggsci","showtext","ggiraph","data.table",
              "flextable","officer","rmarkdown")

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


############################### 
### LOAD FONTS USED IN PLOTS 

font_add_google("Titillium Web","Titillium")
font_add("Arial Narrow","Arial Narrow")
showtext_auto()


#############################
### Keys

meas_key <- tibble(key=c("Air Temperature","Relative Humidity","Wind Speed","Visibility"),
                   value=c("air_temperature","rltv_hum","wind_speed","visibility"))
stat_key <- tibble(key=c("Averages","Maxima","Minima"),
                   value=c("mean","max","min"))
period_key <- tibble(key=c("Daily","Monthly","Raw Data"),
                     value=c("daily","monthly","raw"))
tl_key <-tibble(key=c("Calendar Date","Day of the Week","Day of the Month","Hour of the Day"),
                      value=c("Date","day_of_week","day_of_month","hour"))



############################### 
### CapStr function
#https://rstudio-pubs-static.s3.amazonaws.com/408658_512da947714740b99253228f084a08a9.html

#' Capitalise first letter of string
#' Copied from https://rstudio-pubs-static.s3.amazonaws.com/408658_512da947714740b99253228f084a08a9.html
#' @param  y string 
#' @return string with first letter in uppercase

CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}


############################### 
### Hutton Criteria

#' Calculate the Hutton Criteria
#' @param  station_data "raw": station data vector
#' @return tibble with Date and logical value - TRUE if Hutton Criteria have been met
hutton_criteria <- function(station_data){
  
  result <- station_data                                   %>%  #get data
    mutate(hum90pc=(rltv_hum>=90))                         %>%  # check if humidity of hour is 90% or more
    group_by(Site,Site_Name,Date)                          %>% 
    summarise(min_temp=min(air_temperature),                    #determine min temperature
              hum90pc_count=sum(hum90pc),                       #determine 90% humidity condition
              .groups="drop")                              %>%
    ungroup()                                              %>%
    group_by(Site,Site_Name)                               %>%
    arrange(Date)                                          %>%  #arrange by date
    mutate(hutton_temp=(min_temp>=10),                          #temperature criterion
           hutton_hum=(hum90pc_count>=6))                  %>%  #humidity criterion
    select(Date, hutton_temp,hutton_hum)                   %>%  
    mutate(hutton_temp_yd = lag(hutton_temp,1),
           hutton_hum_yd  = lag(hutton_hum,1))             %>%  #get values for previous day
    mutate(hutton =  (hutton_temp & hutton_temp_yd)&
             (hutton_hum  & hutton_hum_yd))        %>%  #get result of hutton criteria
    filter(!is.na(hutton_temp_yd) & !is.na(hutton_hum_yd)) %>%  #remove first day
    ungroup()                                              %>%
    select(Date, Site,Site_Name,hutton)                         #extract only dates and hutton result
  
  
  return(result)
  
}

############################### 
### Load station data

#' Calculate the Hutton Criteria
#' @param  sites tibble with (subset) of stations of add (loaded from sites.csv)
#' @return tibble "raw" station data 
data_loader <- function(sites){
  
  site_data <- list()
  
  #get each file into one list item each
  for(i in 1:nrow(sites)){
    site_data[[i]] <- read_csv(paste("Data/Site_",sites[i,]$Site_ID,".csv",sep=""))
    site_data[[i]] <- site_data[[i]] %>%  
      mutate(Date=as_date(ob_time)) %>%
      mutate(Site_Name=sites[i,]$Site_Name)
  } 
  site_data <- rbindlist(site_data)          # collapse list into one tibble - 
                                             #from https://stackoverflow.com/questions/26177565/converting-nested-list-to-dataframe
  
  return(site_data)
  
}

############################### 
### Generate data aggregates

#' Create all daily and monthly stats, and hutton criteria in one list
#' @param  station_data "raw" station data
#' @return tibble "raw" station data, "daily" daily stats, "monthly" monthly stats , "hutton" hutton criteria results
aggregate_data <- function(station_data){
  
  result <- list()  
  
  result$raw <- station_data %>% 
    mutate(day_of_week=wday(Date),                                           #day of the week  
           hour_of_week=hour+(day_of_week-1)*24,                             #hour of the week
           day_of_month=day(Date))                                           # day of the month
  
  result$daily <-  station_data %>%  group_by(Site,Site_Name,Date)   %>% 
    summarise(mean_air_temperature=mean(air_temperature,na.rm = TRUE),              #calculate daily means
              mean_rltv_hum = mean(rltv_hum,na.rm = TRUE),
              mean_wind_speed = mean(wind_speed,na.rm = TRUE),
              mean_visibility = mean(visibility,na.rm = TRUE),
              max_air_temperature=max(air_temperature,na.rm = TRUE),                #calculate daily maxs
              max_rltv_hum = max(rltv_hum,na.rm = TRUE),
              max_wind_speed = max(wind_speed,na.rm = TRUE),
              max_visibility = max(visibility,na.rm = TRUE),  
              min_air_temperature=min(air_temperature,na.rm = TRUE),                #calculate daily mins
              min_rltv_hum = min(rltv_hum,na.rm = TRUE),
              min_wind_speed = min(wind_speed,na.rm = TRUE),
              min_visibility = min(visibility,na.rm = TRUE),
              .groups = "drop") %>%
    mutate(day_of_week=wday(Date),                                                  #day of the week
           day_of_month=day(Date))                                                  #day of the month
  
  result$monthly <- station_data %>% 
    mutate(Date=floor_date(Date, "month")) %>% group_by(Site,Site_Name,Date) %>%
    summarise(mean_air_temperature=mean(air_temperature,na.rm = TRUE),              #calculate daily means
              mean_rltv_hum = mean(rltv_hum,na.rm = TRUE),
              mean_wind_speed = mean(wind_speed,na.rm = TRUE),
              mean_visibility = mean(visibility,na.rm = TRUE),
              .groups = "drop")
  
  
  result$hutton <-   hutton_criteria(station_data) %>% 
                     mutate(Date=floor_date(Date, "month"))   %>%   
                     group_by(Site,Site_Name,Date) %>%
                     summarise(hutton_days=sum(hutton),.groups = "drop") %>%
                     ungroup()
  
  return(result)
  
}

############################### 
### Plotting Function

#' plotting function
#' @param  processed_data output of aggregate_data
#' @param  chart_value  type of plot: raw, daily or monthly
#' @param  stat_value   mean, max,min, none
#' @param  meas_value   wind_speed,air_temperature,rltv_hum, visibility
#' @param  time_value   Date,#day_of_week, #hour_of_week
#' @param  interactive_flag whether output is ggplot or ggiraph object (default FALSE)
#' @return plot
plot_data <- function(processed_data, chart_value,stat_value,meas_value,time_value,interactive_flag=FALSE){

  if(chart_value=="raw") stat_value<-"none"
  if(chart_value=="monthly") time_value<-"Date"  
  if(chart_value=="monthly") stat_value<-"mean"  
  
  text_values <- tribble(~key,~text,
                         "raw","",
                         "daily","daily",
                         "monthly","monthly",
                         "Date","Date",
                         "hour_of_day","Our of the day",
                         "day_of_week","Day of the week",
                         "hour","Hour of the week",
                         "mean","average",
                         "max","max.",
                         "min","min.",
                         "wind_speed","Wind Speed",
                         "air_temperature","Air Temperature",
                         "rltv_hum","Relative Humidity",
                         "visibility","Visibility",
                         "Site_Name","Site Name"
  )
  
  
  
  x.value<-time_value
  y.value<- ifelse(stat_value=="none",meas_value,paste(stat_value,meas_value,sep="_"))
  colour.value <-"Site_Name"
  title.text <- paste(text_values %>% filter(key==meas_value) %>% pull(text),
                      " (",
                      text_values %>% filter(key==chart_value) %>% pull(text),
                      " ",
                      text_values %>% filter(key==stat_value) %>% pull(text),
                      ")",sep="")
  
  x.text <- text_values %>% filter(key==x.value) %>% pull(text)
  y.text <- paste(text_values %>% filter(key==chart_value) %>% pull(text) %>% CapStr(.),
                  text_values %>% filter(key==stat_value) %>% pull(text),
                  text_values %>% filter(key==meas_value) %>% pull(text) %>% tolower(.),
                  sep=" ")
  
  
  plotting_data <- processed_data[[which(names(processed_data)==chart_value)]] %>% 
    select(matches(paste(x.value,y.value,colour.value,sep="|"))) 


  p <- plotting_data %>% ggplot(aes_string(x=x.value,y=y.value,colour=colour.value)) +
    theme_minimal() +
    theme(legend.position="right",
          plot.title = element_text(size=16,face="bold",colour = "#272928",family="Titillium"),
          plot.subtitle =element_text(size=10,colour = "azure4",family="Titillium"),
          plot.caption =  element_text(size=10,colour = "azure4",family="Titillium"),
          legend.text = element_text(size=10,colour = "#272928",family="Titillium")) +
    labs(title = title.text,
         x= x.text,
         y= y.text)
  
  tooltip_value <- "Site_Name"
  if(time_value=="Date"){
    if(interactive_flag==FALSE){
      p<- p + geom_line()
    }else{
      p <-p + geom_line_interactive(aes_string(tooltip=tooltip_value,data_id=colour.value))
    }
  }else{
    if(interactive_flag==FALSE){
      p<- p + geom_point()
    }else{
      p<- p + geom_point_interactive(aes_string(tooltip = tooltip_value,data_id=colour.value))
    }
  }
  
  return(p)
}

############################### 
### seven day data set

#' Create tibble with stats from last seven days
#' @param  processed_data output of aggregate_data
#' @param rounding_value rounding precision for stats
#' @return dataset
seven_day_dataset <-function(processed_data,rounding_value=2){
  
  table_data <- 
    processed_data$daily %>% group_by(Site,Site_Name) %>%
    filter(Date>(max(Date)-ddays(7))) %>%
    ungroup() %>%
    select(Site_Name,Date,
           colnames(processed_data$daily)[which(grepl("mean",colnames(processed_data$daily)))]) %>%
    mutate(across(where(is.numeric), round, rounding_value)) 
  
  return(table_data)
  
}


############################### 
### seven day data table

#' Create table with stats from last seven days
#' @param  processed_data output of aggregate_data
#' @return table
seven_day_datatable <- function(processed_data){
  
  table_data <- seven_day_dataset(processed_data)
  
  result <- flextable(table_data)     %>%
    autofit()           %>%
    theme_booktabs()     %>%
    merge_v(j = ~ Site_Name) %>%
    set_header_labels( 
      Site_Name = "Site Name", 
      Date ="Date",
      mean_air_temperature="Avg. Air Temp",
      mean_rltv_hum="Avg. Rel Hum",
      mean_wind_speed="Avg. Wind Speed",
      mean_visibility="Avg Visibility"
    )
  
  #from https://stackoverflow.com/questions/44700492/r-flextable-how-to-add-a-table-wide-horizontal-border-under-a-merged-cell
  
  row_loc <- rle(cumsum( result$body$spans$columns[,1] ))$values
  bigborder <- officer::fp_border(style = "solid", width=2)
  
  
  result <- result %>% 
    border(border.bottom = bigborder, i=row_loc, j = 2:6, part="body") 
  result <- result %>% 
    border(border.bottom = bigborder, 
           i = result$body$spans$columns[,1] > 1, j = 1, part="body") %>% 
    border(border.bottom = bigborder, border.top = bigborder, part = "header")
  
  result
  
  return(result)
  
}

############################### 
### Map with all locations

#' create leaflet map with all locations
#' @param  sites sites data frame loaded from sites.csv (or subset of)
#' #' @return leaflet map
location_map <- function(sites){
  
  bounds <- map("world", "UK", fill = TRUE, plot = FALSE) # create UK bounds  
  # https://stackoverflow.com/questions/49512240/how-to-assign-popup-on-map-polygon-that-corresponds-with-the-country-r-leaflet
  
  map <- leaflet(options=leafletOptions(dragging=FALSE,minZoom = 5,maxZoom = 13)) %>%
    addProviderTiles("CartoDB") %>%
    addPolygons(data = bounds, group = "Countries", 
                color = "red", 
                weight = 2,
                fillOpacity = 0.0) %>%
    addCircleMarkers(~Longitude, ~Latitude,data=sites,
                     color="navy",
                     radius=5,
                     popup = ~Site_Name)
  
  
  return(map)
  
}


############################### 
### Plot Monthly Summary of Hutton Criteria

hutton_plot <-function(processed_data,interactive_flag=FALSE){
  
  p<-processed_data$hutton %>% filter(!is.na(hutton_days)) %>%
    ggplot(aes(x=Date,y=hutton_days,colour=Site_Name)) +
    theme_minimal() +
    theme(legend.position="right",
          plot.title = element_text(size=16,face="bold",colour = "#272928",family="Titillium"),
          plot.subtitle =element_text(size=10,colour = "azure4",family="Titillium"),
          plot.caption =  element_text(size=10,colour = "azure4",family="Titillium"),
          legend.text = element_text(size=10,colour = "#272928",family="Titillium")) +
    labs(title = "Summary of Days meeting the Hutton Criteria",
         x= "Date",
         y= "Number of Days") 
  
  if(interactive_flag==FALSE){
    p<- p + geom_point()
  }else{
    p <-p + geom_point_interactive(aes(tooltip=Site_Name,data_id=Site_Name))
  }
  
  return(p)
}

