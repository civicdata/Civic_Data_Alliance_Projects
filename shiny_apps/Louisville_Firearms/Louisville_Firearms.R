library(shinydashboard)
library(shiny)
library(leaflet)
library(rgdal)
library(gpclib)
library(maptools)
library(R6)
library(raster)
library(broom)
library(scales)
library(reshape2)
library(zoo)
library(ggthemes)
library(tidyverse)
gpclibPermit()

rlk_theme<-function(base_family = "Helvetica", base_size = 12) {
  theme_bw(base_family = base_family, base_size = base_size) +
    theme(
      plot.title = element_text(face = 'bold', hjust = 0, color="#4292c6", size= 24),
      plot.subtitle = element_text(face = 'bold', hjust = 0, color="#4292c6", size= 14),
      text = element_text(colour = 'black'),
      panel.background = element_blank(),
      strip.background = element_blank(),
      plot.background = element_rect('#F6F6F6'),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color="#4292c6",size=rel(1.75)),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.background = element_blank(),
      legend.title = element_text(face='bold', size=8, color="#4292c6"),
      legend.position = 'bottom',
      legend.direction = 'horizontal',
      legend.key = element_blank(),
      legend.text= element_text(face='bold', size=8, color="#4292c6"),
      strip.text = element_blank(),
      axis.text.y = element_text(size = 10, color='#000000'),
      axis.text.x = element_text(size = 10, color='#000000'),
      axis.title = element_text(face = 'bold', size = 10, color='#000000'),
      axis.ticks = element_blank()
    )
}




firearms_data <- read_csv('FIREARMS_DATA_W_YEAR.csv')
firearms_data <- mutate(firearms_data, RECOVERY_DATE = as.Date(firearms_data$RECOVERY_DATE, format = '%m/%d/%Y'),
                        age_group = cut(as.numeric(firearms_data$PERSON_RECOVERED_FROM_AGE), breaks = 10*(0:10)),
                        shooting_yearmon = as.yearmon(RECOVERY_DATE),
                        UCR_CATEGORY = as.factor(UCR_CATEGORY),
                        TYPE_OF_FIREARM = as.factor(TYPE_OF_FIREARM),
                        FIREARM_MANUFACTURER = as.factor(FIREARM_MANUFACTURER),
                        FIREARM_CALIBER = as.factor(FIREARM_CALIBER),
                        PERSON_RECOVERED_FROM_RACE = as.factor(PERSON_RECOVERED_FROM_RACE),
                        PERSON_RECOVERED_FROM_SEX = as.factor(PERSON_RECOVERED_FROM_SEX)
)

jeff_co_zips<-c(40118,40201,40203,40202,40205,40204,40207,40206,40209,40208,40211,40210,40213,40212,40215,40214,40217,40216,40219,40218,40220,40223,
                40222,40225,40229,40228,40241,40243,40242,40245,40257,40258,40272,40025,40023,40027,40292,40291,40299,40059)

categories <- c('Type of Incident' = 'UCR_CATEGORY',
                'Type of Firearm' = 'TYPE_OF_FIREARM',
                'Firearm Manufacturer' = 'FIREARM_MANUFACTURER',
                'Firearm Model' = 'FIREARM_MODEL',
                'Firearm Caliber' = 'FIREARM_CALIBER',
                'Race' = 'PERSON_RECOVERED_FROM_RACE')

ky_zips<-readOGR('./shapefile', layer='ky_zips')
ky_zips@data$id <- rownames(ky_zips@data)
ky_zips.points <- tidy(ky_zips, region = 'id')
ky_zips.df <- inner_join(ky_zips.points, ky_zips@data, by='id')



create_leaflet <- function(analysis_year, analysis_factor, filter_value){
  firearm_summary<-create_table(analysis_year, analysis_factor, filter_value)
  
  lou_zips <- subset(ky_zips, ZCTA5CE10 %in% jeff_co_zips)
  lou_zips@data <- left_join(lou_zips@data, firearm_summary, by=c('ZCTA5CE10' = 'RECOVERY_ZIPCODE')) %>% 
    mutate(analysis = ifelse(is.na(analysis), 0, analysis))
  
  pop<-paste0('<strong>',lou_zips$ZCTA5CE10,':</strong> ',lou_zips$analysis)
  lou_zips@data$color <- colorBin('YlOrRd', NULL, n = 7)(lou_zips$analysis)
  
  leaflet(data = lou_zips) %>% 
    addTiles() %>% 
    setView(lng = -85.7369, lat = 38.1757, zoom = 10) %>% 
    addPolygons(data = lou_zips, 
                stroke = F, 
                smoothFactor = 0.2, 
                fillOpacity = 0.75, 
                fillColor = lou_zips@data$color,
                color ="#BDBDC3",
                popup = pop) %>% 
    addPolylines(weight = 2, color='black')
}

create_table <- function(analysis_year, analysis_factor, filter_value){
  firearms_data$level_of_analysis <- firearms_data[analysis_factor]
  filter(firearms_data, year == as.numeric(analysis_year),
                          level_of_analysis == filter_value) %>% 
    group_by(RECOVERY_ZIPCODE) %>% 
    dplyr::summarize(analysis = n()) %>% 
    mutate(RECOVERY_ZIPCODE = as.factor(RECOVERY_ZIPCODE),
           analysis = ifelse(is.na(analysis), 0, analysis)) %>% 
    arrange(desc(analysis))
}

create_plot <- function(zip_code, analysis_factor, filter_value){
  firearms_data$level_of_analysis <- unlist(firearms_data[analysis_factor])
  if(zip_code == 'All'){
    plot_data<- firearms_data %>% 
      filter(level_of_analysis == filter_value, !is.na(year)) %>% 
      group_by(year) %>% 
      dplyr::summarize(total = n()) %>% 
      arrange(desc(total)) %>% 
      mutate(color = 'bluish')
  }else{
    plot_data<- firearms_data %>% 
      filter(RECOVERY_ZIPCODE == as.numeric(zip_code), level_of_analysis == filter_value, !is.na(year)) %>% 
      group_by(year) %>% 
      dplyr::summarize(total = n()) %>% 
      arrange(desc(total)) %>% 
      mutate(color = 'bluish')
  }

  
  ggplot(plot_data, aes(x = as.character(year), y=total, fill = color, label = total)) + 
    geom_bar(stat='identity')+
    geom_label() +
    scale_fill_manual(values = c('bluish' = '#2171b5'))+
    labs(title = paste0(analysis_factor,' ~ ',filter_value),
         subtitle = zip_code,
         y = 'Number of Incidents',
         x = '')+
    rlk_theme() + 
    theme(legend.position = 'none')
}

trending_plot <- function(analysis_factor, filter_value){
  firearms_data$level_of_analysis <- firearms_data[analysis_factor]
  filter(level_of_analysis == filter_value) %>% 
    group_by(RECOVERY_ZIPCODE) %>% 
    dplyr::summarize(analysis = n()) %>% 
    mutate(RECOVERY_ZIPCODE = as.factor(RECOVERY_ZIPCODE),
           analysis = ifelse(is.na(analysis), 0, analysis)) %>% 
    arrange(desc(analysis))
}


ui<-dashboardPage(
  dashboardHeader(title = 'Gun Incidents in Louisville, KY',
                  titleWidth = 600),
  dashboardSidebar(
    selectInput('maps_or_graphs', 'Maps or Graphs?', c('Maps' = 'map', 'Graphs' = 'graph'), selected = 'graph'),
    conditionalPanel(
      condition = 'input.maps_or_graphs == "map"',
      selectInput('year','Select Year', c(2016,2015,2014,2013,2012,2011,2010), selected = 2016)
    ),
    conditionalPanel(
      condition = 'input.maps_or_graphs == "graph"',
      selectInput('zip','Zip Code:', c('All', jeff_co_zips), selected = 'All')
    ),
    selectInput('stat', 'Select Category', categories, selected = 'UCR_CATEGORY'),
    uiOutput('selectFilter')
  ),
  dashboardBody(
    conditionalPanel(
      condition = "input.maps_or_graphs == 'map'",
      tags$style(type = "text/css", "#map {height: calc(100vh - 400px) !important;}"),
      leafletOutput('map'),
      dataTableOutput('data_in_table')
    ),
    conditionalPanel(
      condition = "input.maps_or_graphs == 'graph'",
      plotOutput('make_a_plot')
    )
  )
)

server<-function(input,output){
  selected_stat <- reactive({input$stat})
  selected_filter <- reactive({input$filter_selection})
  output$map <- renderLeaflet({
    create_leaflet(input$year, selected_stat(), selected_filter())
  })

  output$selectFilter<-renderUI({
    selectInput('filter_selection', 'Filter By:', as.character(unique(unlist(firearms_data[selected_stat()]))))
  })

  output$data_in_table<-renderDataTable({
    dt <- create_table(input$year, selected_stat(), selected_filter())
    names(dt) <- c('Zip Code', 'Number of Incidents')
    dt
  }, options = list(pageLength = 8))
  
  output$make_a_plot<-renderPlot({
    if(is.null(selected_filter())){
      plot(create_plot(input$zip, selected_stat(), unlist(unique(firearms_data[selected_stat()]))[1]))
    }else{
      plot(create_plot(input$zip, selected_stat(), selected_filter()))
    }
  })
}

shinyApp(ui, server)
