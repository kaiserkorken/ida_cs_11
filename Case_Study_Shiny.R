#### SETUP ####

if (!require(shiny)) {
  install.packages("shiny")
  require(shiny)
}

if (!require(shinydashboard)) {
  install.packages("shinydashboard")
  require(shinydashboard)
}

if (!require(leaflet)) {
  install.packages("leaflet")
  require(leaflet)
}

if (!require(leaflet.extras)) {
  install.packages("leaflet.extras")
  require(leaflet.extras)
}

if (!require(readr)) {
  install.packages("readr")
  require(readr)
}

if (!require(shinythemes)) {
  install.packages("shinythemes")
  require(shinythemes)
}

if (!require(ggplot2)) {
  install.packages("ggplot2")
  require(ggplot2)
}

if (!require(tidyverse)) {
  install.packages("tidyverse")
  require(tidyverse)
}

if (!require(rstudioapi)) {
  install.packages("rstudioapi")
  require(rstudioapi)
}

if (!require(htmltools)) {
  install.packages("htmltools")
  require(htmltools)
}


# for loading our data
#library(raster)
library(readr)
library(readxl)
library(sf)
# for datasets
library(maps)
library(spData)
# for creating animations
library(magick)
# for plotting
library(grid)
library(tmap)
library(viridis)
library(rgdal)
library(tmaptools)
library(dplyr)
library(DT)
library(reshape2)

library(tidygeocoder)

source("Case_Study_App_functions.R")
#### UI #####

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Map", tabName = "map", icon = icon("dashboard")),
    menuItem("Boxplot", tabName = "boxplot", icon = icon("th")),
    menuItem("Material Flow", tabName = "matFlow", icon = icon("th")),
    menuItem("Data set", tabName = "dataSet", icon = icon("th"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "map",
            #h2("Map"),
            # Sidebar layout with input and output definitions ----
            sidebarLayout(
              # Sidebar panel for inputs ----
              sidebarPanel(
                
                textInput("textInputVehicleID", 
                          h3("Vehicle ID:"), 
                          placeholder  = "Enter ID here"),
                
                #dataTableOutput("dynamicVehicleID"),
                
                
                checkboxGroupInput("checkGroupDistanceComparison", 
                                   h3("Options:"), 
                                   choices = list("Longest" = 1, 
                                                  "Shortest" = 2, 
                                                  "Median" = 3),
                                   #selected = c(1,2,3)
                ),
                checkboxGroupInput("checkGroupLevelsMap", 
                                   h3("Level:"), 
                                   choices = list("Single Part to component value" = 1, 
                                                  "Component to OEM" = 2, 
                                                  "OEM to distribution center" = 3,
                                                  "OEM to state capital" = 4),
                                   selected = c(1,2,3,4)
                ),
                radioButtons("radio", 
                             h3("Vehicle Types:"),
                             choices = list("All" = 1, 
                                            "Single only" = 2),
                             selected = 1),
              ),
              
              # Main panel for displaying outputs ----
              mainPanel(
                # Output: 
                tmapOutput("map", width = "100%", height = 750), 
                plotOutput("vehiclePlot")
              ),
            )   
    ),
    
    tabItem(tabName = "boxplot",
            h2("Boxplot"),
            # Sidebar layout with input and output definitions ----
            sidebarLayout(
              # Sidebar panel for inputs ----
              sidebarPanel(
                
                checkboxGroupInput("checkGroupLevelsBoxplot", 
                                   h3("Level:"), 
                                   choices = list("Single Part to component value" = 1, 
                                                  "Component to OEM" = 2, 
                                                  "OEM to distribution center" = 3,
                                                  "OEM to state capital" = 4),
                                   selected = c(1,2,3,4)
                ),
              ),
              
              # Main panel for displaying outputs ----
              mainPanel(
                # Output: 
                plotOutput("boxPlot")
              ),
            )   
    ),
    
    tabItem(tabName = "matFlow",
            h2("Material Flow"), 
            #dataTableOutput("dynamicVehicleID"),
    ),
    
    tabItem(tabName = "dataSet",
            h2("Data set"), 
            dataTableOutput("dynamicDataSet")
    )
  )
)



# Define UI for app that draws a histogram ----
ui <- dashboardPage(
  dashboardHeader(title = "Case Study 11"),
  sidebar,
  body
)

#### SERVER FUNCTION #####

## data aggregation

final_data <- read_csv("Final_dataset_group_11.csv")

server <- function(input, output) {
  
  ## map
  
  # load shapefile for germany
  ger_shp <- read_sf("Additional_files/DEU_adm/DEU_adm3.shp")
  
  # create Germanys Map for plot and render
  map_germany <- tm_shape(ger_shp) +
    tm_borders() +
    tm_polygons(col = "lightblue1", 
                alpha = 0.4, 
                id = "NAME_3",
                popup.vars = c("Bundesland: "="NAME_1")) +
    tm_scale_bar(position = c("left", "bottom"), width = 0.15)
  # Map output
  output$map <- renderTmap({
    
    #initiate
    coord<- data.frame(Breitengrad=NA, Längengrad=NA)%>%
      na.omit()
    single_part_location <- coord
    component_location <- coord
    vehicle_location <- coord
    gemeinde_location <- coord
    state_capital_location <- coord
    # get ID from user-input
    vehicleID <- input$textInputVehicleID 
    checkB<- input$checkGroupLevelsMap
    
    #default filter: set dot to Berlin
    filtermap <- ger_shp %>%
      filter(NAME_3 == "Berlin")
    
    # switch
    inti<-0
    
    vehicleID<-"11-1-12-600035" # kann gelöscht werden
    # if Fahrzeug_ID exists in data
    # get information from data and plot in map
    if (any(final_data$ID_Fahrzeug == vehicleID)) {
      
      # get information rows from data
      row_of_content <- final_data[which(final_data$ID_Fahrzeug == vehicleID),]
      state_capital<-unique(row_of_content$Hauptstadt) # braucht man vllt nciht
      
      # get location data from all needed locations
      single_part_location <- row_of_content[,c("Breitengrad_Einzelteil", "Längengrad_Einzelteil")]
      names(single_part_location) <- gsub("_Einzelteil","", names(single_part_location))
      
      component_location <- row_of_content[,c("Breitengrad_Komponente", "Längengrad_Komponente")]
      names(component_location) <- gsub("_Komponente","", names(component_location))

      vehicle_location <- row_of_content[,c("Breitengrad_Fahrzeug", "Längengrad_Fahrzeug")]
      names(vehicle_location) <- gsub("_Fahrzeug","", names(vehicle_location))
      
      state_capital_location <- row_of_content[,c("Breitengrad_Hauptstadt", "Längengrad_Hauptstadt")]
      names(state_capital_location) <- gsub("_Hauptstadt","", names(state_capital_location))
        
      gemeinde_location <- row_of_content[,c("Breitengrad_Gemeinde", "Längengrad_Gemeinde")]
      names(gemeinde_location) <- gsub("_Gemeinde","", names(gemeinde_location))
      
      # built coordinate points of states
      coord<- coord %>% full_join(single_part_location,by = c("Breitengrad", "Längengrad"))%>%
        full_join(component_location,by = c("Breitengrad", "Längengrad"))%>%
        full_join(vehicle_location,by = c("Breitengrad", "Längengrad"))%>%
        full_join(state_capital_location,by = c("Breitengrad", "Längengrad"))%>%
        full_join(gemeinde_location,by = c("Breitengrad", "Längengrad"))
      # set filter for map 
      coord_sf <-st_as_sf(coord,coords = c( "Längengrad","Breitengrad"), crs=4326)
      filtermap <- coord_sf
      # switch to 1
      inti<-1
    }
   #default when switch is 0
   if(!inti){
     map_germany <- map_germany  +
       tm_shape(filtermap) + tm_dots(size = 0.000001) 
   }else if(inti){
     
     #built connection lines from route
     if("1" %in% checkB) {
       teil_zu_komp<-data.frame(c(single_part_location, component_location))
       map_germany <- getFilterLines(teil_zu_komp,map_germany)
                                
     }
     
     if("2" %in% checkB ){
       komp_zu_fahr<-data.frame(c(component_location, vehicle_location))
       map_germany <- getFilterLines(komp_zu_fahr,map_germany)
       
     }
     if("3" %in% checkB) {
       fahr_zu_stadt<-data.frame(c(vehicle_location, state_capital_location))
       map_germany <- getFilterLines(fahr_zu_stadt,map_germany)
       
     }
     if("4" %in% checkB ){
       stadt_zu_kunde<-data.frame(c(state_capital_location, gemeinde_location))
       map_germany <- getFilterLines(stadt_zu_kunde,map_germany)
     }

   }
    # print Map
    map_germany <- map_germany +
       tm_shape(filtermap) + tm_dots()

  })
  ## vehicle plot
  
  #output$vehiclePlot <- renderPlot({
  #  if (input$textInputVehicleID > 0) {
  #    # TODO: Plot necessary data for search bar usage
  #  }
  #})
  
  ## vehicle ID table and search
  
  #output$dynamicVehicleID <- renderDataTable(datatable(
  #  mtcars["mpg"],
  #  rownames = FALSE,
  #  colnames = c("Vehicle ID"),
  #  filter = "none",
  #  selection="single",
  #  options = list(scrollY = "200px", 
  #                 scrollCollapse = TRUE,
  #                 paging = FALSE,
  #                 columnDefs = list(list(className = 'dt-left', targets = 0)),
                   
                   #columns.searchable = FALSE,
                   #search = list(FALSE),
                   #select = "single",
  #                 select.items = "row",
  #                 columns = list(searchable = FALSE),
  #                 searching = TRUE,
  #                 sDom  = '<"top">lfrt<"bottom">ip'
  #  )
  #)
  #)
  
  
  
  
  
  ## boxplot
  
  output$boxPlot <- renderPlot({
    
    mtcars_by_cyl <- mtcars %>%
      dplyr::select(cyl, hp) %>%
      group_by(cyl) #%>%
    #summarize(total_hp = sum(hp))
    
    dist_vs_type <- mtcars_by_cyl
    colnames(dist_vs_type) = c("type", "total_dist")
    #    dist_vs_type[dist_vs_type == 4] = "Single Part to component value"
    #    dist_vs_type[dist_vs_type == 5] = "Component to OEM"
    #    dist_vs_type[dist_vs_type == 6] = "OEM to distribution center"
    
    #    If you have already made numeric vectors called "a", "b", and "c"
    single_to_component <- 1:10
    component_to_oem <- sqrt(1:200)
    oem_to_distribution <- log2(1:500)
    #    type_shortcut <- c("single_to_component","component_to_oem","oem_to_distribution")
    
    dist_vs_type <- lapply(type_shortcut, get, envir=environment())
    names(dist_vs_type) <- c("Single Part to Component", "Component to OEM", "OEM to Distribution center")
    #print(head(dist_vs_type))
    #boxplot(dist_vs_type)
    
    levels_selected <- input$checkGroupLevelsBoxplot
    data_to_plot <- dist_vs_type[as.numeric(levels_selected)] %>% # choose data according to selection 
      melt() #melt into a long vector
    colnames(data_to_plot) = c("total_dist","type")
    
    #print(levels_selected)
    print(head(data_to_plot))
    
    ggplot(data_to_plot, aes(x=type,y=total_dist)) + 
      geom_boxplot() +
      scale_fill_viridis(discrete = TRUE, alpha=0.6) +
      geom_jitter(color="black", size=0.4, alpha=0.9) +
      theme_ipsum() +
      theme(
        legend.position="none",
        plot.title = element_text(size=11)
      ) +
      ggtitle("Total distance travelled by type of material flow") +
      xlab("") +
      ylab("Distance")
    
    
  })
  
  
  output$dynamicDataSet <- renderDataTable(mtcars)#, options = list(pageLength = 5))
  
}

#### RUN 

shinyApp(ui = ui, server = server)