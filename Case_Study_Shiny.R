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
library(raster)
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

#library(DT)

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
                
                #textInput("textInputVehicleID", 
                #          h3("Vehicle ID:"), 
                #          placeholder  = "Enter ID here"),
                
                dataTableOutput("dynamicVehicleID"),
                
                
                checkboxGroupInput("checkGroupLevels1", 
                                   h3("Options:"), 
                                   choices = list("Longest" = 1, 
                                                  "Shortest" = 2, 
                                                  "Median" = 3),
                                   #selected = c(1,2,3)
                                   ),
                checkboxGroupInput("checkGroupLevels2", 
                                   h3("Level:"), 
                                   choices = list("Single Part" = 1, 
                                                  "Component" = 2, 
                                                  "Vehicle" = 3),
                                   selected = 1
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
                
                checkboxGroupInput("checkGroupLevels1", 
                                   h3("Options:"), 
                                   choices = list("Vehicle Types" = 1, 
                                                  "Components" = 2, 
                                                  "Individual parts" = 3),
                                   selected = c(1,2,3)
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

server <- function(input, output) {

  # load shapefile for germany
  ger_shp <- read_sf("Additional_files/DEU_adm/DEU_adm1.shp")
  
  output$map <- renderTmap({
    coord<- data.frame(Breitengrad=NA, Längengrad=NA)%>%
      na.omit()
    if (any(final_data$ID_Fahrzeug == input$textInputVehicleID)) {
      # TODO: Plot necessary data for search bar usage
      row_of_content <- final_data[which(final_data$ID_Fahrzeug == input$textInputVehicleID),]
      
      single_part_location <- coord
      component_location <- coord
      vehicle_location <- coord
      
      single_part_location <- row_of_content[,c("Breitengrad_Einzelteil", "Längengrad_Einzelteil")]
      names(single_part_location) <- gsub("_Einzelteil","", names(single_part_location))      
      component_location <- row_of_content[,c("Breitengrad_Komponente", "Längengrad_Komponente")]
      names(component_location) <- gsub("_Komponente","", names(component_location))
      vehicle_location <- row_of_content[,c("Breitengrad_Fahrzeug", "Längengrad_Fahrzeug")]
      names(vehicle_location) <- gsub("_Fahrzeug","", names(vehicle_location))
      
      print(vehicle_location)
      coord<- coord %>% full_join(single_part_location,by = c("Breitengrad", "Längengrad"))%>%
        full_join(single_part_location,by = c("Breitengrad", "Längengrad"))%>%
        full_join(vehicle_location,by = c("Breitengrad", "Längengrad"))
      print(coord)
    }
    map_germany <- tm_shape(ger_shp) +
      tm_borders() +
      tm_polygons(col = "lightblue1", 
                  alpha = 0.4, 
                  id = "NAME_2",
                  popup.vars = c("Bundesland: "="NAME_1")) +
      tm_scale_bar(position = c("left", "bottom"), width = 0.15) #+
      #tm_compass(position = c("left", "top"), size = 2)
  })
  
  output$vehiclePlot <- renderPlot({
    if (input$textInputVehicleID > 0) {
      # TODO: Plot necessary data for search bar usage
    }
  })
  
  output$dynamicVehicleID <- renderDataTable(datatable(
                                             mtcars["mpg"],
                                             rownames = FALSE,
                                             colnames = c("Vehicle ID"),
                                             filter = "none",
                                             selection="multiple",
                                             options = list(scrollY = "200px", 
                                                            scrollCollapse = TRUE,
                                                            paging = FALSE,
                                                            columnDefs = list(list(className = 'dt-left', targets = 0)),
                                                            
                                                            #columns.searchable = FALSE,
                                                            #search = list(FALSE),
                                                            #select = "single",
                                                            select.items = "row",
                                                            columns = list(searchable = FALSE),
                                                            searching = TRUE,
                                                            sDom  = '<"top">lfrt<"bottom">ip'
                                                            )
                                              )
                                             )
  
  
  
  
  output$boxPlot <- renderPlot({
    
      p <- ggplot(ToothGrowth, aes(x=dose, y=len)) + 
        geom_boxplot()
      
      p
  })
  
  
  output$dynamicDataSet <- renderDataTable(mtcars)#, options = list(pageLength = 5))
  
}

#### RUN 

shinyApp(ui = ui, server = server)
