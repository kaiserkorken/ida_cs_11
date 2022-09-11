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

#### UI #####

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Map", tabName = "map", icon = icon("dashboard")),
    menuItem("Boxplot", tabName = "boxplot", icon = icon("th"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "map",
            h2("Map"),
            # Sidebar layout with input and output definitions ----
            sidebarLayout(
              # Sidebar panel for inputs ----
              sidebarPanel(
                
                textInput("textInputVehicleID", 
                          h3("Vehicle ID:"), 
                          value = "Enter ID here"),
                checkboxGroupInput("checkGroupLevels", 
                                   h3("Options:"), 
                                   choices = list("Longest" = 1, 
                                                  "Shortest" = 2, 
                                                  "Median" = 3),
                                   selected = c(1,2,3)),
                checkboxGroupInput("checkGroupLevels", 
                                   h3("Level:"), 
                                   choices = list("Single Part" = 1, 
                                                  "Component" = 2, 
                                                  "Vehicle" = 3),
                                   selected = 1),
                radioButtons("radio", 
                             h3("Vehicle Types:"),
                             choices = list("All" = 1, 
                                            "Single only" = 2),
                             selected = 1),
              ),
              
              # Main panel for displaying outputs ----
              mainPanel(
                # Output: 
                plotOutput(outputId = "map")
              ),
            )   
    ),
    
    tabItem(tabName = "boxplot",
            h2("Boxplot")
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

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  

  output$map <- renderPlot({

    # load shapefile for germany
    ger_shp <- read_sf("Additional_files/DEU_adm/DEU_adm2.shp")
    
    map_germany <- tm_shape(ger_shp) +
      tm_borders() +
      tm_polygons() +
      #tm_logo("datasets/chapter_2/bavaria.png", height = 2) +
      tm_scale_bar(position = c("left", "bottom"), width = 0.15) +
      tm_compass(position = c("left", "top"), size = 2)
  })
}


#### RUN 

shinyApp(ui = ui, server = server)
