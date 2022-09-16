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
if (!require(readr)) {
  install.packages("readr")
  require(readr)
}

if (!require(readxl)) {
  install.packages("readxl")
  require(readxl)
}

if (!require(sf)) {
  install.packages("sf")
  require(sf)
}
# for datasets
if (!require(maps)) {
  install.packages("maps")
  require(maps)
}
if (!require(spData)) {
  install.packages("spData")
  require(spData)
}
# for creating animations
if (!require(magick)) {
  install.packages("magick")
  require(magick)
}
# for plotting
if (!require(grid)) {
  install.packages("grid")
  require(grid)
}
if (!require(tmap)) {
  install.packages("tmap")
  require(tmap)
}
if (!require(viridis)) {
  install.packages("viridis")
  require(viridis)
}

if (!require(rgdal)) {
  install.packages("rgdal")
  require(rgdal)
}
if (!require(tmaptools)) {
  install.packages("tmaptools")
  require(tmaptools)
}
if (!require(dplyr)) {
  install.packages("dplyr")
  require(dplyr)
}
if (!require(DT)) {
  install.packages("DT")
  require(DT)
}
if (!require(reshape2)) {
  install.packages("reshape2")
  require(reshape2)
}
if (!require(hrbrthemes)) {
  install.packages("hrbrthemes")
  require(hrbrthemes)
}
if (!require(tidygeocoder)) {
  install.packages("tidygeocoder")
  require(tidygeocoder)
}

source("Additional_files/Case_Study_App_functions.R")

### data aggregation 
# runs once when starting

# check if Final_Data is present
if (file.exists("Final_Data_Group_11.csv")) {
  # load Dataset and subsets
  source("Additional_files/Case_Study_App_load_data.R")
} else {
  # generate Data from scratch
  source("Additional_files/Case_Study_App_generate_data.R")
}

colnames(final_data)[12]<-"ORT_Fahrzeug"
#### UI #####

sidebar <- dashboardSidebar(
  sidebarMenu(

    menuItem("Map", tabName = "map", icon = icon("map")),
    menuItem("Boxplot", tabName = "boxplot", icon = icon("chart-simple")),
    menuItem("Material Flow", tabName = "matFlow", icon = icon("chart-simple")),
    menuItem("Data set", tabName = "dataSet", icon = icon("database"))

  )
)

body <- dashboardBody(
  tags$head(tags$style('
   body {
      font-family: Source Sans Pro; 
   }'
  )), 
  tabItems(
    tabItem(tabName = "map",
            # Sidebar layout with input and output definitions ----
            sidebarLayout(
              # Sidebar panel for inputs ----
              sidebarPanel(
                
                textInput("textInputVehicleID", 
                          h3("Vehicle ID:"), 
                          placeholder  = "Enter ID here"),
                actionButton("enterID", "Enter"),
                
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
                                                  "Distribution center to state of Customer" = 4),
                                   selected = c(1,2,3,4)
                ),
                radioButtons("radio", 
                             h3("Vehicle Types:"),
                             choices = list("All" = 0, 
                                            "Single only" = 1),
                             selected = 0),
              ),
              
              # Main panel for displaying outputs ----
              mainPanel(
                # Output: 
                img(src="legend.svg", width = 313, height = 175),
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
                                                  "Distribution center to state of Customer" = 4),
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
            # Sidebar layout with input and output definitions ----
            sidebarLayout(
              sidebarPanel(),
              # Sidebar panel for inputs ----
              # Main panel for displaying outputs ----
              mainPanel(
                # Output: 
                plotOutput("matflow", width = "100%")
              ),
            )
            ),
    
    tabItem(tabName = "dataSet",
            h2("Data set"), 
            dataTableOutput("dynamicDataSet")
    )
  )
)



# Define UI for app that draws a histogram ----
ui <- dashboardPage(
  title = "Case Study Group 11",
  header = dashboardHeader(title = img(src="logo_v2.png", width = '100%')),
  sidebar,
  body
)

#### SERVER FUNCTION #####

server <- function(input, output) {
  
  ### map
  
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
  
  #initiate
  coord<- data.frame(Breitengrad=NA, Längengrad=NA)%>%
    na.omit()
  single_part_location <- coord
  component_location <- coord
  vehicle_location <- coord
  gemeinde_location <- coord
  state_capital_location <- coord
  display_df<-data.frame(ID_Fahrzeug=NA,gesamtDistanz=NA)

  
  #default filter: set dot to Berlin
  filtermap <- ger_shp %>%
    filter(NAME_3 == "Berlin")
  
  getVehicleID <- eventReactive(input$enterID, {
    return(input$textInputVehicleID)
  })
  # Map output
  
  output$map <- renderTmap({
    
    # get ID from user-input
    
    checkA <- input$checkGroupDistanceComparison
    checkB <- input$checkGroupLevelsMap
    checkC <- input$radio
    
    if(checkC == "1"){
      final_data<-final_data%>%
        filter(grepl("11-[[:digit:]]-[[:digit:]]+",ID_Fahrzeug))
    }

    vehicleID <- ""
    distanzen <- final_data%>%
      select(contains("Distanz"))#%>%
    final_data$Distanz_je_komp <- rowSums(distanzen)
    
    final_data<-final_data%>%
      group_by(ID_Fahrzeug)%>%
      mutate(gesamtDistanz=sum(Distanz_je_komp))%>%
      ungroup()
    
    display_df<-final_data%>%
      select("ID_Fahrzeug","gesamtDistanz")
    
    # switch
    inti<-0
    intj<-1
    
    vehicleID <- getVehicleID()
    
    row_of_content <- final_data[0,]
    
    
    if(!is.null(checkA)){
      #row_of_content<-row_of_content[0,]
      if("1" %in% checkA){
        max_dist_vehicle<-display_df[which(display_df$gesamtDistanz == max(display_df$gesamtDistanz)),]%>%
          distinct()
        row_of_content<-row_of_content%>%
          full_join(final_data[which(final_data$ID_Fahrzeug==max_dist_vehicle$ID_Fahrzeug),])
      }
      
      if("2" %in% checkA){
        min_dist_vehicle<-display_df[which(display_df$gesamtDistanz == min(display_df$gesamtDistanz)),]%>%
          distinct()
        row_of_content<-row_of_content%>%
          full_join(final_data[which(final_data$ID_Fahrzeug==min_dist_vehicle$ID_Fahrzeug),])
      }
      
      if("3" %in% checkA){
        med_dist_vehicle<-display_df[which.min(abs(display_df$gesamtDistanz-median(display_df$gesamtDistanz))),]%>%
          distinct()
        row_of_content<-row_of_content%>%
          full_join(final_data[which(final_data$ID_Fahrzeug==med_dist_vehicle$ID_Fahrzeug),])
      }
    }
    
    # if Fahrzeug_ID exists in data
    # get information from data and plot in map
    if (any(final_data$ID_Fahrzeug == vehicleID)) {
    
      # get information rows from data
      row_of_content <- row_of_content%>%
        full_join(final_data[which(final_data$ID_Fahrzeug == vehicleID),])
    }  
      
     # backup_row_of_content <- row_of_content
    if(nrow(row_of_content>0)){

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
      coord_sf <- st_as_sf(coord, coords = c("Längengrad","Breitengrad"), crs=4326)
     
       filtermap <- coord_sf
      

        # switch to 1
        inti <- 1
   }


    
   #default when switch is 0
   if(!inti){
     map_germany <- map_germany  +
       tm_shape(filtermap) + tm_dots(size = 0.000001) 
   }else if(inti){
     #built connection lines from route
     if("1" %in% checkB) {
       teil_zu_komp <- data.frame(c(single_part_location, component_location))
       map_germany <- getFilterLines(teil_zu_komp, map_germany,1) 
       }
     if("2" %in% checkB ){
       komp_zu_fahr <- data.frame(c(component_location, vehicle_location))
       map_germany <- getFilterLines(komp_zu_fahr, map_germany,2)
     }
     if("3" %in% checkB) {
       fahr_zu_stadt <- data.frame(c(vehicle_location, state_capital_location))
       map_germany <- getFilterLines(fahr_zu_stadt, map_germany,3) 
     }
     if("4" %in% checkB ){
       stadt_zu_kunde <- data.frame(c(state_capital_location, gemeinde_location))
       map_germany <- getFilterLines(stadt_zu_kunde, map_germany,4)
     }
     
     single_part_label<-row_of_content%>%
       select(contains("_Einzelteil"))
     names(single_part_label) <- gsub("_Einzelteil","", names(single_part_label))
     
     component_label<-row_of_content%>%
       select(contains("_Komponente"))
     names(component_label) <- gsub("_Komponente","", names(component_label))
     
     vehicle_label<-row_of_content%>%
       select(contains("_Fahrzeug"))
     names(vehicle_label) <- gsub("_Fahrzeug","", names(vehicle_label))
     
     state_capital_label<-row_of_content%>%
       select(contains("Hauptstadt"))
     names(state_capital_label) <- gsub("_Hauptstadt","", names(state_capital_label))
     
     gemeinde_label<-row_of_content%>%
       select(contains("Gemeinde"),"gesamtDistanz")
     names(gemeinde_label) <- gsub("_Gemeinde","", names(gemeinde_label))
     
     e_filt <- st_as_sf(single_part_label, coords = c("Längengrad","Breitengrad"), crs=4326)
     k_filt <- st_as_sf(component_label, coords = c("Längengrad","Breitengrad"), crs=4326)
     f_filt <- st_as_sf(vehicle_label, coords = c("Längengrad","Breitengrad"), crs=4326)
     s_filt <- st_as_sf(state_capital_label, coords = c("Längengrad","Breitengrad"), crs=4326)
     g_filt <- st_as_sf(gemeinde_label, coords = c("Längengrad","Breitengrad"), crs=4326)

     map_germany<-map_germany + 
       tm_shape(e_filt)+tm_dots(col = "red",
                                scale =2,
                                border.col = "black",
                                labels="Single-Part-Supplier",
                                id="ORT",
                                popup.vars = c("Type:" = "Typ", "Serial number:" = "ID","Factory plant:"="Werksnummer"),
                                title = "Legened:")+
       tm_shape(k_filt)+tm_dots(col = "yellow",
                                scale =2,
                                border.col = "black",
                                labels="Component-Supplier",
                                id="ORT",
                                popup.vars = c("Type:" = "Typ", "Serial number:" = "ID","Factory plant:"="Werksnummer","Distance from SPS to CS in km:"="Distanz_Einzelteil_zu_in_km"),
                                title = "Legened:")+
       tm_shape(f_filt)+tm_dots(col = "blue",
                                scale =2,
                                border.col = "black",
                                labels="OEM1 Factory",
                                id="ORT",
                                popup.vars = c( "OEM1 Factory:" = "Werksnummer", "Vehicle ID:" ="ID", "Distance from CS to OEM1 in km:"="Distanz_Komponente_zu_in_km"),
                                title = "Legened:")+
       tm_shape(s_filt)+tm_dots(col = "green",
                                scale =2,
                                border.col = "black",
                                id="Hauptstadt",
                                popup.vars = c("Distance from OEM1 to State Capital in km:"="Distanz_Fahrzeug_zu_in_km"),
                                labels="Distribution Center")+
       tm_shape(g_filt)+tm_dots(col = "orange",
                                scale =2,
                                border.col = "black",
                                id="Gemeinden",
                                popup.vars = c("Distance from State Capital to State of Customer in km:"="Distanz_Hauptstadt_zu_in_km","Distance overall in km:"="gesamtDistanz"),
                                labels="State of Customer")
   }
  })
  
  ## boxplot
  
  output$boxPlot <- renderPlot({
    # get levels to plot from checkboxes
    levels_selected <- sort(as.numeric(input$checkGroupLevelsBoxplot))
    boxplot_from_selected(captions_list, dist_vs_type, levels_selected)
  })
  
  
  output$dynamicDataSet <- renderDataTable(final_data,
    options = list(scrollX = TRUE)
  )
  
  output$matflow<-renderPlot({

    ggplot(final_data,aes(ORT_Komponente, fill = ORT_Fahrzeug))+
     geom_bar(position = position_dodge(width=0.5))+
      ggtitle("Material Flow of Component Parts to the Vehicle Factories")+
      xlab("Location of Component Supplier")+
      ylab("")+theme(legend.position='top', 
                                    legend.justification='left',
                                    legend.direction='horizontal',
                                    legend.title =element_blank())
    
      
  })
}

#### RUN 

shinyApp(ui = ui, server = server)
