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
if (!require(data.table)) {
  install.packages("data.table")
  require(data.table)
}
if (!require(dplyr)) {
  install.packages("dplyr")
  require(dplyr)
}
if (!require(sp)) {
  install.packages("sp")
  require(sp)
}
if (!require(maptools)) {
  install.packages("maptools")
  require(maptools)
}


final_data <- read_csv("Final_Data_Group_11.csv")

## material flow on different levels

# sampling from the final data set for performance
samples <- function(df) {
  df[sample(nrow(df), 10000), ]
}

distance_single_to_component <- final_data %>%
  select("ID_Einzelteil","Distanz_Einzelteil_zu_Komponente_in_km") %>%
  unique() %>%
  select("Distanz_Einzelteil_zu_Komponente_in_km") %>%
  samples()

distance_component_to_OEM <- final_data %>%
  select("ID_Komponente","Distanz_Komponente_zu_Fahrzeug_in_km") %>%
  unique() %>%
  select("Distanz_Komponente_zu_Fahrzeug_in_km") %>%
  samples()

distance_OEM_to_state_capital <- final_data %>%
  select("ID_Fahrzeug","Distanz_Fahrzeug_zu_Hauptstadt_in_km") %>%
  unique() %>%
  select("Distanz_Fahrzeug_zu_Hauptstadt_in_km") %>%
  samples()

distance_OEM_to_distribution <- final_data %>%
  select("ID_Fahrzeug","Distanz_Fahrzeug_zu_Hauptstadt_in_km","Distanz_Hauptstadt_zu_Gemeinde_in_km") %>%
  summarize(ID_Fahrzeug, Distanz_Fahrzeug_zu_Gemeinde_in_km = Distanz_Fahrzeug_zu_Hauptstadt_in_km + Distanz_Hauptstadt_zu_Gemeinde_in_km) %>%
  select("Distanz_Fahrzeug_zu_Gemeinde_in_km") %>%
  samples()

# aggregate in single list for plotting
levels <- c("distance_single_to_component","distance_component_to_OEM","distance_OEM_to_distribution","distance_OEM_to_state_capital")
dist_vs_type <- lapply(levels, get, envir=environment())

# captions
captions_list <- c("1: From Single Part to Component", "2: From Component to OEM", "3: From OEM to Distribution center", "4: From OEM to State Capital")
names(dist_vs_type) <- captions_list
