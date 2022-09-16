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


##########################
### SHINY APP functions###
##########################

# function from Rpubs
points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
  
  # Convert to SpatialPointsDataFrame
  coordinates(data) <- c(long, lat)
  
  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }
  
  # If there is only one path...
  if (is.null(id_field)) {
    
    lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
    
    return(lines)
    
    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  
    
    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])
    
    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
    
    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0("line", as.character(p))
      l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
      sp_lines <- spRbind(sp_lines, l)
    }
    
    return(sp_lines)
  }
}

# Function to built connection lines between points
# Args: df-> data of coordinates; ger_map -> Map 
# Return: ger_map
getFilterLines <- function(df,ger_map,num){
  #built color array; visible colors
  color<-c("red","yellow","blue","green","orange")
  #built connection df-> df2 to get the route
  for (i in 1:nrow(df)){
    a<- df[i,]%>%
      select(contains("Breitengrad"))%>%
      t()
    b<-df[i,]%>%
      select(contains("Längengrad"))%>%
      t()
    df2<-data.frame("Breitengrad"= a, "Längengrad"= b)
    rownames(df2) <- NULL
    colnames(df2)<- c("Breitengrad","Längengrad")
    # built route
    filtLine<-points_to_line(df2, 
                             long="Längengrad",
                             lat = "Breitengrad"
                             
    )
    proj4string(filtLine) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    #add route to map
    ger_map <- ger_map + tm_shape(filtLine)  + tm_lines(col = color[num], scale = 2, lty=2) 
    
  }
  return(ger_map) 
}





# create a boxplot for different levels of material flow
boxplot_from_selected <- function(captions_list, dist_vs_type, levels_selected=1:4) {
  # plot selected levels
  p <-ggplot()
  if (1 %in% levels_selected) {p <- p + geom_boxplot(aes(x=captions_list[1], y=unlist(dist_vs_type[1]), fill=captions_list[1]))}
  if (2 %in% levels_selected) {p <- p + geom_boxplot(aes(x=captions_list[2], y=unlist(dist_vs_type[2]), fill=captions_list[2]))}
  if (3 %in% levels_selected) {p <- p + geom_boxplot(aes(x=captions_list[3], y=unlist(dist_vs_type[3]), fill=captions_list[3]))}
  if (4 %in% levels_selected) {p <- p + geom_boxplot(aes(x=captions_list[4], y=unlist(dist_vs_type[4]), fill=captions_list[4]))}
  p + 
    #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    scale_fill_brewer(palette="Blues") +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=20)
    ) +
    ggtitle("Overview of distance travelled of material over the supply chain") +
    xlab("") +
    ylab("Distance in kilometers")
}



###############################
### Miscellaneous functions ###
###############################

# calculate shortest distances between vectors of Points A and B given by longitude and latitude
calc_distance_in_km <- function(lonA, latA, lonB, latB) {
  as.integer(distHaversine(cbind(lonA,latA),cbind(lonB,latB)))/1000
}
