# Load required packages
library("sf")
library("rnaturalearth")
library("leaflet")
library("dplyr")
library("mapview")


# create subdirectory to store figure outputs
dir.create("Baxter et al. - Map Figure/FiguresResults")

######################################################################
############################## Figure 1 ##############################
######################################################################

# download data for boxes
Box1Data <- read.csv("Baxter et al. - Map Figure/Data/BoxOneData.csv")
Box2Data <- read.csv("Baxter et al. - Map Figure/Data/BoxTwoData.csv")
Box3Data <- read.csv("Baxter et al. - Map Figure/Data/BoxThreeData.csv")
Box4Data <- read.csv("Baxter et al. - Map Figure/Data/BoxFourData.csv")


# define function for drawing bounding box
find_bbox <- function(map_data) {
  
  # Remove rows where a specific column has NA values
  map_data <- map_data %>%
    filter(!is.na(lat) & !is.na(long))
 
  # check if coordinates are the same
  all_same <- all(map_data$long == map_data$long[1])
  
  if (all_same) {
    xmin = mean(map_data$long) - 0.3
    xmax = mean(map_data$long) + 0.3
    ymin = mean(map_data$lat) - 0.3
    ymax = mean(map_data$lat) + 0.3
    
  } else {
    # find original bounding box
    coordinates = st_as_sf(map_data, coords = c("long", "lat"), crs = 4326)
    bbox = st_bbox(coordinates)
    
    # set margin to 0.1 and find original height and width
    margin = 0.1
    width = bbox$xmax - bbox$xmin
    height = bbox$ymax - bbox$ymin
    
    if (width > height){
      # adjust coordinates to accommodate margin
      xmin = bbox$xmin - width * margin
      xmax = bbox$xmax + width * margin
      ymin = mean(map_data$lat) - width/2 - width*margin
      ymax = mean(map_data$lat) + width/2 + width*margin
    
      } else if (height > width){
      # adjust coordinates to accommodate margin
      xmin = mean(map_data$long) - height/2 - height*margin
      xmax = mean(map_data$long) + height/2 + height*margin
      ymin = bbox$ymin - height * margin
      ymax = bbox$ymax + height * margin
    }

  }
  
  # create new bounding box and convert so it can be recognized by ggplot2
  bbox_coords = matrix(c(xmin, ymin,  # lower-left
                         xmax, ymin,  # lower-right
                         xmax, ymax,  # upper-right
                         xmin, ymax,  # upper-left
                         xmin, ymin), # close the polygon
                       ncol = 2, byrow = TRUE)
  bbox_polygon = st_polygon(list(bbox_coords))
  bbox_sf = st_sfc(bbox_polygon, crs = 4326)
  expanded_bbox <- st_bbox(bbox_sf)
  
  return(expanded_bbox)}

# define function for drawing each box
smallBox <- function(mapdata_name) {
  
  # find data for named mapdata
  mapdata <- get(mapdata_name)
  
  # Remove rows where a specific column has NA values
  mapdata <- mapdata %>%
    filter(!is.na(lat) & !is.na(long))

  # call function to find bbox and further modify it so it can be placed on map
  bbox <- find_bbox(mapdata)
  bbox_sfc <- st_as_sfc(bbox)
  
  # Convert individual sites to sf points
  points_sf <- st_as_sf(mapdata, coords = c("long", "lat"), crs = 4326)
  
  # Create a leaflet map with bounding box and points
  map <- leaflet() %>%
    addProviderTiles(providers$OpenTopoMap) %>%
    addPolygons(data = bbox_sfc, color = "black", fill = FALSE, weight = 10, opacity = 1) %>% 
    addCircleMarkers(data = points_sf, 
                     color = "black", 
                     fill = TRUE, 
                     fillColor = "black", 
                     radius = 20, 
                     weight = 2, 
                     fillOpacity = 1)
  
  # Save as PNG with higher resolution
  mapshot(map, file = paste0("Baxter et al. - Map Figure/FiguresResults/", mapdata_name, ".png"), vwidth = 2000, vheight = 1500)
}

# call function to draw boxes and plot sites in boxes
smallBox(mapdata_name = "Box1Data")
smallBox(mapdata_name = "Box2Data")
smallBox(mapdata_name = "Box3Data")
smallBox(mapdata_name = "Box4Data")

# create function to draw state map with boxes on it

# ensure any overlays are properly saved
mapviewOptions(basemaps = "OpenStreetMap",
               fgb = FALSE) 

# Get US states and filter for Nevada
us_states <- ne_states(country = "United States of America", returnclass = "sf")
nevada <- us_states[us_states$name == "Nevada", ]

# call function to find bbox and further modify it so it can be placed on map
bbox1 <- st_as_sfc(find_bbox(Box1Data))
bbox2 <- st_as_sfc(find_bbox(Box2Data))
bbox3 <- st_as_sfc(find_bbox(Box3Data))
bbox4 <- st_as_sfc(find_bbox(Box4Data))

# Map Nevada
mapNevada <- mapview(nevada, color = "black", fill = FALSE, lwd = 20) +
                mapview(bbox1, color = "black", fill = FALSE, alpha = 1, lwd = 15) +
                mapview(bbox2, color = "black", fill = FALSE, alpha = 1, lwd = 15) +
                mapview(bbox3, color = "black", fill = FALSE, alpha = 1, lwd = 15) +
                mapview(bbox4, color = "black", fill = FALSE, alpha = 1, lwd = 15)

mapNevadaZoom <- function(latitude) {
  
  # Extract the leaflet map and apply fitBounds
  mapNevadaAdjusted <- mapNevada@map %>%
    setView(lng = -115.5, lat = latitude, zoom = 9)  
  
  # Save as PNG with higher resolution
  mapshot(mapNevadaAdjusted, file = paste0("Baxter et al. - Map Figure/FiguresResults/NevadaMap", latitude, ".png"), vwidth = 3740, vheight = 3740)
}

mapNevadaZoom(latitude = 39)
mapNevadaZoom(latitude = 37)

######################################################################
######################################################################
######################################################################