# Load required packages
library(maps)
library(mapdata)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(leaflet)
library(webshot2)
library(dplyr)
library(mapview)


# create subdirectory to store figure outputs
dir.create("FiguresResults")

######################################################################
############################## FIGURE 1 ############################## 
######################################################################

# Get US state boundaries
states_map <- map("state", fill = TRUE, plot = FALSE)
states_sf <- st_as_sf(states_map)

# Get US county boundaries
counties_map <- map("county", fill = TRUE, plot = FALSE)
counties_sf <- st_as_sf(counties_map)

# Filter for Nevada counties
nevada_sf <- counties_sf[grepl("nevada,", counties_sf$ID), ]

# Define highlighted counties
highlighted_counties <- c("elko", "eureka", "nye", "white pine")

# Create a column for highlighting
nevada_sf$highlight <- ifelse(sub(",.*", "", sub("nevada,", "", nevada_sf$ID)) %in% highlighted_counties, 
                              "highlight", "normal")

# Get surrounding states
surrounding_states <- c("california", "oregon", "idaho", "utah", "arizona")
surrounding_states_sf <- states_sf[states_sf$ID %in% surrounding_states, ]

# Get major lakes and rivers
lakes_sf <- ne_download(scale = "large", type = "lakes", category = "physical", returnclass = "sf")
rivers_sf <- ne_download(scale = "large", type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")

# Fix invalid geometries
lakes_sf <- st_make_valid(lakes_sf) %>% st_simplify(dTolerance = 0.01)
rivers_sf <- st_make_valid(rivers_sf) %>% st_simplify(dTolerance = 0.01)

# Reproject everything to WGS84
nevada_sf <- st_transform(nevada_sf, crs = 4326)
surrounding_states_sf <- st_transform(surrounding_states_sf, crs = 4326)
lakes_sf <- st_transform(lakes_sf, crs = 4326)
rivers_sf <- st_transform(rivers_sf, crs = 4326)

# Define zoom limits (focus on Nevada)
nevada_bbox <- st_bbox(nevada_sf)
xlim <- c(nevada_bbox["xmin"] - 1, nevada_bbox["xmax"] + 1)  # Add small padding
ylim <- c(nevada_bbox["ymin"] - 1, nevada_bbox["ymax"] + 1)

# Filter lakes and rivers for those within Nevada’s bounding box
lakes_nevada <- st_crop(lakes_sf, nevada_bbox)
rivers_nevada <- st_crop(rivers_sf, nevada_bbox)

# Set manually chosen longitudes (every other line)
longitude_lines <- seq(-120, -110, by = 2)  # Adjust range as needed
latitude_lines <- seq(35, 43, by = 2)  # Adjust range as needed

# Create the ggplot map
fig1 <- ggplot() +
  # Plot surrounding states in gray
  geom_sf(data = surrounding_states_sf, fill = "gray20", color = "black", size = 0.4, alpha = 0.6) +
  # Plot Nevada counties (default)
  geom_sf(data = nevada_sf[nevada_sf$highlight == "normal", ], fill = "gray80", color = "black", size = 0.6) +
  # Plot highlighted counties in red
  geom_sf(data = nevada_sf[nevada_sf$highlight == "highlight", ], fill = "white", color = "black", size = 0.8) +
  # Plot lakes in blue
  geom_sf(data = lakes_nevada, fill = "blue4", color = "blue4", alpha = 0.6) +
  # Plot rivers in blue
  geom_sf(data = rivers_nevada, color = "blue4", size = 0.8, alpha = 0.7) +
  # Set zoomed-in limits and custom graticules
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE, 
           crs = 4326, 
           default = TRUE) +
  # Customize grid lines (only every other longitude)
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray90", size = 0.5),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(breaks = longitude_lines) +
  scale_y_continuous(breaks = latitude_lines) +
  labs(title = "Nevada Counties, Lakes, and Rivers",
       subtitle = "Highlighted: Elko, Eureka, Nye, White Pine")

# save to new FiguresResults folder
ggsave("FiguresResults/Fig1.png", plot = fig1, bg = "white")

######################################################################
######################################################################
######################################################################




# ensure any overlays are properly saved
mapviewOptions(fgb = FALSE) 

# download data for boxes
Box1Data <- read.csv("Data/BoxOneData.csv")
Box2Data <- read.csv("Data/BoxTwoData.csv")
Box3Data <- read.csv("Data/BoxThreeData.csv")
Box4Data <- read.csv("Data/BoxFourData.csv")


# define function for drawing bounding box
find_bbox <- function(map_data) {
  
  # Remove rows where a specific column has NA values
  map_data <- map_data %>%
    filter(!is.na(lat) & !is.na(long))
 
  # check if coordinates are the same
  all_same <- all(map_data$long == map_data$long[1])
  
  if (all_same) {
    xmin = mean(map_data$long) - 0.5
    xmax = mean(map_data$long) + 0.5
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
    
    # adjust coordinates to accommodate margin
    xmin = bbox$xmin - width * margin
    xmax = bbox$xmax + width * margin
    ymin = bbox$ymin - height * margin
    ymax = bbox$ymax + height * margin
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
    addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png") %>%
    addPolygons(data = bbox_sfc, color = "black", fill = FALSE, weight = 10, opacity = 1) %>% 
    addCircleMarkers(data = points_sf, 
                     color = "black", 
                     fill = TRUE, 
                     fillColor = "black", 
                     radius = 20, 
                     weight = 2, 
                     fillOpacity = 1)
  
  # Save as PNG with higher resolution
  mapshot(map, file = paste0("FiguresResults/", mapdata_name, ".png"), vwidth = 2000, vheight = 1500)
}

# call function to draw boxes and plot sites in boxes
smallBox(mapdata_name = "Box1Data")
smallBox(mapdata_name = "Box2Data")
smallBox(mapdata_name = "Box3Data")
smallBox(mapdata_name = "Box4Data")

# create function to draw state map with boxes on it

# ensure any overlays are properly saved
mapviewOptions(fgb = FALSE) 

# Get US states and filter for Nevada
us_states <- ne_states(country = "United States of America", returnclass = "sf")
nevada <- us_states[us_states$name == "Nevada", ]

# call function to find bbox and further modify it so it can be placed on map
bbox1 <- st_as_sfc(find_bbox(Box1Data))
bbox2 <- st_as_sfc(find_bbox(Box2Data))
bbox3 <- st_as_sfc(find_bbox(Box3Data))
bbox4 <- st_as_sfc(find_bbox(Box4Data))

# Map Nevada
mapNevada <- mapview(nevada, color = "black", fill = FALSE, lwd = 5) +
                mapview(bbox1, color = "black", fill = FALSE, alpha = 1, lwd = 3) +
                mapview(bbox2, color = "black", fill = FALSE, alpha = 1, lwd = 3) +
                mapview(bbox3, color = "black", fill = FALSE, alpha = 1, lwd = 3) +
                mapview(bbox4, color = "black", fill = FALSE, alpha = 1, lwd = 3)

# Save as PNG with higher resolution
mapshot(mapNevada, file = "FiguresResults/NevadaMap.png", vwidth = 2000, vheight = 1500)
