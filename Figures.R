# Load required packages
library(maps)
library(mapdata)
library(sf)
library(leaflet)

# Get Nevada county map data
nevada_map <- map("county", fill = TRUE, plot = FALSE)
nevada_sf <- st_as_sf(nevada_map)

# Filter for Nevada counties
nevada_sf <- nevada_sf[grepl("nevada,", nevada_sf$ID), ]

# Reproject to WGS84 CRS (if not already in it)
nevada_sf <- st_transform(nevada_sf, crs = 4326)  # 4326 is the code for WGS84

# Create a Leaflet map
leaflet(data = nevada_sf) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addWMSTiles(
    "https://your-geology-wms-url",
    layers = "terrane_layer",
    options = WMSTileOptions(format = "image/png", transparent = TRUE)
  ) %>%
  addPolygons(fillColor = "lightgray",
              color = "black",
              weight = 1,
              opacity = 1,
              fillOpacity = 0.5) %>%
  addLegend(position = "bottomright", 
            colors = "lightgray", 
            labels = "Nevada Counties",
            title = "Legend")
