# Load required packages
library(maps)
library(mapdata)
library(sf)
library(leaflet)
library(ggplot2)

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

# Reproject both layers to WGS84
nevada_sf <- st_transform(nevada_sf, crs = 4326)
surrounding_states_sf <- st_transform(surrounding_states_sf, crs = 4326)

# Define zoom limits (focus on Nevada)
nevada_bbox <- st_bbox(nevada_sf)
xlim <- c(nevada_bbox["xmin"] - 0.5, nevada_bbox["xmax"] + 0.5)  # Add small padding
ylim <- c(nevada_bbox["ymin"] - 0.5, nevada_bbox["ymax"] + 0.5)

# Create the ggplot map
ggplot() +
  # Plot surrounding states in gray
  geom_sf(data = surrounding_states_sf, fill = "gray20", color = "black", size = 0.4, alpha = 0.6) +
  # Plot Nevada counties (default)
  geom_sf(data = nevada_sf[nevada_sf$highlight == "normal", ], fill = "gray80", color = "black", size = 0.6) +
  # Plot highlighted counties in red
  geom_sf(data = nevada_sf[nevada_sf$highlight == "highlight", ], fill = "white", color = "black", size = 0.8) +
  # Set zoomed-in limits
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  # Set theme and labels
  theme_minimal() +
  labs(title = "Study Region in Nevada, USA",
       subtitle = "Highlighted Counties: Elko, Eureka, Nye, White Pine") +
  theme(legend.position = "none")

######################################################################
######################################################################
######################################################################




# Get US state boundaries
states_map <- map("state", fill = TRUE, plot = FALSE)
states_sf <- st_as_sf(states_map)

# Get Nevada county boundaries
counties_map <- map("county", fill = TRUE, plot = FALSE)
counties_sf <- st_as_sf(counties_map)

# Filter for Nevada counties
nevada_sf <- counties_sf[grepl("nevada,", counties_sf$ID), ]

# Define the counties to highlight
highlighted_counties <- c("elko", "eureka", "nye", "white pine")

# Create a column to differentiate highlighted counties
nevada_sf$highlight <- ifelse(sub(",.*", "", sub("nevada,", "", nevada_sf$ID)) %in% highlighted_counties, "highlight", "normal")

# Get surrounding states
surrounding_states <- c("california", "oregon", "idaho", "utah", "arizona", "nevada")
surrounding_states_sf <- states_sf[states_sf$ID %in% surrounding_states, ]

# Reproject to WGS84 for Leaflet
nevada_sf <- st_transform(nevada_sf, crs = 4326)
surrounding_states_sf <- st_transform(surrounding_states_sf, crs = 4326)

# Create a Leaflet map
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%  # Basemap
  addWMSTiles(
    "https://your-geology-wms-url",
    layers = "terrane_layer",
    options = WMSTileOptions(format = "image/png", transparent = TRUE)
  ) %>%
  # Add surrounding states with grey color
  addPolygons(data = surrounding_states_sf,
              fillColor = "gray80",
              color = "black",
              weight = 1,
              opacity = 1,
              fillOpacity = 0.4) %>%
  # Add Nevada counties (default)
  addPolygons(data = nevada_sf[nevada_sf$highlight == "normal", ],
              fillColor = "lightblue",
              color = "black",
              weight = 1.5,
              opacity = 1,
              fillOpacity = 0.6) %>%
  # Add highlighted counties
  addPolygons(data = nevada_sf[nevada_sf$highlight == "highlight", ],
              fillColor = "red",
              color = "black",
              weight = 2,
              opacity = 1,
              fillOpacity = 0.8) %>%
  addLegend(position = "bottomright", 
            colors = c("gray80", "lightblue", "red"), 
            labels = c("Surrounding States", "Nevada Counties", "Highlighted Counties"), 
            title = "Legend")
