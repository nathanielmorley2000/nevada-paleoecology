# Load required packages
library(maps)
library(mapdata)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


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
    panel.grid.major = element_line(color = "gray70", size = 0.5),
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
