# https://rstudio.github.io/leaflet/
# 
# Interactive panning/zooming
# Compose maps using arbitrary combinations of:
# Map tiles
# Markers
# Polygons
# Lines
# Popups
# GeoJSON
# Create maps right from the R console or RStudio
# Embed maps in knitr/R Markdown documents and Shiny apps
# Easily render spatial objects from the sp or sf packages, or data frames with latitude/longitude columns
# Use map bounds and mouse events to drive Shiny logic
# Display maps in non spherical mercator projections
# Augment map features using chosen plugins from leaflet plugins repository

#==============================================================================
# Required Packages
#==============================================================================
library(leaflet)
library(maps)
library(rgdal)

#==============================================================================
# Map Basics
#==============================================================================

m <- leaflet()
m <- addTiles(m)
m <- addMarkers(m, lng = 174.768, lat = -36.852, popup = "The birthplace of R")
m

mapStates = map("state", fill = TRUE, plot = FALSE)
leaflet(data = mapStates) %>% addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)
  
m = leaflet() %>% addTiles()
df = data.frame(
  lat = rnorm(100),
  lng = rnorm(100),
  size = runif(100, 5, 20),
  color = sample(colors(), 100)
)
m = leaflet(df) %>% addTiles()
m %>% addCircleMarkers(radius = ~size, color = ~color, fill = FALSE)
m %>% addCircleMarkers(radius = runif(100, 4, 10), color = c('red'))

#==============================================================================
# Coloring
#==============================================================================

#------------------------------------------------------------------------------
# Coloring continuous data
#------------------------------------------------------------------------------

# From http://data.okfn.org/data/datasets/geo-boundaries-world-110m
countries <- readOGR("json/countries.geojson", "OGRGeoJSON")
map <- leaflet(countries)

par(mar = c(5,5,0,0), cex = 0.8)
hist(countries$gdp_md_est, breaks = 20, main = "")

# Create a continuous palette function
pal <- colorNumeric(
  palette = "Blues",
  domain = countries$gdp_md_est)
  
# Apply the function to provide RGB colors to addPolygons
map %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
    color = ~pal(gdp_md_est))

binpal <- colorBin("Blues", countries$gdp_md_est, 6, pretty = FALSE)

map %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
    color = ~binpal(gdp_md_est))
	
qpal <- colorQuantile("Blues", countries$gdp_md_est, n = 7)
map %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
    color = ~qpal(gdp_md_est))
	
#------------------------------------------------------------------------------
# Coloring categorical data
#------------------------------------------------------------------------------

# Make up some random levels. (TODO: Better example)
countries$category <- factor(sample.int(5L, nrow(countries), TRUE))

factpal <- colorFactor(topo.colors(5), countries$category)

leaflet(countries) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
    color = ~factpal(category))
	
#==============================================================================
# Legends
#==============================================================================

# From http://data.okfn.org/data/datasets/geo-boundaries-world-110m
countries <- readOGR("json/countries.geojson", "OGRGeoJSON")
## OGR data source with driver: GeoJSON 
## Source: "json/countries.geojson", layer: "OGRGeoJSON"
## with 177 features
## It has 2 fields
map <- leaflet(countries) %>% addTiles()
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = countries$gdp_md_est
)
map %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
    color = ~pal(gdp_md_est)
  ) %>%
  addLegend("bottomright", pal = pal, values = ~gdp_md_est,
    title = "Est. GDP (2010)",
    labFormat = labelFormat(prefix = "$"),
    opacity = 1
  )
  
qpal <- colorQuantile("RdYlBu", countries$gdp_md_est, n = 5)
map %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
    color = ~qpal(gdp_md_est)
  ) %>%
  addLegend(pal = qpal, values = ~gdp_md_est, opacity = 1)	