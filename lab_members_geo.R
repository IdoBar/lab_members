#### resources  ####
# https://stackoverflow.com/questions/31668163/geographic-geospatial-distance-between-2-lists-of-lat-lon-points-coordinates
# https://www.shanelynn.ie/massive-geocoding-with-r-and-google-maps/
pacman::p_load(tidyverse, ggmap, geosphere, leaflet, maps)
# See .Renviron for Google API key


addresses <- readxl::read_excel("data/lab_members_addresses.xlsx")
# Get geodetails
geo_df <- addresses$Address %>% map_dfr(~getGeoDetails(.x)) %>% 
  bind_cols(addresses, .) %>% 
  write_csv("data/lab_members_geodata.csv")

# plot map
leaflet(geo_df) %>% addProviderTiles("Esri.WorldImagery") %>% # Esri.WorldTopoMap OpenTopoMap Stamen.Terrain Esri.NatGeoWorldMap GeoportailFrance.orthos (requires API)
  addProviderTiles("CartoDB.PositronOnlyLabels") %>% #  CartoDB.VoyagerOnlyLabels
  addMarkers(popup = ~Name, label = ~Name, 
             icon = ~icons(iconUrl = "img/red_marker.png",
               iconWidth = 15, iconHeight = 30))
# create distance matrix
dist_mat <- distm(geo_df[c('lat','long')], geo_df[c('lat','long')], 
             fun=distVincentyEllipsoid)
?addProviderTiles
