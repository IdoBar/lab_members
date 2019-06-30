#### resources  ####
# https://stackoverflow.com/questions/31668163/geographic-geospatial-distance-between-2-lists-of-lat-lon-points-coordinates
# https://www.shanelynn.ie/massive-geocoding-with-r-and-google-maps/
pacman::p_load(maps, tidyverse, ggmap, glue, geosphere, pheatmap,
               leaflet, leafpm, paletteer) # countrycode, imager
# See .Renviron for Google API key
source("src/getGeo.R")

addresses <- readxl::read_excel("data/lab_members_addresses.xlsx") %>% 
  # mutate(Country = sub(".+,[ ]*(.+)$", "\\1", Address), 
  #        country_code=countrycode(Country, 'country.name', 
  #                                         destination = "iso2c"),
         mutate(first_name=sub(" .+", "", Name))
# test <- addresses %>% mutate_geocode(Address, output="more")

# geodetails <- geocode(addresses$Address, output = "all", messaging=TRUE)
# names(geodetails) <- addresses$Address

# geodetails %>% keep(~ "university" %in% flatten_chr(.$results[[1]]$types)) %>% 
#   iwalk(~message(glue("{.y} is a university")))


# try to extract the information using tidyr
test <- tibble(query=addresses$Address, results=map(geodetails, "results"), status=map_chr(geodetails, "status")) %>% mutate(formatted_address=pluck(geodetails, "formatted_address"))
test %>% purrr::pluck("results") %>% map_chr(.x= flatten(.), "formatted_address")
# extract geocoding information
update_addresses <- TRUE
if (isTRUE(update_addresses)){
  
  # Get geodetails
  geodetails <- geocode(addresses$Address, output = "all", messaging=TRUE)
  names(geodetails) <- addresses$Address
  # notif if some addresses couldn't be found
  geodetails %>% discard(~ .$status=="OK") %>% iwalk(~message(glue("Could not find details for {.y}")))
  # svae just successful queries 
  geo_ok <- geodetails %>% keep(~ .$status=="OK")
  # using a mixture of purrr() and base R list verbs
  # main challenges are to drill down into the first list element within the results list for each list item, then extract the long_name and short_name from address_components slot, but only if the address component type is "country" (which is nested in the "types" slot within each "address_components") 
  results <- geo_ok %>% map_df(~tibble(
    formatted_address=.$results[[1]]$formatted_address,
    lat=.$results[[1]]$geometry$location$lat,
    long=.$results[[1]]$geometry$location$lng,
    Country=.$results[[1]]$address_components %>% 
      keep(~ "country" %in% flatten_chr(.$types)) %>% flatten() %>% .$long_name,
    country_code=.$results[[1]]$address_components %>% 
      keep(~ "country" %in% flatten_chr(.$types)) %>% flatten() %>% .$short_name,
    status=.$status)) %>% mutate(query=names(geo_ok))
  
  
  
  geo_df <- results %>% 
  # geo_df <- addresses$Address %>% map_dfr(~getGeoDetails(.x)) %>% 
    left_join(addresses, ., by=c("Address"="query")) %>% 
    write_csv("data/lab_members_geodata.csv")
} else geo_df <- read_csv("data/lab_members_geodata.csv")

# Get flags information
# download flags
update_flags <- TRUE
walk(geo_df$country_code, ~{
  if (!file.exists(glue("img/flags/{.x}.svg")) | isTRUE(update_flags)) {
    download.file(glue('https://raw.githubusercontent.com/google/region-flags/gh-pages/svg/{.x}.svg'), 
                       glue("img/flags/{.x}.svg"))
  } else {
    message(glue("File img/flags/{.x}.svg exists"))
    }
})

# 
# 
# if_else(!file.exists(glue("img/flags/{.x}.svg")), download.file(glue('https://raw.githubusercontent.com/google/region-flags/gh-pages/svg/{.x}.svg'), glue("img/flags/{.x}.svg")), 
#                                      ))

base_height=15
# rect_flags <- htmltab::htmltab("https://en.wikipedia.org/wiki/List_of_aspect_ratios_of_national_flags") %>% as_tibble() %>%
#   setNames(., c("Country", "Ratio")) %>% 
#   filter(grepl(paste(geo_df$Country, collapse = "|"), Country, ignore.case = TRUE)) %>% 
#   # filter(!grep(paste(geo_df$Country, collapse = "\\)|\\("), Country, ignore.case = TRUE))
#   mutate(country_code=countrycode(Country, 'country.name', destination = "iso2c")) %>% 
rect_flags <-  geo_df %>% select(country_code) %>%
  mutate(flag_file=glue("./img/flags/{country_code}.svg"),
         Ratio=map_dbl(flag_file, ~getAspectRatioSVG(.x)),
         flag_height=base_height, flag_width=base_height*Ratio,
         )
# Use round emoji flags as markers
round_flags <- htmltab::htmltab("https://apps.timwhitlock.info/emoji/tables/iso3166") %>% as_tibble() %>% 
  mutate(flag_file=tolower(sub(" ", "-", 
                              gsub("U+", "", Unicode, fixed = TRUE)))) %>%
  select(Country=Name, country_code=ISO, flag_file) %>% 
  inner_join(geo_df %>% select(country_code), .) 

walk2(unique(round_flags$flag_file), unique(round_flags$country_code), ~{
  if (!file.exists(glue("img/emojis/{.y}.svg")) | isTRUE(update_flags)) {
    download.file(glue('https://raw.githubusercontent.com/eosrei/emojione-color-font/master/assets/emojione-svg/{.x}.svg'), glue("img/emojis/{.y}.svg"))
  } else {
    message(glue("File img/emojis/{.y}.svg exists"))
  }
      })
# https://raw.githubusercontent.com/hjnilsson/country-flags/master/svg/{country_code}.svg

# flags <- geo_df$country_code %>%
#   map_chr(~dir("../country-flags/png250px/", sprintf("^%s.png$", .x),
#                full.names = TRUE ))

flagIcons <-  rect_flags[c("flag_file", "flag_width", "flag_height")] %>%
  pmap(~makeIcon(..1, ..1,  iconWidth = ..2, iconHeight = ..3,
                 iconAnchorX = 0, iconAnchorY = 0)) %>%
  setNames(., rect_flags$country_code)
# make it an iconList
attr(flagIcons, "class") <- "leaflet_icon_set"

# 
# flagIconList <- iconList(bd = flagIcons[[1]])  
# #   
emojiIcons <- icons(glue::glue("img/emojis/{round_flags$country_code}.svg"),
iconHeight = base_height, iconWidth = base_height,
iconAnchorX = 0, iconAnchorY = 0)
# plot map
leaflet(geo_df) %>% addProviderTiles("Esri.WorldStreetMap") %>% # Esri.WorldTopoMap OpenTopoMap Stamen.Terrain Esri.NatGeoWorldMap GeoportailFrance.orthos (requires API) Esri.WorldStreetMap
  # addProviderTiles("CartoDB.PositronOnlyLabels") %>% #  CartoDB.VoyagerOnlyLabels
  addMarkers(popup = ~Name, label = ~Name, 
             icon = emojiIcons) %>% # ~flagIcons[country_code]
  addScaleBar(position = "bottomright",
              options = scaleBarOptions(imperial = FALSE)) %>% 
  addMeasure(primaryLengthUnit = "kilometers")

# create distance matrix
dist_mat <- distm(geo_df[c('long','lat')], geo_df[c('long','lat')], 
             fun=distVincentyEllipsoid)/1000
dimnames(dist_mat) <- list(geo_df$first_name, geo_df$first_name)
dist_mat[dist_mat==0] <- NA
pheatmap(dist_mat, cutree_rows = 2, 
         color = paletteer_c(viridis, inferno, 30, direction = -1),
         cutree_cols = 2)
dimnames(dist_mat[which.min(dist_mat)])
long_dist <- as.data.frame(dist_mat) %>% rownames_to_column("Name") %>% 
  gather("Colleague", "Distance", 2:ncol(.)) 
# long_dist %>%  filter(!is.na(Distance)) %>% group_by(Name) %>% summarise(Closest=min(Distance)) 
closest <- long_dist %>%  filter(!is.na(Distance)) %>% group_by(Name) %>% 
  summarise(min_dist=min(Distance)) %>% 
  arrange(desc(min_dist)) # `Distance to closest colleague`

summary <- long_dist %>%  filter(!is.na(Distance)) %>% group_by(Name) %>% 
  summarise(max_dist=max(Distance)) %>%  # `Distance to furthest colleague`
  arrange(desc(max_dist)) %>% inner_join(closest, .) %>% 
  setNames(., c("Name", "Distance to closest location", "Distance to furthest location"))

