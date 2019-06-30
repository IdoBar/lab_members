#define a function that will process googles server responses for us.
getGeoDetails <- function(address){   
  require(ggmap)
  # address=addresses$Address[1]
  #use the gecode function to query google servers
  geo_reply <- geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
  # str(geo_reply)
    #now extract the bits that we need from the returned list
  answer <- data.frame(Country=NA, country_code=NA, lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
  answer$status <- geo_reply$status
  #if we are over the query limit - want to pause for an hour
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60)
    geo_reply <- geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    # answer$status <- geo_reply$status
  }
  #return Na's if we didn't get a match:
  if (geo_reply$status != "OK"){
    return(answer)
  }   
  # else, extract what we need from the Google server reply into a dataframe:
  # main challenges are to drill down into the first list element within the results list for each list
  # item, then extract the long_name and short_name from address_components slot, but only if the address
  # component type is "country" (which is nested in the "types" slot within each "address_components") 
  ### purrr approach ###
  answer <- geo_reply %>% map_df(~tibble(
    formatted_address=.$results[[1]]$formatted_address,
    lat=.$results[[1]]$geometry$location$lat,
    long=.$results[[1]]$geometry$location$lng,
    Country=.$results[[1]]$address_components %>% 
      keep(~ "country" %in% map_chr(.$types, c)) %>% flatten() %>% .$long_name,
    country_code=.$results[[1]]$address_components %>% 
      keep(~ "country" %in% map_chr(.$types, c)) %>% flatten() %>% .$short_name,
    address_type <- paste(.$results[[1]]$types, collapse=','),
    status=.$status))
  
  ### base R/combined purrr approach ###
  # country_info <- geo_reply$results[[1]]$address_components %>% 
  #   keep(~ "country" %in% map_chr(.$types, c)) %>% flatten() # %>% .$long_name
  # country_info <- Filter(function(x) "country" %in% map_chr(x$types, c),
  # geo_reply$results[[1]]$address_components) %>% flatten()
  
  ### completely base R approach  ###
  # for (i in 1:length(geo_reply$results[[1]]$address_components)){
  #   # i=1
  #   types <- unlist(geo_reply$results[[1]]$address_components[[i]][["types"]])
  #   if ("country" %in% types) {
  #     country_info <- geo_reply$results[[1]]$address_components[[i]]
  #   }
  # }
  #  
  # answer$Country <- country_info$long_name
  # answer$country_code <- country_info$short_name
  # answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  # answer$long <- geo_reply$results[[1]]$geometry$location$lng   
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  # answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  # answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  return(answer)
}

getAspectRatioSVG <- function(img_file){
  require(xml2)
  # img_file=rect_flags$flag_file[3]
  img_info <- read_xml(img_file) 
  aspect_ratio <- as.numeric(xml_attr(img_info, "width"))/as.numeric(xml_attr(img_info, "height"))
  if (length(aspect_ratio)>0 & is.numeric(aspect_ratio) & aspect_ratio>0) return(aspect_ratio)
  message(sprintf("File %s is not a valid SVG file or does not contain dimensions", img_file))
}