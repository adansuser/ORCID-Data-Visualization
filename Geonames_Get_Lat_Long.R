## This will geocode location information into latitude and longitude and append it to the .csv.
## The Shiny application will require these latitude and longitude columns for mapping functionalities, 
## as Tableau previously generated them automatically.
## Please note that a GeoNames account will be necessary to run this script.

# Install and load packages -----------------------------------------------

# the following packages are required for this script, but they are already installed and loaded in Rorcid_Crossref_Authors.R
# you do not need to uncomment these lines unless you are running this script independently or for the first time.

# install.packages(purr)
# install.packages("data.table")
# devtools::install_github("ropensci/geonames")

# library(data.table)
# library(geonames)
# library(purrr)

# Set the institution's location information --------------------------------

# the following setup for institution location and GeoNames username is also included in Rorcid_Crossref_Authors.R.
# uncomment and update these values only if running this script independently.

# home_city <- "ENTER ORGANIZATION CITY HERE"
# home_country <- "ENTER HOME COUNTRY HERE (e.g. CA)"

# options(geonamesUsername = "PASTE GEONAMES USERNAME HERE")

# Update co_auth_full_info ------------------------------------------------

# get unique city / region / country values from co_auth_full_info
places <- unique(co_auth_full_info[c("city2", "region2", "country2")])
places <- places[!apply(is.na(places) | places == "", 1, all),]
places$uniqueId <- with(places, paste(city2,region2,country2))
places$lat <- ''
places$lng <- ''

for (my_row in 1:nrow(places)) {
  if (!is.na(places[my_row,'region2']) && places[my_row,'region2']!="" && !is.null(places[my_row,'region2']) && 
      places[my_row,'country2']=="US" && str_detect(places[my_row,'region2'],"^[A-Z]{2}$") ) {
    lanc_df <- GNsearch(name_equals = places[my_row,'city2'] , country = places[my_row,'country2'], adminCode1 = places[my_row,'region2'])    
  } else {
    lanc_df <- GNsearch(name_equals = places[my_row,'city2'] , country = places[my_row,'country2'])    
  }
  
  lanc_coords <- lanc_df[1, c("lng", "lat")]  
  
  if (!is.na(lanc_coords$lat) && lanc_coords$lat!="" && !is.null(lanc_coords$lat) && 
      !is.na(lanc_coords$lng) && lanc_coords$lng!="" && !is.null(lanc_coords$lng) ) {
    places[my_row,'lat'] <- lanc_coords$lat
    places[my_row,'lng'] <- lanc_coords$lng
  } else {
    places[my_row,'lat'] <- NA
    places[my_row,'lng'] <- NA
  }
}

# An error will ensue beyong 1000 items:
# Error in getJson(name, params) : error code 19 from server: the hourly limit of 1000 credits for lyr_orcid has been exceeded. 
# Please throttle your requests or use the commercial service.
# To get around this the places statements above could be run for 1000 items at a time, spaced an hour apart
# the places dataframe needs to be broken up into smaller dataframs of <=1000 rows

# you can write the file to json if you want to work with it outside of R
# write_json(places, "./data/places.json")

# here is how you would read it back in, if necessary
# places <- read_json("./data/places.json", simplifyVector = TRUE)

# call geonames one more time to get the lat long of the home city
home_lanc_df <- GNsearch(name_equals = home_city, country = home_country)
home_lanc_coords <- home_lanc_df[1, c("lng", "lat")]  

# back up my dataframe
co_auth_full_info_bck<-co_auth_full_info 

# need to paste these lat longs back into co_auth_full_info
# create a column for the unique place id on each co_auth row
co_auth_full_info$unique_place_Id <- with(co_auth_full_info, paste(city2,region2,country2))

# insert home auth's latitude and longitude -- right now derived from one set home location
co_auth_full_info<-add_col(co_auth_full_info, lat1 = home_lanc_coords$lat, .after = "country1")
co_auth_full_info<-add_col(co_auth_full_info, lng1 = home_lanc_coords$lng, .after = "lat1")

# create columns for the co-auth lat lngs
co_auth_full_info$lat2 <- NA
co_auth_full_info$lng2 <- NA

# left join on unique place ID to get the lat and lng of the co-auth filled in where possible
# be double sure that we have no exact duplicate place rows
places <- places[!duplicated(places$uniqueid),]

# for the co-auth that had location data and for whom we now have a lat lng, join that info into the co_auth df
co_auth_full_info_latlng <- left_join(co_auth_full_info,places,by=c("unique_place_id" = "uniqueid"))

# IF lat2, lng2 are NA, fill from the joined table
# fill in the joined location fields where location is blank
co_auth_full_info_latlng <- co_auth_full_info_latlng %>% 
  mutate(lat2 = coalesce(lat2,lat),
         lng2 = coalesce(lng2,lng)
  )

# get rid of NA values
co_auth_full_info_latlng[is.na(co_auth_full_info_latlng)] <- ""

# remove some columns that are not needed
co_auth_full_info_latlng <- subset(co_auth_full_info_latlng, select = -c(city2.y, region2.y, country2.y, lat, lng))

#fix some column names and reorder 
co_auth_full_info_latlng <- co_auth_full_info_latlng %>% 
  rename(
    city2 = city2.x,
    region2 = region2.x,
    country2 = country2.x,
  )

write_csv(co_auth_full_info_latlng, "./data/orcid_data_latlng.csv")