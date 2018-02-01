library(tidyverse)
library(readxl)
library(leaflet)

# data import
market_data <- read_excel("Market_data.xlsx" , sheet = 1)

# data selection
market_sub1 <- market_data %>% 
  select(MarketName:County,Season1Date:WildHarvested) %>% 
  # tidy up data with gather::tidyr
  gather(product, value, Bakedgoods:WildHarvested) %>% 
  filter(value == "Y") %>% 
  # tidy up data with unite::tidyr - paste 3 columns into 1
  unite(address,street:County , sep = ", ") %>% 
  # select approved cols
  select(-value, -Location, -Credit, -Organic) %>% 
  # nesting product col 
  nest(product, .key = "product")

# function to converts any list columns to character type
set_lists_to_chars <- function(x) {
  if (class(x) == 'list') {
    y <- paste(unlist(x[1]), sep = '', collapse = ', ')
  } else {
    y <- x 
  }
  return(y)
}

# apply function to tibble with list columns:
market_sub2 <- as.tibble(map(market_sub1, set_lists_to_chars))

#saveRDS(market_sub2, "market_data_tidy.rds")

# group data by Market name
# using the leaflet package to map the data
# using purrr::pmap() to take each market’s information

# For each market, form the popup with information about it
# Name
# MarketName,
# Website, 
# Facebook,
# address,
# Season Date,
# Season Time,
# product list

market_sub2 <- market_sub2 %>%
  group_by(MarketName) %>%
  mutate(popup_details = pmap(list(MarketName,
                                   Website, 
                                   Facebook,
                                   address,
                                   Season1Date,
                                   Season1Time,
                                   product),
                              function(MarketName, Website, Facebook,
                                       address,Season1Date,Season1Time,product)
                                paste("<b>", MarketName, "</b><br>",
                                      # constructing the markers’ text so it’s displayed nicely
                                      address, "<br>",
                                      "<b><a href='",Website,"'> Website </a></b><br>",
                                      "<b><a href='",Facebook,"'> Facebook </a></b>",
                                      "<p><b>Seasonal opening dates:</b>",Season1Date,"</p>",
                                      "<p><b>Opening hours:</b>",Season1Time,"</p>",
                                      "<p><b> Selling product:</b>", product)))

# generate the map
# set a longitude and latitude for the map and how zoomed in it should be
# calling addTiles() with no arguments; by default, OpenStreetMap tiles are used.
leaflet(data = market_sub2) %>% 
  setView(lng = -89.40123, lat = 43.07305, zoom = 10) %>% 
  addTiles() %>% 
  # icon markers are added using the addMarkers
  addMarkers(~x, ~y, popup = ~popup_details, label = ~MarketName,
             # cluster them using the Leaflet.markercluster plug-in
             clusterOptions = markerClusterOptions())

