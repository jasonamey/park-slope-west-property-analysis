library(sf)
library(tidyverse)
library(leaflet)
library(leaflet.extras)

read_shp_from_zip <- function(zippedURL) {
  td <- tempdir()
  zip_contents <- unzip(zippedURL,
    exdir = td
  )

  fname_shp <- zip_contents[grepl("\\.shp$", zip_contents)]
  boundaries_sf <- read_sf(fname_shp)
  return(boundaries_sf)
}


# enter csv path
PROPERTY_VALUES_2010_2023 <- "final_project/data/finance/2010_2023_park_slope_values.csv"

PARK_SLOPE_PROPERTIES <- read_csv(PROPERTY_VALUES_2010_2023)


# shp file zip path
filepath <- "~/STA9750-2024-FALL/Final-Project/ProspectParkW.zip"

EIGHTH_AVE_SHP <- "final_project/data/maps/EightAvenue.zip"
PPW_SHP <- "final_project/data/maps/ProspectParkW.zip"

# PPW.shpfile<-read_shp_from_zip(PPW_SHP) #read shp file from zip

EIGHTH_AVE <- read_sf("final_project/data/maps/eighth_ave/selected_lots.shp")
PPSW_SHPFILE <- read_sf("final_project/data/maps/ppsw/selected_lots.shp")
PPW_SHPFILE <- read_sf("final_project/data/maps/ppw/selected_lots.shp")

bunched <- rbind(EIGHTH_AVE, PPSW_SHPFILE, PPW_SHPFILE)

bunched_t <- bunched |>
  mutate(ID = paste(block, lot, sep = "-"))

combined_t <- bunched_t |>
  left_join(PARK_SLOPE_PROPERTIES, by = "ID")

glimpse(combined_t)

#### i tried to full join them based on the shape file
# combined<-bunched|>
#   select(borocode,lot,block,geometry) |>
#   rename(BORO=borocode,LOT=lot,BLOCK=block)|>
#   inner_join(park_slope_properties_filtered ,by=c("BORO","BLOCK","LOT"))
#


## enter the combined file , the years to compare.
visualise_test <- function(combined_file, year1, year2) {
  data <- st_as_sf(combined_file, wkt = "geometry", crs = 4326)
  # Extract last two digits of the years
  year1_last_digit <- substr(as.character(year1), 3, 4)
  year2_last_digit <- substr(as.character(year2), 3, 4)

  # Dynamically generate column names for the years
  value_col_1 <- paste0("VALUE_", year1_last_digit)
  value_col_2 <- paste0("VALUE_", year2_last_digit)

  data <- data |>
    mutate(difference = (!!sym(value_col_2) - !!sym(value_col_1)) / !!sym(value_col_1)) |> # Difference between year2 and year1
    filter(!is.na(difference)) |>
    mutate(color = ifelse(difference > 0, "green", "red"))

  glimpse(data)

  library(dplyr)
  library(leaflet)
  library(sf)
  library(scales) # For alpha()

  difference_palette <- colorNumeric(
    palette = c("red", "white", "green"), # Red for decrease, white for neutral, green for increase
    domain = data$difference, # The range of the differences
    na.color = "gray" # Color for NA values
  )

  data <- data |>
    mutate(fill_color = alpha(difference_palette(difference), alpha = 0.5))

  # Plot the map using leaflet
  leaflet(data) |>
    addTiles() |>
    addPolygons(
      color = "black", weight = 1,
      fillOpacity = 0.7,
      fillColor = ~fill_color, # Apply the color with transparency
      popup = ~ paste("Difference (", year1, "-", year2, "):", difference)
    ) |>
    addLegend(
      "bottomright",
      pal = difference_palette,
      values = data$difference,
      title = paste("Difference (", year1, "-", year2, ")"),
      opacity = 1
    ) |>
    setView(
      lng = mean(st_coordinates(data)[, 1]),
      lat = mean(st_coordinates(data)[, 2]),
      zoom = 12
    )
}


visualise_test(combined_t, 2010, 2018)

year1 <- 2010
year2 <- 2018
combined_file <- combined_t

data <- st_as_sf(combined_file, wkt = "geometry", crs = 4326)
# Extract last two digits of the years
year1_last_digit <- substr(as.character(year1), 3, 4)
year2_last_digit <- substr(as.character(year2), 3, 4)

# Dynamically generate column names for the years
value_col_1 <- paste0("VALUE_", year1_last_digit)
value_col_2 <- paste0("VALUE_", year2_last_digit)

data <- data |>
  mutate(difference = (!!sym(value_col_2) - !!sym(value_col_1)) / !!sym(value_col_1)) |> # Difference between year2 and year1
  filter(!is.na(difference)) |>
  mutate(fill_color = ifelse(difference > 0, alpha("green", ifelse(difference > 1, 1, difference)), alpha("red", -1 * difference)))


library(dplyr)
library(leaflet)
library(sf)
library(scales) # For alpha()

# data <- data |>
#   mutate(fill_color = alpha(difference_palette(difference), alpha = 0.5))

# Plot the map using leaflet
leaflet(data) |>
  addTiles() |>
  addPolygons(
    color = "black", weight = .5,
    fillOpacity = 0.9,
    fillColor = ~fill_color, # Apply the color with transparency
    popup = ~ paste("Difference (", year1, "-", year2, "):", difference),
    label = ~ paste("Block: ", BLOCK, " | Lot: ", LOT, " | Percentage Change: ", round(difference, 2) * 10, "%"), # Hover label showing percentage change
    labelOptions = labelOptions(
      noHide = F, # Keep the label visible
      direction = "auto", # Automatically place label in a readable position
      style = list("font-weight" = "bold", "color" = "black")
    )
  ) |>
  setView(
    lng = -73.9764,
    lat = 40.6643,
    zoom = 16 # You can adjust the zoom level based on how zoomed in you want the map
  )
