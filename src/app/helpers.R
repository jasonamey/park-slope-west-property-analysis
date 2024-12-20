# helpers.R

library(dplyr)

# Load Data
PROPERTIES <- read.csv("2010_2023_park_slope_values.csv")

# Constants
PPW_STREETS <- c("PROSPECT PARK WEST", "8 AVENUE", "7 AVENUE", "6 AVENUE", "5 AVENUE", "PROSPECT PARK SW")
FOURTH_AVE_STREETS <- PPW_STREETS[PPW_STREETS != "PROSPECT PARK WEST"]

# Functions
create_property_value_changes <- function(year_1 = 11, year_2 = 16, property_data = PROPERTIES) {
  property_data_copy <- property_data |>
    filter(STR_NAME %in% c(
      "PROSPECT PARK WEST", "PROSPECT PARK SW", "8 AVENUE",
      "7 AVENUE", "6 AVENUE", "5 AVENUE", "4 AVENUE"
    ))

  for (i in year_1:(year_2 - 1)) {
    for (j in (i + 1):year_2) {
      col_name_1 <- paste("VALUE", i, sep = "_")
      col_name_2 <- paste("VALUE", j, sep = "_")
      change_col_name <- paste("change_", i, "_to_", j, sep = "")
      property_data_copy <- property_data_copy |>
        mutate(!!change_col_name := round((!!sym(col_name_2) - !!sym(col_name_1)) / !!sym(col_name_1), 6))
    }
  }
  return(property_data_copy)
}


create_yearly_dataframe <- function(start_year, end_year, street_names) {
  years <- seq(start_year, end_year - 1)

  df <- data.frame(matrix(nrow = length(street_names), ncol = length(years)))
  colnames(df) <- years
  rownames(df) <- street_names
  return(df)
}



get_result <- function(p_value, statistic) {
  if (p_value < 0.05 && statistic > 0) {
    result <- "+"
  } else if (p_value < 0.05 && statistic < 0) {
    result <- "-"
  } else {
    result <- "0"
  }
  return(result)
}

PPW_t_test <- function(year_1, year_2, streets = PPW_STREETS) {
  data <- create_property_value_changes(year_1, year_2)
  df <- create_yearly_dataframe(year_1, year_2, streets)

  for (i in year_1:(year_2 - 1)) {
    change_col_name <- paste("change_", i, "_to_", i + 1, sep = "")
    for (street in streets) {
      if (street != "PROSPECT PARK WEST") {
        ppw <- data |>
          filter(STR_NAME == "PROSPECT PARK WEST") |>
          select(STR_NAME, value = !!sym(change_col_name)) |>
          na.omit()

        street_data <- data |>
          filter(STR_NAME == street) |>
          select(STR_NAME, value = !!sym(change_col_name)) |>
          na.omit()

        t_result <- t.test(ppw$value, street_data$value, paired = FALSE)
        df[street, as.character(i)] <- get_result(t_result$p.value, t_result$statistic)
      }
    }
  }
  return(df)
}


FOURTH_t_test <- function(streets = FOURTH_AVE_STREETS) {
  count <- 0
  stat_count <- 0
  str <- ""
  for (i in 11:13) {
    for (j in 14:19) {
      val_1_col <- paste("VALUE_", i, sep = "")
      val_2_col <- paste("VALUE_", j, sep = "")
      fourth_a <- property_copy |>
        filter(STR_NAME == "4 AVENUE") |>
        na.omit()
      t_result <- t.test(fourth_a[[val_1_col]], fourth_a[[val_2_col]], paired = TRUE)
      if (t_result$p.value < .05 && t_result$statistic < 0) {
        fourth_2011_2014_p <- fourth_2011_2014_p + 1
        str <- paste(str, " 20", i, "-20", j, sep = "")
        stat_count <- stat_count + 1
      }
      count <- count + 1
    }
  }

  for (i in 11:13) {
    for (j in 20:23) {
      val_1_col <- paste("VALUE_", i, sep = "")
      val_2_col <- paste("VALUE_", j, sep = "")
      fourth_a <- property_copy |>
        filter(STR_NAME == "4 AVENUE") |>
        na.omit()
      t_result <- t.test(fourth_a[[val_1_col]], fourth_a[[val_2_col]], paired = TRUE)
      if (t_result$p.value < .05 && t_result$statistic < 0) {
        fourth_2011_2020_p <- fourth_2011_2020_p + 1
        str <- paste(str, " 20", i, "-20", j, sep = "")
        stat_count <- stat_count + 1
      }
      count <- count + 1
    }
  }
  return(str)
}
