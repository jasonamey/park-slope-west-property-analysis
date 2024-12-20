library(shiny)
library(dplyr)
library(sf)
library(leaflet)
library(tidyverse)
library(DT)
library(ggridges)
library(viridis)
library(hrbrthemes)

source("helpers.R")


generate_leaflet_map <- function(combined_t, year1, year2) {
  data <- st_as_sf(combined_t, wkt = "geometry", crs = 4326)

  data <- data |>
    filter(!is.na(change)) |>
    mutate(fill_color = ifelse(change > 0,
      alpha("green", scaled_difference),
      alpha("red", scaled_difference)
    ))

  # Plot the map using leaflet
  leaflet(data) |>
    addTiles() |>
    addPolygons(
      color = "black", weight = .5,
      fillOpacity = 0.9,
      fillColor = ~fill_color, # Apply the color with transparency
      label = ~ paste(
        "Block: ", BLOCK, " | Lot: ", LOT,
        " | Percentage Change: ",
        round(change, 2) * 100, "%"
      ),
      labelOptions = labelOptions(
        noHide = F, # Keep the label visible
        direction = "auto", # Automatically place label in a readable position
        style = list("font-weight" = "bold", "color" = "black")
      )
    ) |>
    setView(lng = -73.9764, lat = 40.6643, zoom = 16) # Adjust the zoom level
}


ui <- fluidPage(
  # Application title
  titlePanel("Property Value Analysis in Park Slope Brooklyn (2010-2023)"),

  # Horizontal layout for inputs
  fluidRow(
    column(
      3,
      sliderInput("year_range", "Select Year Range:",
        min = 2011, max = 2023, value = c(2013, 2019), step = 1
      )
    )
  ),

  # Action Button for triggering analysis
  fluidRow(
    column(
      12,
      actionButton("run_analysis", "Run Analysis")
    )
  ),

  # Main panel for displaying outputs
  mainPanel(
    leafletOutput("map_output"),
    dataTableOutput("t_test_table"),
    verbatimTextOutput("t_test_output"),
    plotOutput("ridge_plot"),
    plotOutput("box_plot"),
    dataTableOutput("summary_by_street")
  )
)


# base_property_info_t <- base_property_info() |>
#   mutate(
#     change = round((!!sym(col_name_2) - !!sym(col_name_1)) / !!sym(col_name_1), 6),
#     change = ifelse(change > 1.5, 1.5, change)
#   ) |>
#   filter(is.finite(change)) |>
#   mutate(scaled_difference = scales::rescale(change, to = c(0, 1)))
# combined_t <- left_join(bunched_t(), base_property_info_t, by = "ID")


server <- function(input, output) {
  # Load and preprocess data
  base_property_info <- reactive({
    read.csv("2010_2023_park_slope_values.csv")
  })

  # Read shapefiles and combine
  bunched_t <- reactive({
    EIGHTH_AVE <- read_sf("eighth_ave/selected_lots.shp")
    PPSW_SHPFILE <- read_sf("ppsw/selected_lots.shp")
    PPW_SHPFILE <- read_sf("ppw/selected_lots.shp")
    FOURTH_AVE <- read_sf("fourth_ave/selected_lots.shp")
    FIFTH_AVE <- read_sf("fifth_ave/selected_lots.shp")
    SIXTH_AVE <- read_sf("sixth_ave/selected_lots.shp")
    SEVENTH_AVE <- read_sf("seventh_ave/selected_lots.shp")

    bunched <- rbind(FIFTH_AVE, SIXTH_AVE, SEVENTH_AVE, EIGHTH_AVE, PPSW_SHPFILE, PPW_SHPFILE, FOURTH_AVE)
    bunched_t <- bunched |>
      mutate(ID = paste(block, lot, sep = "-"))
    return(bunched_t)
  })

  # Trigger analysis when the "Run Analysis" button is pressed
  observeEvent(input$run_analysis, {
    year_1 <- input$year_range[1] %% 100 # Extract last two digits of the first year
    year_2 <- input$year_range[2] %% 100 # Extract last two digits of the second year
    col_name_1 <- paste("VALUE", year_1, sep = "_")
    col_name_2 <- paste("VALUE", year_2, sep = "_")

    base_property_info_t <- base_property_info() |>
      mutate(
        change = round((!!sym(col_name_2) - !!sym(col_name_1)) / !!sym(col_name_1), 6),
        log_change = log1p(abs(change)) * sign(change) # Apply log transformation, preserving the sign
      ) |>
      filter(is.finite(change)) |>
      mutate(scaled_difference = scales::rescale(log_change, to = c(0, 1)))

    combined_t <- left_join(bunched_t(), base_property_info_t, by = "ID")

    # Render the map
    output$map_output <- renderLeaflet({
      generate_leaflet_map(combined_t, input$year_range[1], input$year_range[2])
    })

    percentage_changedf <- combined_t |>
      select(STR_NAME, change) |>
      filter(STR_NAME %in% c("PROSPECT PARK WEST", "8 AVENUE", "7 AVENUE", "6 AVENUE", "5 AVENUE", "PROSPECT PARK SW")) |>
      mutate(percentage_change = change * 100) |>
      rename(`Street Name` = STR_NAME) |>
      arrange(`Street Name`)

    summary_by_street <- percentage_changedf |>
      group_by(`Street Name`) |>
      summarize(
        `mean of % change` = round(mean(percentage_change, na.rm = TRUE), 6),
        `standard deviation of % change` = round(sd(percentage_change, na.rm = TRUE), 6)
      )

    output$summary_by_street <- DT::renderDT({
      datatable(summary_by_street, caption = "Summary of Percentage Change by Street", options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE
      ))
    })
    output$box_plot <- renderPlot({
      ggplot(percentage_changedf, aes(x = `Street Name`, y = percentage_change)) +
        geom_boxplot() +
        labs(title = "Percentage Change by Street Name", x = "Street Name", y = " Percentage Change") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })


    output$ridge_plot <- renderPlot({
      data <- combined_t |>
        select(STR_NAME, change) |>
        filter(STR_NAME %in% c("PROSPECT PARK WEST", "8 AVENUE", "7 AVENUE", "6 AVENUE", "5 AVENUE", "PROSPECT PARK SW")) |>
        arrange(STR_NAME)
      ridge_data <- data |> pivot_longer(
        cols = change,
        names_to = "Metric",
        values_to = "Value"
      )

      # Generate the ridge plot
      ggplot(ridge_data, aes(x = Value * 100, y = STR_NAME, fill = ..x..)) +
        geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
        scale_fill_viridis(name = "Difference", option = "C") +
        labs(
          title = paste("Ridge Plot of Percentage Change in Property Values (", year_1, "-", year_2, ")", sep = ""),
          x = " % Change in Property Value",
          y = "Street"
        ) +
        theme_ipsum() +
        theme_ridges() +
        theme(legend.position = "none")
    })
  })

  observeEvent(input$run_analysis, {
    year_1 <- input$year_range[1] %% 100
    year_2 <- input$year_range[2] %% 100

    t_test_results <- PPW_t_test(year_1, year_2)

    colnames(t_test_results) <- lapply(colnames(t_test_results), function(i) paste0("20", i, "- 20", year_2))

    colnames(t_test_results) <- unlist(colnames(t_test_results))

    output$t_test_table <- DT::renderDT({
      datatable(t_test_results, caption = "T-Test Results for Prospect Park West Analysis", options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE
      ))
    })
  })
}

shinyApp(ui = ui, server = server)
