# server.R

library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(leaflet)
library(DT)

source("helpers.R") # Optional: use this if you move utility functions to a separate helpers.R file

# Module server for weather input
weatherInputServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive value to store weather data
    weather_data <- reactiveVal()
    
    # Process data based on user choice
    observeEvent(input$data_source, {
      if (input$data_source == "api") {
        # Clear existing data if switching to API
        weather_data(NULL)
      }
    })
    
    # Fetch data using API
    observeEvent(input$fetch, {
      req(input$date_range, input$coordinates, input$api_key)
      start_date <- as.character(input$date_range[1])
      end_date <- as.character(input$date_range[2])
      coordinates <- strsplit(input$coordinates, "\n")[[1]]
      
      all_data <- do.call(rbind, lapply(coordinates, function(coord) {
        lat_lon_id <- strsplit(coord, ",")[[1]]
        lat <- as.numeric(lat_lon_id[1])
        lon <- as.numeric(lat_lon_id[2])
        id <- trimws(lat_lon_id[3])
        if (!is.na(lat) && !is.na(lon) && !is.na(id)) {
          tryCatch(
            fetch_weather_data(lat, lon, start_date, end_date, input$api_key) %>%
              mutate(ID = id),
            error = function(e) {
              showNotification(sprintf("Failed to fetch data for %s", coord), type = "error")
              NULL
            }
          )
        } else {
          showNotification(sprintf("Invalid input: %s", coord), type = "error")
          NULL
        }
      }))
      weather_data(calculate_thermal_suitability_index(calculate_degree_days(all_data)))
      print(head(weather_data())) # for debugging
    })
    
    # Process uploaded CSV file
    observeEvent(input$csv_file, {
      req(input$csv_file)
      file_data <- read.csv(input$csv_file$datapath)
      
      # Debug raw data file
      print("Raw uploaded data:")
      print(head(file_data))
      
      # Validate required columns
      required_cols <- c("lat", "lon", "ID", "date", "mean_temp")
      if (!all(required_cols %in% names(file_data))) {
        showNotification("Uploaded CSV file must contain columns: lat, lon, ID, date, mean_temp", type = "error")
        return()
      }
      
      # Process CSV data
      file_data <- file_data %>%
        mutate(
          date = as.Date(date),
          lat = as.numeric(lat),
          lon = as.numeric(lon),
          mean_temp = as.numeric(mean_temp)
          ) %>%  # Ensure dates are properly formatted
        calculate_degree_days() %>%       # Calculate degree days
        calculate_thermal_suitability_index()  # Calculate TSI
      
      # Debug processed data
      print("Processed weather data:")
      print(head(file_data))
      
      weather_data(file_data)  # Store processed data
      print(head(weather_data())) # for debugging

    #   # Notify user of success
    #   start_date <- min(file_data$date)
    #   end_date <- max(file_data$date)
    #   showNotification(sprintf("Data uploaded successfully. Date range: %s to %s", start_date, end_date), type = "message")
    })
    
    # Provide CSV download
    output$download <- downloadHandler(
      filename = function() {
        paste("weather_data", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(weather_data(), file, row.names = FALSE)
      }
    )
    
    # Return the processed data
    return(weather_data)
  })
}


# Module server for plotting
weatherPlotServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    # Observer to refresh visualizations when `data()` changes
    observe({
      req(data())  # Ensure `data()` is valid
      print("Refreshing visualizations with updated data...")  # Debugging
      
    output$map <- renderLeaflet({
      req(data())
      coords <- data() %>%
        group_by(lat, lon, ID) %>%
        summarise(final_tsi = last(thermal_suitability_index), .groups = 'drop')
      coords <- coords %>%
        mutate(color = case_when(
          final_tsi < 0.5 ~ "blue",
          final_tsi < 1 ~ "yellow",
          final_tsi < 2 ~ "orange",
          TRUE ~ "red"
        ))
      
      pal <- colorFactor(
        palette = c("blue", "yellow", "orange", "red"),
        domain = c("Low (<0.5)", "Moderate (0.5-1)", "High (1-2)", "Very high (>=2)")
      )
      leaflet() %>%
        addTiles() %>%
        addCircleMarkers(
          lng = coords$lon,
          lat = coords$lat,
          popup = paste("ID:", coords$ID, "TSI:", round(coords$final_tsi, 2)),
          radius = 5,
          color = coords$color,
          fill = TRUE
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = c("Low (<0.5)", "Moderate (0.5-1)", "High (1-2)", "Very high (>=2)"),
          title = "Thermal Suitability Index (TSI)",
          opacity = 1
        )
    })
    
    output$plot_temp <- renderPlot({
      req(data())
      ggplot(data(), aes(x = as.Date(date), y = mean_temp, color = interaction(ID))) +
        geom_line(size = 1.5) +
        geom_hline(yintercept = 8, linetype = "dashed", color = "grey") +  # Horizontal line at 8°C
        geom_hline(yintercept = 21, linetype = "dashed", color = "grey") + # Horizontal line at 21°C
        labs(
          title = "", 
          x = "Date", 
          y = "Mean Temperature (°C)", 
          color = "ID",
          caption = "Figure 1. Mean daily temperature for the requested dates and locations. Dashed lines indicate thresholds lower and upper larval development thresholds of 8°C and 21°C. Each requested location is represented as a separate line."
        ) +
        theme_minimal() +
        theme(plot.caption = element_text(size = 12, face = "italic"))
    })
    
    output$plot_degree_days <- renderPlot({
      req(data())
      ggplot(data(), aes(x = as.Date(date), y = degree_days, color = interaction(ID))) +
        geom_line(size = 1.5) +
        geom_hline(yintercept = 245, linetype = "dashed", color = "grey") +  # Horizontal line at 245 degree days
        labs(
          title = " ", 
          x = "Date", 
          y = "Degree Days", 
          color = "ID",
          caption = "Figure 2. The estimated degree-days accumulated during the requested date range, based on the daily mean temperature data. Dashed line indicates the cumulative threshold of 245 degree days required to complete development from the first larval stage to infective larvae within the average gastropod host."
        ) +
        theme_minimal() +
        theme(plot.caption = element_text(size = 12, face = "italic"))
    })
    
    output$plot_tsi <- renderPlot({
      req(data())
      ggplot(data(), aes(x = as.Date(date), y = thermal_suitability_index, color = interaction(ID))) +
        geom_line(size = 1.5) +
        geom_hline(yintercept = 1, linetype = "dashed", color = "grey") +  # Horizontal line at TSI = 1
        labs(
          title = " ", 
          x = "Date", 
          y = "Thermal Suitability Index", 
          color = "ID",
          caption = "Figure 3. The estimated Thermal Suitability Index (TSI). Dashed line indicates TSI threshold of 1, above which development from the first larval stage to the infective stage within the gastropod host is theoretically complete and transmission is possible. Please note this is based on the requested date range and does not account for potential development completed prior to the range of these data."
        ) +
        theme_minimal() +
        theme(plot.caption = element_text(size = 12, face = "italic"))
    })
  })
  })
}

server <- function(input, output, session) {
  weather_data <- weatherInputServer("weatherInput")
  weatherPlotServer("weatherPlot", weather_data)
  # Display data table
  output$table <- renderDataTable({
    req(weather_data())
    datatable(weather_data())
  })
}
