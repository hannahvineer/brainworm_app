# helpers.R

fetch_weather_data <- function(lat, lon, start_date, end_date, api_key) {
  tryCatch({
    url <- glue::glue(
      "https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/{lat},{lon}/{start_date}/{end_date}?unitGroup=metric&elements=datetime,temp&include=days&key={api_key}&contentType=json"
    )
    response <- httr::GET(url)
    if (httr::status_code(response) != 200) stop("API request failed")
    data <- httr::content(response, as = "parsed", type = "application/json")
    if (!"days" %in% names(data)) stop("Unexpected API response structure")
    data$days %>%
      purrr::map_df(~ data.frame(
        date = .x$datetime,
        mean_temp = .x$temp,
        lat = lat,
        lon = lon
      ))
  }, error = function(e) {
    message("Failed to fetch data: ", e$message)
    return(NULL)
  })
}

calculate_degree_days <- function(data, min_threshold = 8, max_threshold = 21) {
  data %>%
    mutate(
      degree_days = case_when(
        mean_temp < min_threshold ~ 0,
        mean_temp > max_threshold ~ max_threshold - min_threshold,
        TRUE ~ mean_temp - min_threshold
      )
    )
}

calculate_thermal_suitability_index <- function(data) {
  data %>%
    group_by(lat, lon) %>%
    mutate(
      cumulative_degree_days = cumsum(degree_days),
      thermal_suitability_index = cumulative_degree_days / 245
    ) %>%
    ungroup()
}

