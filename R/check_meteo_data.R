#' Check daily meteorological data
#'
#' Checks the structure and completeness of daily meteorological data.
#'
#' @param data A data frame containing Date, Location, Rain, Tmax and Tmin.
#' @param date Name of the date column.
#' @param location Name of the location column.
#' @param rain Name of the rainfall column.
#' @param tmax Name of the maximum temperature column.
#' @param tmin Name of the minimum temperature column.
#'
#' @return A list containing data summary and data-quality information.
#'
#' @examples
#' data(weather_daily)
#'
#' qc <- check_meteo_data(weather_daily)
#'
#' qc$summary
#'
#' @export
check_meteo_data <- function(data,
                             date = "Date",
                             location = "Location",
                             rain = "Rain",
                             tmax = "Tmax",
                             tmin = "Tmin") {

  # ---------------------------------------------------------
  # Check required columns
  # ---------------------------------------------------------

  required_columns <- c(
    date,
    location,
    rain,
    tmax,
    tmin
  )

  missing_columns <- setdiff(
    required_columns,
    names(data)
  )

  if (length(missing_columns) > 0) {
    stop(
      "Missing column(s): ",
      paste(missing_columns, collapse = ", ")
    )
  }

  # ---------------------------------------------------------
  # Check and convert date
  # ---------------------------------------------------------

  data[[date]] <- as.Date(data[[date]])

  if (any(is.na(data[[date]]))) {
    stop("The date column contains invalid dates.")
  }

  # ---------------------------------------------------------
  # Check numeric meteorological variables
  # ---------------------------------------------------------

  numeric_columns <- c(
    rain,
    tmax,
    tmin
  )

  non_numeric <- numeric_columns[
    !vapply(
      data[numeric_columns],
      is.numeric,
      logical(1)
    )
  ]

  if (length(non_numeric) > 0) {
    stop(
      "The following column(s) must be numeric: ",
      paste(non_numeric, collapse = ", ")
    )
  }

  # ---------------------------------------------------------
  # Location-wise quality assessment
  # ---------------------------------------------------------

  locations <- unique(data[[location]])

  summary_list <- vector(
    "list",
    length(locations)
  )

  missing_dates_list <- vector(
    "list",
    length(locations)
  )

  for (i in seq_along(locations)) {

    loc <- locations[i]

    d <- data[
      data[[location]] == loc,
      ,
      drop = FALSE
    ]

    d_dates <- as.Date(d[[date]])

    start_date <- min(d_dates)
    end_date <- max(d_dates)

    expected_dates <- seq(
      start_date,
      end_date,
      by = "day"
    )

    expected_days <- length(expected_dates)

    observed_dates <- unique(d_dates)

    observed_days <- length(observed_dates)

    missing_dates <- setdiff(
      expected_dates,
      observed_dates
    )

    duplicate_dates <- sum(
      duplicated(d_dates)
    )

    missing_rain <- sum(
      is.na(d[[rain]])
    )

    missing_tmax <- sum(
      is.na(d[[tmax]])
    )

    missing_tmin <- sum(
      is.na(d[[tmin]])
    )

    completeness <- observed_days / expected_days

    summary_list[[i]] <- data.frame(
      Location = loc,
      StartDate = start_date,
      EndDate = end_date,
      ExpectedDays = expected_days,
      ObservedDays = observed_days,
      MissingDates = length(missing_dates),
      DuplicateDates = duplicate_dates,
      MissingRain = missing_rain,
      MissingTmax = missing_tmax,
      MissingTmin = missing_tmin,
      Completeness = completeness,
      stringsAsFactors = FALSE
    )

    missing_dates_list[[i]] <- missing_dates
  }

  # ---------------------------------------------------------
  # Combine summary
  # ---------------------------------------------------------

  summary_table <- do.call(
    rbind,
    summary_list
  )

  row.names(summary_table) <- NULL

  names(missing_dates_list) <- locations

  # ---------------------------------------------------------
  # Return results
  # ---------------------------------------------------------

  list(
    summary = summary_table,
    missing_dates = missing_dates_list
  )
}
