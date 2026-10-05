#' Calculate daily agrometeorological indices
#'
#' Calculates daily mean temperature, diurnal temperature range,
#' growing degree days, temperature-stress indicators, and
#' precipitation-related indices.
#'
#' @param data A data frame containing Date, Location, Rain, Tmax and Tmin.
#' @param date Name of the date column.
#' @param location Name of the location column.
#' @param tmax Name of the maximum temperature column.
#' @param tmin Name of the minimum temperature column.
#' @param tbase Base temperature for GDD calculation.
#' @param heat_threshold Temperature above which a heat-stress day is recorded.
#' @param cold_threshold Temperature below which a cold-stress day is recorded.
#'
#' @return A data frame containing the original data and calculated indices.
#'
#' @examples
#' data(weather_daily)
#'
#' daily <- daily_agromet(
#'   weather_daily,
#'   tbase = 10
#' )
#'
#' head(daily)
#'
#' @export
daily_agromet <- function(data,
                          date = "Date",
                          location = "Location",
                          tmax = "Tmax",
                          tmin = "Tmin",
                          tbase,
                          heat_threshold = 35,
                          cold_threshold = 15) {

  # Check required columns
  required_columns <- c(
    date,
    location,
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

  # Check Tmax and Tmin
  if (!is.numeric(data[[tmax]])) {
    stop("Tmax must be numeric.")
  }

  if (!is.numeric(data[[tmin]])) {
    stop("Tmin must be numeric.")
  }

  # Check tbase
  if (length(tbase) != 1 || !is.numeric(tbase) || is.na(tbase)) {
    stop("tbase must be a single numeric value.")
  }

  # Convert date to Date class
  data[[date]] <- as.Date(data[[date]])

  # Calculate daily mean temperature
  data$Tavg <- (data[[tmax]] + data[[tmin]]) / 2

  # Calculate diurnal temperature range
  data$DTR <- data[[tmax]] - data[[tmin]]

  # Calculate growing degree days
  data$GDD <- pmax(data$Tavg - tbase, 0)

  # Heat stress day
  data$HeatStress <- as.integer(
    data[[tmax]] > heat_threshold
  )

  # Cold stress day
  data$ColdStress <- as.integer(
    data[[tmin]] < cold_threshold
  )

  # Rainy day
  data$RainyDay <- as.integer(
    data$Rain >= 2.5
  )

  # Summer day
  data$SU <- as.integer(
    data[[tmax]] > 25
  )

  # Tropical night
  data$TR <- as.integer(
    data[[tmin]] > 20
  )

  # Frost day
  data$FD <- as.integer(
    data[[tmin]] < 0
  )

  # Heavy precipitation day: 10 mm
  data$R10 <- as.integer(
    data$Rain >= 10
  )

  # Heavy precipitation day: 20 mm
  data$R20 <- as.integer(
    data$Rain >= 20
  )

  # Dry day
  data$DryDay <- as.integer(
    data$Rain < 1
  )

  # Wet day
  data$WetDay <- as.integer(
    data$Rain >= 1
  )

  # Hot day
  data$HWDay <- as.integer(
    data[[tmax]] > 35
  )

  # Cold day
  data$CWDay <- as.integer(
    data[[tmin]] < 15
  )

  # Return data
  data
}
