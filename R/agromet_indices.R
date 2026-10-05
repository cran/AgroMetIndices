#' Calculate agrometeorological indices
#'
#' Main user-facing function for calculating daily, monthly,
#' or user-defined seasonal agrometeorological indices.
#'
#' @param data A daily meteorological data frame.
#' @param frequency Calculation frequency: "daily", "monthly", or "seasonal".
#' @param seasons A season-definition data frame created using create_seasons().
#'   Required when frequency = "seasonal".
#' @param date Name of the date column.
#' @param location Name of the location column.
#' @param rain Name of the rainfall column.
#' @param tmax Name of the maximum temperature column.
#' @param tmin Name of the minimum temperature column.
#' @param tbase Base temperature for GDD calculation.
#' @param heat_threshold Heat-stress threshold.
#' @param cold_threshold Cold-stress threshold.
#'
#' @return Calculated agrometeorological indices.
#'
#' @examples
#' data(weather_daily)
#'
#' # Daily indices
#' daily <- agromet_indices(
#'   weather_daily,
#'   frequency = "daily",
#'   tbase = 10
#' )
#'
#' head(daily)
#'
#' # Monthly indices
#' monthly <- agromet_indices(
#'   weather_daily,
#'   frequency = "monthly",
#'   tbase = 10
#' )
#'
#' head(monthly)
#'
#' # Seasonal indices
#' seasons <- rbind(
#'   create_seasons("Kharif", "01-06", "31-10"),
#'   create_seasons("Rabi", "01-11", "31-03")
#' )
#'
#' seasonal <- agromet_indices(
#'   weather_daily,
#'   frequency = "seasonal",
#'   seasons = seasons,
#'   tbase = 10
#' )
#'
#' head(seasonal$indices)
#'
#' @export
agromet_indices <- function(data,
                            frequency = "daily",
                            seasons = NULL,
                            date = "Date",
                            location = "Location",
                            rain = "Rain",
                            tmax = "Tmax",
                            tmin = "Tmin",
                            tbase,
                            heat_threshold = 35,
                            cold_threshold = 15) {

  frequency <- match.arg(
    frequency,
    choices = c(
      "daily",
      "monthly",
      "seasonal"
    )
  )

  # Calculate daily indices
  daily_result <- daily_agromet(
    data = data,
    date = date,
    location = location,
    tmax = tmax,
    tmin = tmin,
    tbase = tbase,
    heat_threshold = heat_threshold,
    cold_threshold = cold_threshold
  )

  # Daily output
  if (frequency == "daily") {

    return(daily_result)
  }

  # Monthly output
  if (frequency == "monthly") {

    return(
      monthly_agromet(
        data = daily_result,
        date = date,
        location = location,
        tmax = tmax,
        tmin = tmin
      )
    )
  }

  # Seasonal output
  if (frequency == "seasonal") {

    if (is.null(seasons)) {

      stop(
        "seasons must be provided when frequency = 'seasonal'."
      )
    }

    return(
      seasonal_agromet(
        data = daily_result,
        seasons = seasons,
        date = date,
        location = location
      )
    )
  }
}
