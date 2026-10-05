# ---------------------------------------------------------
# Internal helper: Rx5day
# ---------------------------------------------------------

rx5day_calc <- function(date, rain) {

  ord <- order(date)

  date <- as.Date(date[ord])
  rain <- rain[ord]

  if (length(rain) < 5) {
    return(NA_real_)
  }

  if (any(is.na(rain))) {
    return(NA_real_)
  }

  rolling_totals <- sapply(
    seq_len(length(rain) - 4),
    function(i) {
      sum(rain[i:(i + 4)])
    }
  )

  max(rolling_totals)
}


# ---------------------------------------------------------
# Internal helper: maximum consecutive days
# ---------------------------------------------------------

max_consecutive_days <- function(date, condition) {

  ord <- order(date)

  date <- as.Date(date[ord])
  condition <- condition[ord]

  if (length(condition) == 0) {
    return(0L)
  }

  condition[is.na(condition)] <- FALSE

  runs <- rle(condition)

  if (!any(runs$values)) {
    return(0L)
  }

  max(runs$lengths[runs$values])
}


#' Calculate monthly agrometeorological indices
#'
#' Aggregates daily agrometeorological indices to monthly values.
#' The function calculates monthly temperature, rainfall, growing degree
#' days, precipitation extremes, consecutive dry and wet days, heat and
#' cold stress indicators, and monthly data-quality information.
#'
#' @param data A data frame containing daily agrometeorological indices,
#'   typically produced by [daily_agromet()].
#' @param date Name of the date column.
#' @param location Name of the location column.
#' @param tmax Name of the maximum temperature column.
#' @param tmin Name of the minimum temperature column.
#'
#' @return A data frame containing monthly agrometeorological indices
#'   and monthly data-quality information.
#'
#' @examples
#' data(weather_daily)
#'
#' daily <- daily_agromet(
#'   weather_daily,
#'   tbase = 10
#' )
#'
#' monthly <- monthly_agromet(
#'   daily
#' )
#'
#' head(monthly)
#'
#' @export
monthly_agromet <- function(data,
                            date = "Date",
                            location = "Location",
                            tmax = "Tmax",
                            tmin = "Tmin") {

  # Check required columns
  required_columns <- c(
    date,
    location,
    "Rain",
    "Tmax",
    "Tmin",
    "Tavg",
    "DTR",
    "GDD",
    "HeatStress",
    "ColdStress",
    "RainyDay",
    "SU",
    "TR",
    "FD",
    "R10",
    "R20",
    "DryDay",
    "WetDay",
    "HWDay",
    "CWDay"
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

  # Convert date to Date class
  data[[date]] <- as.Date(data[[date]])

  # Create year and month
  data$Year <- as.integer(
    format(data[[date]], "%Y")
  )

  data$Month <- as.integer(
    format(data[[date]], "%m")
  )

  # ---------------------------------------------------------
  # Monthly data-quality information
  # ---------------------------------------------------------

  qc_groups <- split(
    data,
    list(
      data[[location]],
      data$Year,
      data$Month
    ),
    drop = TRUE
  )

  qc_result <- do.call(
    rbind,
    lapply(
      qc_groups,
      function(d) {

        expected_dates <- seq(
          as.Date(
            paste0(
              d$Year[1],
              "-",
              sprintf("%02d", d$Month[1]),
              "-01"
            )
          ),
          by = "month",
          length.out = 2
        )

        expected_days <- as.integer(
          expected_dates[2] - expected_dates[1]
        )

        observed_dates <- unique(
          as.Date(d[[date]])
        )

        missing_dates <- setdiff(
          seq(
            expected_dates[1],
            expected_dates[2] - 1,
            by = "day"
          ),
          observed_dates
        )

        data.frame(
          Location = d[[location]][1],
          Year = d$Year[1],
          Month = d$Month[1],
          ExpectedDays = expected_days,
          ObservedDays = length(observed_dates),
          MissingDates = length(missing_dates),
          MissingRain = sum(is.na(d$Rain)),
          MissingTmax = sum(is.na(d[[tmax]])),
          MissingTmin = sum(is.na(d[[tmin]])),
          Completeness = (
            length(observed_dates) /
              expected_days
          )
        )
      }
    )
  )

  # ---------------------------------------------------------
  # Monthly aggregation
  # ---------------------------------------------------------

  result <- aggregate(
    data[, c(
      "Tmax",
      "Tmin",
      "Tavg",
      "DTR",
      "Rain",
      "GDD",
      "HeatStress",
      "ColdStress",
      "RainyDay",
      "SU",
      "TR",
      "FD",
      "R10",
      "R20",
      "DryDay",
      "WetDay",
      "HWDay",
      "CWDay"
    )],
    by = list(
      Location = data[[location]],
      Year = data$Year,
      Month = data$Month
    ),
    FUN = mean,
    na.rm = TRUE
  )

  # Rainfall and accumulated indices
  result$Rain <- aggregate(
    data$Rain,
    by = list(
      Location = data[[location]],
      Year = data$Year,
      Month = data$Month
    ),
    FUN = sum,
    na.rm = TRUE
  )$x

  result$GDD <- aggregate(
    data$GDD,
    by = list(
      Location = data[[location]],
      Year = data$Year,
      Month = data$Month
    ),
    FUN = sum,
    na.rm = TRUE
  )$x

  # ---------------------------------------------------------
  # Count daily indicators
  # ---------------------------------------------------------

  indicator_columns <- c(
    "HeatStress",
    "ColdStress",
    "RainyDay",
    "SU",
    "TR",
    "FD",
    "R10",
    "R20",
    "DryDay",
    "WetDay",
    "HWDay",
    "CWDay"
  )

  for (var in indicator_columns) {

    result[[var]] <- aggregate(
      data[[var]],
      by = list(
        Location = data[[location]],
        Year = data$Year,
        Month = data$Month
      ),
      FUN = sum,
      na.rm = TRUE
    )$x
  }

  # ---------------------------------------------------------
  # Standard precipitation indices
  # ---------------------------------------------------------

  # PRCPTOT: total precipitation on wet days
  result$PRCPTOT <- aggregate(
    data$Rain,
    by = list(
      Location = data[[location]],
      Year = data$Year,
      Month = data$Month
    ),
    FUN = function(x) {
      sum(
        x[x >= 1],
        na.rm = TRUE
      )
    }
  )$x

  # SDII: Simple Daily Intensity Index
  wet_day_count <- aggregate(
    data$Rain >= 1,
    by = list(
      Location = data[[location]],
      Year = data$Year,
      Month = data$Month
    ),
    FUN = sum,
    na.rm = TRUE
  )$x

  result$SDII <- ifelse(
    wet_day_count > 0,
    result$PRCPTOT / wet_day_count,
    NA_real_
  )

  # Rx1day: maximum 1-day precipitation
  result$Rx1day <- aggregate(
    data$Rain,
    by = list(
      Location = data[[location]],
      Year = data$Year,
      Month = data$Month
    ),
    FUN = function(x) {

      if (all(is.na(x))) {
        NA_real_
      } else {
        max(
          x,
          na.rm = TRUE
        )
      }
    }
  )$x

  # Rx5day: maximum 5-day precipitation total
  rx5_groups <- split(
    data,
    interaction(
      data[[location]],
      data$Year,
      data$Month,
      drop = TRUE,
      lex.order = TRUE
    )
  )

  rx5_values <- lapply(
    rx5_groups,
    function(d) {

      rx5day_calc(
        as.Date(d[[date]]),
        d$Rain
      )
    }
  )

  result$Rx5day <- unname(
    unlist(rx5_values)
  )

  # ---------------------------------------------------------
  # Consecutive dry and wet days
  # ---------------------------------------------------------

  CDD_values <- lapply(
    rx5_groups,
    function(d) {

      max_consecutive_days(
        as.Date(d[[date]]),
        d$Rain < 1
      )
    }
  )

  CWD_values <- lapply(
    rx5_groups,
    function(d) {

      max_consecutive_days(
        as.Date(d[[date]]),
        d$Rain >= 1
      )
    }
  )

  result$CDD <- unname(
    unlist(CDD_values)
  )

  result$CWD <- unname(
    unlist(CWD_values)
  )

  # ---------------------------------------------------------
  # Consecutive heat and cold spells
  # ---------------------------------------------------------

  spell_result <- do.call(
    rbind,
    lapply(
      split(
        data,
        list(
          data[[location]],
          data$Year,
          data$Month
        ),
        drop = TRUE
      ),
      function(d) {

        data.frame(

          Location = d[[location]][1],

          Year = d$Year[1],

          Month = d$Month[1],

          MaxHeatSpellDays =
            max_consecutive_days(
              as.Date(d[[date]]),
              d[[tmax]] > 35
            ),

          MaxColdSpellDays =
            max_consecutive_days(
              as.Date(d[[date]]),
              d[[tmin]] < 15
            )
        )
      }
    )
  )

  # Match spell results to monthly result
  result <- merge(
    result,
    spell_result,
    by = c(
      "Location",
      "Year",
      "Month"
    ),
    all.x = TRUE,
    sort = FALSE
  )

  # Rename monthly summary variables
  names(result)[
    names(result) == "Tmax"
  ] <- "Tmax_mean"

  names(result)[
    names(result) == "Tmin"
  ] <- "Tmin_mean"

  names(result)[
    names(result) == "Tavg"
  ] <- "Tavg_mean"

  names(result)[
    names(result) == "DTR"
  ] <- "DTR_mean"

  names(result)[
    names(result) == "Rain"
  ] <- "Rain_total"

  names(result)[
    names(result) == "GDD"
  ] <- "GDD_total"

  # Arrange output
  result <- result[
    order(
      result$Location,
      result$Year,
      result$Month
    ),
  ]

  row.names(result) <- NULL

  # Add quality-control information
  result <- merge(
    result,
    qc_result,
    by = c(
      "Location",
      "Year",
      "Month"
    ),
    all.x = TRUE,
    sort = FALSE
  )

  result
}
