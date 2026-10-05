#' Assign season and season year
#'
#' Assigns each daily observation to a user-defined crop season
#' and determines the corresponding SeasonYear.
#'
#' @param data A data frame containing a date column.
#' @param seasons A data frame created using create_seasons().
#' @param date Name of the date column.
#'
#' @return The input data frame with Season and SeasonYear columns.
#'
#' @examples
#' data(weather_daily)
#'
#' seasons <- create_seasons(
#'   season_name = c("Kharif", "Rabi"),
#'   start = c("01-06", "01-11"),
#'   end = c("31-10", "31-03")
#' )
#'
#' seasonal_data <- assign_season(
#'   weather_daily,
#'   seasons = seasons
#' )
#'
#' head(seasonal_data)
#'
#' table(seasonal_data$Season)
#'
#' @export
assign_season <- function(data,
                          seasons,
                          date = "Date") {

  if (!date %in% names(data)) {
    stop("Date column not found.")
  }

  data[[date]] <- as.Date(data[[date]])

  data$Season <- NA_character_
  data$SeasonYear <- NA_integer_

  for (i in seq_len(nrow(seasons))) {

    season_name <- seasons$Season[i]

    start_day <- as.integer(
      substr(seasons$Start[i], 1, 2)
    )

    start_month <- as.integer(
      substr(seasons$Start[i], 4, 5)
    )

    end_day <- as.integer(
      substr(seasons$End[i], 1, 2)
    )

    end_month <- as.integer(
      substr(seasons$End[i], 4, 5)
    )

    date_day <- as.integer(
      format(data[[date]], "%d")
    )

    date_month <- as.integer(
      format(data[[date]], "%m")
    )

    crosses_year <- (
      start_month * 100 + start_day >
        end_month * 100 + end_day
    )

    if (!crosses_year) {

      inside <- (
        date_month * 100 + date_day >=
          start_month * 100 + start_day &
          date_month * 100 + date_day <=
          end_month * 100 + end_day
      )

      data$Season[inside] <- season_name

      data$SeasonYear[inside] <-
        as.integer(
          format(data[[date]][inside], "%Y")
        )

    } else {

      inside <- (
        date_month * 100 + date_day >=
          start_month * 100 + start_day |
          date_month * 100 + date_day <=
          end_month * 100 + end_day
      )

      data$Season[inside] <- season_name

      years <- as.integer(
        format(data[[date]][inside], "%Y")
      )

      months <- date_month[inside]

      data$SeasonYear[inside] <- ifelse(
        months >= start_month,
        years,
        years - 1
      )
    }
  }

  data
}


#' Check completeness of crop seasons
#'
#' Calculates seasonal completeness while distinguishing dates that fall
#' outside a location's available observation period from dates missing
#' within that period.
#'
#' @param data A data frame containing Date, Season and SeasonYear.
#' @param seasons A data frame created using create_seasons().
#' @param date Name of the date column.
#'
#' @return A data frame containing seasonal completeness information.
#'
#' @examples
#' data(weather_daily)
#'
#' seasons <- create_seasons(
#'   season_name = c("Kharif", "Rabi"),
#'   start = c("01-06", "01-11"),
#'   end = c("31-10", "31-03")
#' )
#'
#' seasonal_data <- assign_season(
#'   weather_daily,
#'   seasons = seasons
#' )
#'
#' season_qc <- check_season_completeness(
#'   seasonal_data,
#'   seasons = seasons
#' )
#'
#' season_qc
#'
#' @export
check_season_completeness <- function(data,
                                      seasons,
                                      date = "Date") {

  if (!all(c(date, "Season", "SeasonYear") %in% names(data))) {
    stop(
      "Date, Season and SeasonYear columns are required."
    )
  }

  data[[date]] <- as.Date(data[[date]])

  # Available observation period for each location
  location_period <- do.call(
    rbind,
    lapply(
      split(
        data,
        data$Location,
        drop = TRUE
      ),
      function(d) {

        data.frame(
          Location = d$Location[1],
          AvailableStart = min(d[[date]]),
          AvailableEnd = max(d[[date]])
        )
      }
    )
  )

  data <- data[
    !is.na(data$Season) &
      !is.na(data$SeasonYear),
    ,
    drop = FALSE
  ]

  if (nrow(data) == 0) {
    stop("No observations are assigned to a season.")
  }

  groups <- split(
    data,
    list(
      data$Location,
      data$Season,
      data$SeasonYear
    ),
    drop = TRUE
  )

  result <- do.call(
    rbind,
    lapply(
      groups,
      function(d) {

        location_name <- d$Location[1]
        season_name <- d$Season[1]
        season_year <- d$SeasonYear[1]

        season_row <- seasons[
          seasons$Season == season_name,
          ,
          drop = FALSE
        ]

        start_parts <- as.integer(
          strsplit(
            season_row$Start[1],
            "-"
          )[[1]]
        )

        end_parts <- as.integer(
          strsplit(
            season_row$End[1],
            "-"
          )[[1]]
        )

        start_day <- start_parts[1]
        start_month <- start_parts[2]

        end_day <- end_parts[1]
        end_month <- end_parts[2]

        start_date <- as.Date(
          sprintf(
            "%04d-%02d-%02d",
            season_year,
            start_month,
            start_day
          )
        )

        end_year <- ifelse(
          end_month < start_month ||
            (
              end_month == start_month &&
                end_day < start_day
            ),
          season_year + 1,
          season_year
        )

        end_date <- as.Date(
          sprintf(
            "%04d-%02d-%02d",
            end_year,
            end_month,
            end_day
          )
        )

        available <- location_period[
          location_period$Location == location_name,
          ,
          drop = FALSE
        ]

        available_start <- available$AvailableStart[1]
        available_end <- available$AvailableEnd[1]

        expected_dates <- seq(
          start_date,
          end_date,
          by = "day"
        )

        dates_inside_period <- expected_dates[
          expected_dates >= available_start &
            expected_dates <= available_end
        ]

        observed_dates <- unique(
          as.Date(d[[date]])
        )

        missing_dates <- setdiff(
          dates_inside_period,
          observed_dates
        )

        outside_start <- sum(
          expected_dates < available_start
        )

        outside_end <- sum(
          expected_dates > available_end
        )

        expected_within_period <- length(
          dates_inside_period
        )

        observed_within_period <- sum(
          observed_dates >= available_start &
            observed_dates <= available_end
        )

        complete <- (
          outside_start == 0 &
            outside_end == 0 &
            length(missing_dates) == 0
        )

        data.frame(
          Location = location_name,
          Season = season_name,
          SeasonYear = season_year,
          StartDate = start_date,
          EndDate = end_date,
          AvailableStart = available_start,
          AvailableEnd = available_end,
          ExpectedDays = length(expected_dates),
          ExpectedWithinPeriod = expected_within_period,
          ObservedDays = observed_within_period,
          MissingDates = length(missing_dates),
          OutsideStart = outside_start,
          OutsideEnd = outside_end,
          Completeness = ifelse(
            expected_within_period > 0,
            observed_within_period /
              expected_within_period,
            NA_real_
          ),
          Complete = complete
        )
      }
    )
  )

  rownames(result) <- NULL

  result
}
