# ---------------------------------------------------------
# Internal helper: seasonal Rx5day
# ---------------------------------------------------------

rx5day_season_calc <- function(date, rain) {

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
# Internal helper: seasonal maximum consecutive days
# ---------------------------------------------------------

max_consecutive_season_days <- function(date, condition) {

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


#' Calculate seasonal agrometeorological indices
#'
#' Calculates seasonal agrometeorological indices for user-defined
#' crop seasons. Only complete seasons are used for calculation.
#'
#' @param data A data frame containing daily agrometeorological indices,
#'   typically produced by [daily_agromet()].
#' @param seasons A data frame created using [create_seasons()].
#' @param date Name of the date column.
#' @param location Name of the location column.
#'
#' @return A list containing two elements:
#'   \code{indices}, a data frame containing seasonal agrometeorological
#'   indices, and \code{qc}, a data frame containing seasonal
#'   completeness information.
#'
#' @examples
#' data(weather_daily)
#'
#' daily <- daily_agromet(
#'   weather_daily,
#'   tbase = 10
#' )
#'
#' seasons <- create_seasons(
#'   season_name = c("Kharif", "Rabi"),
#'   start = c("01-06", "01-11"),
#'   end = c("31-10", "31-03")
#' )
#'
#' seasonal <- seasonal_agromet(
#'   daily,
#'   seasons = seasons
#' )
#'
#' head(seasonal$indices)
#'
#' head(seasonal$qc)
#'
#' @export
seasonal_agromet <- function(data,
                             seasons,
                             date = "Date",
                             location = "Location") {

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

  data[[date]] <- as.Date(data[[date]])

  # Assign seasons and SeasonYear
  data <- assign_season(
    data = data,
    seasons = seasons,
    date = date
  )

  # Seasonal completeness
  season_qc <- check_season_completeness(
    data = data,
    seasons = seasons,
    date = date
  )

  # Keep only complete seasons
  complete_seasons <- season_qc[
    season_qc$Complete == TRUE,
    ,
    drop = FALSE
  ]

  if (nrow(complete_seasons) == 0) {

    stop(
      "No complete seasons are available for calculation."
    )
  }

  data_complete <- merge(
    data,
    complete_seasons[
      ,
      c(
        "Season",
        "SeasonYear"
      )
    ],
    by = c(
      "Season",
      "SeasonYear"
    ),
    all = FALSE
  )

  # Calculate seasonal indices
  groups <- split(
    data_complete,
    list(
      data_complete[[location]],
      data_complete$Season,
      data_complete$SeasonYear
    ),
    drop = TRUE
  )

  result <- do.call(
    rbind,
    lapply(
      groups,
      function(d) {

        data.frame(

          Location = d[[location]][1],

          Season = d$Season[1],

          SeasonYear = d$SeasonYear[1],

          Tmax_mean = mean(
            d[["Tmax"]]
          ),

          Tmin_mean = mean(
            d[["Tmin"]]
          ),

          Tavg_mean = mean(
            d$Tavg
          ),

          DTR_mean = mean(
            d$DTR
          ),

          Rain_total = sum(
            d$Rain
          ),

          GDD_total = sum(
            d$GDD
          ),

          HeatStress = sum(
            d$HeatStress
          ),

          ColdStress = sum(
            d$ColdStress
          ),

          RainyDay = sum(
            d$RainyDay
          ),

          SU = sum(
            d$SU
          ),

          TR = sum(
            d$TR
          ),

          FD = sum(
            d$FD
          ),

          R10 = sum(
            d$R10
          ),

          R20 = sum(
            d$R20
          ),

          DryDay = sum(
            d$DryDay
          ),

          WetDay = sum(
            d$WetDay
          ),

          HWDay = sum(
            d$HWDay
          ),

          CWDay = sum(
            d$CWDay
          ),

          PRCPTOT = sum(
            d$Rain[d$Rain >= 1]
          ),

          SDII = ifelse(
            sum(d$Rain >= 1) > 0,
            sum(d$Rain[d$Rain >= 1]) /
              sum(d$Rain >= 1),
            NA_real_
          ),

          Rx1day = max(
            d$Rain
          ),

          Rx5day = rx5day_season_calc(
            as.Date(d[[date]]),
            d$Rain
          ),

          CDD = max_consecutive_season_days(
            as.Date(d[[date]]),
            d$Rain < 1
          ),

          CWD = max_consecutive_season_days(
            as.Date(d[[date]]),
            d$Rain >= 1
          ),

          MaxHeatSpellDays = max_consecutive_season_days(
            as.Date(d[[date]]),
            d$Tmax > 35
          ),

          MaxColdSpellDays = max_consecutive_season_days(
            as.Date(d[[date]]),
            d$Tmin < 15
          )
        )
      }
    )
  )

  rownames(result) <- NULL

  list(
    indices = result,
    qc = season_qc
  )
}
