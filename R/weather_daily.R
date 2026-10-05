#' Daily weather example dataset
#'
#' A two-year daily weather dataset for Kashi, India, containing
#' daily rainfall, maximum temperature, and minimum temperature.
#'
#' The dataset covers 1 January 2023 to 31 December 2024,
#' with 731 daily observations, including the leap day in 2024.
#'
#' @format A data frame with 731 rows and 5 variables:
#' \describe{
#'   \item{Date}{Date of observation.}
#'   \item{Location}{Location name.}
#'   \item{Rain}{Daily rainfall (mm).}
#'   \item{Tmax}{Daily maximum air temperature (°C).}
#'   \item{Tmin}{Daily minimum air temperature (°C).}
#' }
#'
#' @source Local daily weather dataset used for demonstration
#' of the \pkg{AgroMeteoIndices} package.
#'
#' @examples
#' data(weather_daily)
#' head(weather_daily)
#'
"weather_daily"
