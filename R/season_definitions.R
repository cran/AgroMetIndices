#' Create crop-season definitions
#'
#' Creates a standardized table of user-defined crop seasons.
#'
#' @param season_name Character vector of season names.
#' @param start Character vector of season start dates in "DD-MM" format.
#' @param end Character vector of season end dates in "DD-MM" format.
#'
#' @return A data frame containing season definitions.
#'
#' @examples
#' seasons <- create_seasons(
#'   season_name = c("Kharif", "Rabi"),
#'   start = c("01-06", "01-11"),
#'   end = c("31-10", "31-03")
#' )
#'
#' seasons
#'
#' @export
create_seasons <- function(season_name,
                           start,
                           end) {

  if (length(season_name) != length(start) ||
      length(season_name) != length(end)) {

    stop(
      "season_name, start and end must have the same length."
    )
  }

  dd_mm_pattern <- "^(0[1-9]|[12][0-9]|3[01])-(0[1-9]|1[0-2])$"

  if (any(!grepl(dd_mm_pattern, start))) {

    stop(
      "start must use DD-MM format."
    )
  }

  if (any(!grepl(dd_mm_pattern, end))) {

    stop(
      "end must use DD-MM format."
    )
  }

  data.frame(
    Season = season_name,
    Start = start,
    End = end,
    stringsAsFactors = FALSE
  )
}
