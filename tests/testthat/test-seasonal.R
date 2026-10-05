test_that("seasonal completeness identifies incomplete boundary seasons", {

  test_data <- data.frame(
    Date = seq(
      as.Date("2023-01-01"),
      as.Date("2024-12-31"),
      by = "day"
    ),
    Location = "Kashi"
  )

  seasons <- data.frame(
    Season = c("Kharif", "Rabi"),
    Start = c("01-06", "01-11"),
    End = c("31-10", "31-03")
  )

  test_data <- assign_season(
    test_data,
    seasons
  )

  season_qc <- check_season_completeness(
    test_data,
    seasons
  )

  expect_true("Complete" %in% names(season_qc))

  expect_false(
    season_qc$Complete[
      season_qc$Season == "Rabi" &
        season_qc$SeasonYear == 2024
    ]
  )

  expect_true(
    all(
      season_qc$Complete[
        season_qc$Season == "Kharif"
      ]
    )
  )
})


test_that("seasonal_agromet uses only complete seasons", {

  dates <- seq(
    as.Date("2023-01-01"),
    as.Date("2024-12-31"),
    by = "day"
  )

  season_data <- data.frame(
    Date = dates,
    Location = "Kashi",
    Rain = 1,
    Tmax = 30,
    Tmin = 20,
    Tavg = 25,
    DTR = 10,
    GDD = 10,
    HeatStress = 0,
    ColdStress = 0,
    RainyDay = 1,
    SU = 0,
    TR = 0,
    FD = 0,
    R10 = 0,
    R20 = 0,
    DryDay = 0,
    WetDay = 1,
    HWDay = 0,
    CWDay = 0
  )

  seasons <- data.frame(
    Season = c("Kharif", "Rabi"),
    Start = c("01-06", "01-11"),
    End = c("31-10", "31-03")
  )

  result <- seasonal_agromet(
    season_data,
    seasons = seasons
  )

  expect_true(
    all(
      result$indices$SeasonYear %in% c(2023, 2024)
    )
  )

  expect_true(
    all(c("indices", "qc") %in% names(result))
  )
})
