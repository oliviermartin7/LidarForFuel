testthat::test_that("filter_season", {
  plot_hist_days <- FALSE
  path2laz <- system.file(
    "extdata", "M30_FontBlanche.laz",
    package = "lidarforfuel"
  )
  las <- lidR::readLAS(path2laz)

  # here LAS is only month 6 (June)
  months <- 5:10
  las1 <- filter_seasons(las, months, plot_hist_days = plot_hist_days)
  testthat::expect_true(nrow(las1@data) == nrow(las@data))

  months <- 1:5
  testthat::expect_warning({
    las <- filter_seasons(las, months, plot_hist_days = plot_hist_days)
  })
  testthat::expect_true(nrow(las@data) == 0)
})

testthat::test_that("filter_date_mode", {
  # scales well (tested for 1e7 points)
  N <- 9
  # N+1 points have the same date
  data <- data.frame(gpstime = c(1:1e2, runif(N) + 50) * 3600 * 24)
  data[["X"]] <- runif(nrow(data))
  data[["Y"]] <- runif(nrow(data))
  data[["Z"]] <- runif(nrow(data))
  las <- lidR::LAS(data)

  deviation_days <- 2

  testthat::expect_warning({
    las <- filter_date_mode(
      las,
      deviation_days = deviation_days,
      gpstime_ref = "2023-01-01 00:00:00",
      plot_hist_days = FALSE
    )
  })
  testthat::expect_true(nrow(las@data) == deviation_days * 2 + N + 1)

  # test where only one date is present, should not filter any point
  data[["gpstime"]] <- runif(nrow(data)) * 3600 * 24
  las <- lidR::LAS(data)
  las <- filter_date_mode(
    las,
    deviation_days = deviation_days,
    gpstime_ref = "2023-01-01 00:00:00",
    plot_hist_days = FALSE
  )
  testthat::expect_true(nrow(las@data) == nrow(data))
})


testthat::test_that("fPCpretreatment", {
  path2laz <- system.file(
    "extdata", "M30_FontBlanche.laz",
    package = "lidarforfuel"
  )
  # LMA value selected = 120.6 that is the LMA for Pinus halepensis, the
  # dominant species of the plot
  m30_font_blanche_pretreated <- fPCpretreatment(path2laz, LMA = 120.6)
  # displaying the new attributes in the las
  expected_names <- c(
    "Easting", "Northing", "Elevation", "Time", "LMA", "WD", "Zref"
  )
  testthat::expect_contains(names(m30_font_blanche_pretreated), expected_names)
})
