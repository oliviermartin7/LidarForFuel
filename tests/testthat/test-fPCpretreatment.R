test_that("filter_season", {
  plot_hist_days <- FALSE
  ref <- as.POSIXct("2011-09-14 01:46:40", tz = "UTC")
  gpstime <- seq.Date(as.Date("2023-07-01"), as.Date("2023-08-31"), by = "days")
  gpstime <- as.POSIXct(gpstime) - ref
  data <- data.frame(gpstime = gpstime)
  data[["X"]] <- runif(nrow(data))
  data[["Y"]] <- runif(nrow(data))
  data[["Z"]] <- runif(nrow(data))
  las <- lidR::LAS(data)

  # here returned las should be full
  months <- 5:10
  las1 <- filter_seasons(las, months, plot_hist_days = plot_hist_days)
  expect_true(nrow(las1@data) == nrow(las@data))

  # returned las should be half size
  months <- 7
  expect_warning({
    las1 <- filter_seasons(las, months, plot_hist_days = plot_hist_days)
    expect_true(nrow(las1@data) == nrow(las@data) / 2)
  })

  # returned las should be half empty
  months <- 1:5
  expect_warning({
    las1 <- filter_seasons(las, months, plot_hist_days = plot_hist_days)
  })
  expect_true(nrow(las1@data) == 0)
})

test_that("filter_date_mode", {
  # scales well (tested for 1e7 points)
  N <- 9
  # N+1 points have the same date
  data <- data.frame(gpstime = c(1:1e2, runif(N) + 50) * 3600 * 24)
  data[["X"]] <- runif(nrow(data))
  data[["Y"]] <- runif(nrow(data))
  data[["Z"]] <- runif(nrow(data))
  las <- lidR::LAS(data)

  deviation_days <- 2

  expect_warning({
    las <- filter_date_mode(
      las,
      deviation_days = deviation_days,
      gpstime_ref = "2023-01-01 00:00:00",
      plot_hist_days = FALSE
    )
  })
  expect_true(nrow(las@data) == deviation_days * 2 + N + 1)

  # test where only one date is present, should not filter any point
  data[["gpstime"]] <- runif(nrow(data)) * 3600 * 24
  las <- lidR::LAS(data)
  las <- filter_date_mode(
    las,
    deviation_days = deviation_days,
    gpstime_ref = "2023-01-01 00:00:00",
    plot_hist_days = FALSE
  )
  expect_true(nrow(las@data) == nrow(data))
})


test_that("fPCpretreatment", {
  path2laz <- small_laz_file()
  # LMA value selected = 120.6 that is the LMA for Pinus halepensis, the
  # dominant species of the plot
  expect_warning(
    m30_font_blanche_pretreated <- fPCpretreatment(path2laz, LMA = 120.6)
  )
  # displaying the new attributes in the las
  expected_names <- c(
    "Easting", "Northing", "Elevation", "Time", "LMA", "WD", "Zref"
  )
  expect_contains(names(m30_font_blanche_pretreated), expected_names)
})



# test_that("fPCpretreatment-full-tile", {
#   path2laz <- "/media/DATA/boissieu/git/LidarForFuel/draft/data/lidarhd.old/LHD_FXX_0904_6339_PTS_LAMB93_IGN69.copc.laz"
#   # LMA value selected = 120.6 that is the LMA for Pinus halepensis, the
#   # dominant species of the plot
#   tic("Total time")
#   expect_warning(
#     m30_font_blanche_pretreated <- fPCpretreatment(path2laz, LMA = 120.6)
#   )
#   toc()
#   # displaying the new attributes in the las
#   expected_names <- c(
#     "Easting", "Northing", "Elevation", "Time", "LMA", "WD", "Zref"
#   )
#   expect_contains(names(m30_font_blanche_pretreated), expected_names)
# })
