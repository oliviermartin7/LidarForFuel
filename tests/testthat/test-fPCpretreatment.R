test_that("filter_season", {
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
  las1 <- lidR::filter_poi(las, filter_gpstime(gpstime, months = months))
  expect_true(nrow(las1@data) == nrow(las@data))

  # returned las should be half size
  months <- 7

  las1 <- lidR::filter_poi(las, filter_gpstime(gpstime, months = months))
  expect_true(nrow(las1@data) == nrow(las@data) / 2)

  # returned las should be half empty
  months <- 1:5
  las1 <- lidR::filter_poi(las, filter_gpstime(gpstime, months = months))
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

  las1 <- lidR::filter_poi(
    las,
    filter_gpstime(
      gpstime,
      deviation_days = deviation_days,
      gpstime_ref = "2023-01-01 00:00:00"
    )
  )
  expect_true(nrow(las1@data) == deviation_days * 2 + N + 1)

  # test where only one date is present, should not filter any point
  data[["gpstime"]] <- runif(nrow(data)) * 3600 * 24
  las <- lidR::LAS(data)
  las <- lidR::filter_poi(
    las,
    filter_gpstime(
      gpstime, deviation_days = deviation_days,
      gpstime_ref = "2023-01-01 00:00:00"
    )
  )
  expect_true(nrow(las@data) == nrow(data))
})


test_that("fPCpretreatment", {
  path2laz <- small_laz_file()
  # LMA value selected = 120.6 that is the LMA for Pinus halepensis, the
  # dominant species of the plot
  expect_warning(
    m30_font_blanche_pretreated <- fPCpretreatment(path2laz)
  )
  # displaying the new attributes in the las
  expected_names <- c(
    "Easting", "Northing", "Elevation", "Time", "Zref"
  )
  expect_contains(names(m30_font_blanche_pretreated), expected_names)

  # write and read to check that the attributes are still there
  outdir <- withr::local_tempdir(pattern = "lidarforfuel") # , .local_envir = globalenv()
  expect_true(file.exists(outdir))
  tmpfile <- file.path(outdir, "m30_font_blanche_pretreated.laz")
  lidR::writeLAS(m30_font_blanche_pretreated, tmpfile)
  m30_font_blanche_pretreated_read <- lidR::readLAS(tmpfile)
  expect_contains(names(m30_font_blanche_pretreated_read), expected_names)

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
