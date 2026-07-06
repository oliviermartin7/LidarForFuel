test_that("pixel_filter", {
  path2laz <- small_laz_file()

  #######
  # datetime_filter
  #######

  # test season filter
  las <- lidR::readLAS(path2laz)
  ref_file <- system.file("extdata", "raster_template.tif", package = "lidarforfuel")
  ref <- stars::read_stars(ref_file)
  # make first point to be in month 9
  las@data$gpstime[1] <- 0 # 2021-09-14 01:46:40
  months <- lubridate::month(gpstime_to_datetime(las$gpstime)) |> unique()
  expect_true(any(months == 9))

  # # test with variables
  months <- 5:8
  las1 <- lidR::filter_poi(las = las, filter_gpstime(gpstime, months = months))
  # test with variables
  months <- 5:8
  las1 <- pixel_filter(las = las, res = ref, filter = ~ is_in_season(gpstime_to_datetime(gpstime), months = 5:8))
  las1 <- pixel_filter(las = las, res = ref, filter = .filter_gpstime(months = months))
  # las1 <- pixel_filter2(las = las, res = ref,
  #   filter = filter_gpstime(gpstime, months = months, deviation_days = 14, gpstime_ref = "2011-09-14 01:46:40"))

  new_months <- lubridate::month(gpstime_to_datetime(las1$gpstime))
  expect_true(!any(new_months == 9))

  # test deviation_days
  # pix1_las and pix2_las are the simulation
  # of las for 2 separated pixels.
  # The second pixel is not used in the test,
  # but is only here to make sure that it works with 2 pixels
  N <- 1e2
  res <- 10
  deviation_days <- 14
  # set gpstime away from 0
  offset <- deviation_days * 2 * 24 * 3600
  data <- data.frame(gpstime = runif(N) + offset, X = runif(N), Y = runif(N), Z = 1)
  pix1_las <- lidR::LAS(data = data)
  # with a second pixel
  N <- 1e2
  data <- data.frame(gpstime = runif(N) + offset, X = runif(N) + res * 2, Y = runif(N), Z = 2)
  pix2_las <- lidR::LAS(data = data)
  expect_warning({las <- rbind(pix1_las, pix2_las)})
  ref <- lidR::pixel_metrics(las, ~ length(Z), res = res, pkg = "stars")

  # check that the filter works at the pixel scale
  for (ratio in c(0.25, 0.5, 0.75)) {
    npoints <- floor(ratio * nrow(pix1_las))
    pix1_las@data$gpstime[1:npoints] <- 0
    expect_warning({las <- rbind(pix1_las, pix2_las)})
    las1 <- pixel_filter(las = las, res = ref, filter = .filter_gpstime(deviation_days = deviation_days))
    new_pix1_las <- las1 |> lidR::filter_poi(Z == 1)
    new_pix2_las <- las1 |> lidR::filter_poi(Z == 2)
    if (ratio < 0.5) {
      expect_true(nrow(new_pix1_las) == (nrow(pix1_las) - npoints))
      expect_all_true(new_pix1_las$gpstime != 0)
    } else {
      expect_true(nrow(new_pix1_las) == npoints)
      expect_all_true(new_pix1_las$gpstime == 0)
    }
    expect_true(nrow(new_pix2_las) == nrow(pix2_las))
  }
})
