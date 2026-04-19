test_that("pixel_filter", {
  path2laz <- small_laz_file()

  #######
  # datetime_filter
  #######

  # test season filter
  las <- lidR::readLAS(path2laz)
  ref <- stars::read_stars(system.file("extdata", "raster_template.tif", package = "lidarforfuel"))
  # make first point to be in month 9
  las@data$gpstime[1] <- 0 # 2021-09-14 01:46:40
  months <- lubridate::month(gpstime_to_datetime(las$gpstime)) |> unique()
  expect_true(any(months == 9))

  # # test with variables
  months = 5:8
  las1 <- lidR::filter_poi(las = las, filter_gpstime(gpstime, months = months))
  # test with variables
  months = 5:8
  las1 <- pixel_filter(las = las, res = ref, filter = ~is_in_season(gpstime_to_datetime(gpstime), months = 5:8))
  las1 <- pixel_filter(las = las, res = ref, filter = .filter_gpstime(months = months))
  # las1 <- pixel_filter2(las = las, res = ref, 
  #   filter = filter_gpstime(gpstime, months = months, deviation_days = 14, gpstime_ref = "2011-09-14 01:46:40"))

  new_months <- lubridate::month(gpstime_to_datetime(las1$gpstime))
  expect_true(!any(new_months == 9))

  # test deviation_days
  las <- lidR::readLAS(path2laz)
  bbox <- sf::st_bbox(ref)
  res <- stars::st_res(ref)[1]
  xmin <- bbox["xmin"]
  ymin <- bbox["ymin"]
  bbox["xmin"] <- xmin + 2 * res
  bbox["ymin"] <- ymin + 2 * res
  bbox["xmax"] <- xmin + 3 * res
  bbox["ymax"] <- ymin + 3 * res
  pixel <- sf::st_as_sfc(bbox) |> sf::st_as_sf()
  # filter out points inside pixel
  las_wo_pix <- lidR::filter_poi(las, !(X %between% bbox[c("xmin", "xmax")] & Y %between% bbox[c("ymin", "ymax")]))
  deviation_days <- 14
  for (ratio in c(0.25, 0.5, 0.75)) {
    pix_las <- las[pixel]
    npoints <- floor(ratio * nrow(pix_las))
    pix_las@data$gpstime[1:npoints] <- 0
    las1 <- rbind(las_wo_pix, pix_las)
    # expect_true(nrow(las1) == nrow(las) + 2 * nrow(pixel_points))
    las1 <- pixel_filter(las = las1, res = ref, filter = .filter_gpstime(deviation_days = deviation_days))
    pix_las1 <- las1[pixel]
    if (ratio <= 0.5) {
      expect_true(nrow(pix_las1) == (nrow(pix_las) - npoints))
      expect_all_true(pix_las1$gpstime != 0)
    } else {
      expect_true(nrow(pix_las1) == npoints)
      expect_all_true(pix_las1$gpstime == 0)
    }
  }
})
