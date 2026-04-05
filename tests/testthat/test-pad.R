test_that("pad", {
  sup_layers <- c("date", "Cover_h_pad", "Cover_2", "Cover_4", "Cover_6", "cos_theta")

  path2laz <- small_laz_file()
  las <- path2laz |> lidR::readLAS()
  traj <- get_traj(las)
  nlas <- fPCpretreatment(las, traj = traj)

  # cloud metrics
  pad <- lidR::cloud_metrics(nlas, pad_metrics(z0 = 0, dz = 0.5, nlayers = 120)) |>
    unlist()
  expect_length(pad, 120 + 6)
  expect_true(names(pad[length(pad)]) == "PAD_59.5_60")
  expect_true(pad["PAD_17_17.5"] == 0)
  expect_all_true(sup_layers %in% names(pad))

  pad <- lidR::cloud_metrics(nlas, pad_metrics(use_cover = FALSE)) |>
    unlist()

  pad <- lidR::cloud_metrics(nlas, pad_metrics(z0 = 0, dz = 0.5, nlayers = 2)) |>
    unlist()
  expect_all_true(names(pad) == c(sup_layers, "PAD_0_0.5", "PAD_0.5_1"))

  pad <- lidR::cloud_metrics(nlas, pad_metrics(use_cover = FALSE)) |>
    unlist()
  expect_all_true(sup_layers %in% names(pad))

  # test null cover
  nlas1 <- nlas
  nlas1@data = nlas1@data[Z < 2]
  pad <- lidR::cloud_metrics(nlas1, pad_metrics(z0 = 0, dz = 0.5, nlayers = 2)) |>
    unlist()
  expect_true(pad["Cover_2"]==0)
  nlas1@data = nlas1@data[1:90, ]
  expect_warning(
    {
      pad <- lidR::cloud_metrics(nlas1, pad_metrics(z0 = 0, dz = 0.5, nlayers = 2, limit_N_points = 100)) |>
        unlist()
    },
    regexp = "NULL return: the number of points < limit_N_points. Check the point cloud.",
    fixed = TRUE
  )
  expect_true(is.null(pad))
  # expect_all_true(names(pad) == c("date", "Cover", "Cover_4", "Cover_6", "PAD_0_0.5", "PAD_0.5_1"))


  # pixel metrics
  pad_rast <- lidR::pixel_metrics(nlas, pad_metrics(), res = 10)
  expect_all_true(terra::res(pad_rast) == c(10, 10))
  expect_true(terra::nlyr(pad_rast) == (60 + 6))

  # test with Ni
  pad_rast <- lidR::pixel_metrics(nlas, pad_metrics(keep_N = TRUE), res = 10)
  expect_true(terra::nlyr(pad_rast) == (60 * 3 + 6))

  # check names are correctly written to file
  tmpfile <- withr::local_tempfile(fileext = ".tif")
  terra::writeRaster(pad_rast, tmpfile, gdal = c("COMPRESS=DEFLATE"))
  expect_true(file.exists(tmpfile))
  expect_all_true(terra::rast(tmpfile) |> names() == names(pad_rast))
})
