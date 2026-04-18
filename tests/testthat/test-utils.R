test_that("is_raster", {
  expect_true(is_raster(stars::read_stars(raster_file())))
  expect_true(is_raster(terra::rast(raster_file())))
})

test_that("raster_bbox", {
  ref <- stars::read_stars(raster_file())
  expect_equal(raster_bbox(ref), sf::st_bbox(ref))
  ref <- terra::rast(raster_file())
  expect_equal(raster_bbox(ref), sf::st_bbox(ref))
})

test_that("raster_res", {
  ref <- stars::read_stars(raster_file())
  expect_equal(raster_res(ref), stars::st_res(ref)["x"])
  ref <- terra::rast(raster_file())
  expect_equal(raster_res(ref), terra::res(ref)[1])
})

test_that("adjust_start", {
  expect_equal(adjust_start(start = 0, xmin = 0, res = 1), 0)
  expect_equal(adjust_start(start = 1, xmin = 0, res = 1), 0)
  expect_equal(adjust_start(start = -1, xmin = 0, res = 1), 0)
  expect_equal(adjust_start(start = 0, xmin = 0.5, res = 1), 0)
  expect_equal(adjust_start(start = 0.5, xmin = 0.5, res = 1), 0.5)
  expect_equal(adjust_start(start = 0.5, xmin = 0, res = 1), -0.5)
})