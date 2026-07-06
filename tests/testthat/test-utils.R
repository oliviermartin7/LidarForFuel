test_that("is_raster", {
  x <- raster_template()
  expect_true(is_raster(stars::st_as_stars(x)))
  expect_true(is_raster(x))
})

test_that("raster_bbox", {
  x <- raster_template()
  ref <- stars::st_as_stars(x)
  expect_equal(raster_bbox(ref), sf::st_bbox(ref))
  ref <- x
  expect_equal(raster_bbox(ref), sf::st_bbox(ref))
})

test_that("raster_res", {
  x <- raster_template()
  ref <- stars::st_as_stars(x)
  expect_equal(raster_res(ref), stars::st_res(ref)["x"])
  ref <- x
  expect_equal(raster_res(ref), terra::res(ref)[1])
})

test_that("adjust_start", {
  for (start in c(-10, -1, 0, 0.5, 10)) {
    for (xmin in c(-10, -1, 0, 0.5, 10)) {
      for (res in c(0.5, 1, 10)) {
        new_start <- adjust_start(start = start, xmin = xmin, res = res)
        expect_true(new_start <= xmin)
        expect_true(((xmin - new_start) <= res))
      }
    }
  }
})
