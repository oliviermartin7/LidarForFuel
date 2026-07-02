test_that("lasrmdup", {
  las_file <- system.file("extdata", "example.laz", package = "rlas")
  las <- lidR::readLAS(las_file)
  las@data$ReturnNumber <- 1 # make duplicates
  las1 <- lasrmdup(las)

  expect_true(nrow(las) > nrow(las1))
})

test_that("lasrenumber", {
  las_file <- system.file("extdata", "example.laz", package = "rlas")
  # example is wrongly numbered unfortunatly...
  las <- lidR::readLAS(las_file) |> lasrenumber()
  las1 <- lidR::readLAS(las_file) |> lasrenumber()
  las@data[, ReturnNumber := ReturnNumber + 1] # remove first return
  expect_all_false(las$ReturnNumber == las1$ReturnNumber)
  las2 <- lasrenumber(las)
  expect_all_true(las2$ReturnNumber == las1$ReturnNumber)
})


test_that("get and add traj", {
  path2laz <- small_laz_file()
  las <- path2laz |> lidR::readLAS()
  traj <- get_traj(las)
  expect_true(nrow(traj) > 0)
  expect_contains(names(traj), c("gpstime", "PointSourceID", "SCORE", "geometry"))

  # get default trajectory
  # when not enough pulses to reconstruct
  las1 <- lidR::LAS(las)
  las1@data <- las1@data[1:50, ]
  expect_warning({
    traj1 <- get_traj(las1)
  })
  expect_true(nrow(traj1) > 0)
  # TODO: test multi_pulse

  # add trajectory
  las2 <- add_traj_to_las(las, traj)
  expect_contains(names(las2@data), c("Easting", "Northing", "Elevation", "Time"))
})
