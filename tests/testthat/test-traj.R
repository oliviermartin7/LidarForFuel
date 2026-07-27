test_that("lasrmdup", {
  las_file <- system.file("extdata", "example.laz", package = "rlas")
  las <- lidR::readLAS(las_file)
  # with duplicate returns
  las@data$ReturnNumber <- 1
  las1 <- lasrmdup(las)
  expect_true(nrow(las) > nrow(las1))

  # with duplicated pulses
  las <- lidR::readLAS(las_file)
  # make a fake duplicated pulse, using different ReturnNumber to make sure it is not the duplicate returns filter
  # that removes points
  las@data <- las@data[1:3, `:=`(ReturnNumber = 1:3, NumberOfReturns = 3, UserData = 0, gpstime = las$gpstime[1])]
  las@data <- las@data[4:6, `:=`(ReturnNumber = 4:6, NumberOfReturns = 6, UserData = 0, gpstime = las$gpstime[1])]
  las1 <- lasrmdup(las)
  expect_true(nrow(las) == nrow(las1) + 6)

  # with more returns then NumberOfReturns
  las@data <- las@data[1:6, `:=`(ReturnNumber = c(1:5, 5), NumberOfReturns = 5, UserData = 0, gpstime = las$gpstime[1])]
  las1 <- lasrmdup(las)
  expect_true(nrow(las) == nrow(las1) + 6)

  # However, in case of the same number of returns by pulse,
  # we cannot detect it
  las@data <- las@data[1:3, `:=`(ReturnNumber = 1:3, NumberOfReturns = 6, UserData = 0, gpstime = las$gpstime[1])]
  las@data <- las@data[4:6, `:=`(ReturnNumber = 4:6, NumberOfReturns = 6, UserData = 0, gpstime = las$gpstime[1])]
  las1 <- lasrmdup(las)
  expect_false(nrow(las) == nrow(las1) + 6)
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
  expect_error(
    {
      traj1 <- get_traj(las1)
    },
    regexp = "Computed trajectory is empty",
    fixed = TRUE
  )

  # what happens if any gpstime=0
  las2 <- lidR::readLAS(path2laz)
  setorder(las2@data, gpstime)
  las2@data <- las2@data[1:1e3, `:=`(gpstime = 0, PointSourceID = 0)][]
  # no error, just loosing traj points
  traj2 <- get_traj(las2)
  expect_true(nrow(traj2) > 0)
  
  # TODO: test multi_pulse

  # add trajectory
  las <- path2laz |> lidR::readLAS()
  las2 <- add_traj_to_las(las, traj)
  expect_contains(names(las2@data), c("Easting", "Northing", "Elevation", "Time"))
})
