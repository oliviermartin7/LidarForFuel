test_that("class_metrics", {
  path2laz <- small_laz_file()
  las <- path2laz |> lidR::readLAS()
  traj <- get_traj(las)
  nlas <- fPCpretreatment(las, traj = traj)

  # class metrics
  classes <- nlas@data$Classification |>
    unique() |>
    sort()
  counts <- lidR::cloud_metrics(nlas, class_metrics(classes = classes)) |>
    unlist()

  expect_length(counts, length(classes))

  counts <- lidR::pixel_metrics(nlas, class_metrics(classes = classes), res = 10)
  expect_warning(
    {
      counts <- lidR::pixel_metrics(nlas, class_metrics(), res = 10)
    },
    regexp = "Unexpected classes are excluded",
    fixed = TRUE
  )
})
