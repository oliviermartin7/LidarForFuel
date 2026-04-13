test_that(
  "ffuelmetrics2",
  {
    path2laz <- small_laz_file()
    las <- path2laz |> lidR::readLAS()
    traj <- get_traj(las)
    nlas <- fPCpretreatment(las, traj = traj)
    pad <- lidR::cloud_metrics(nlas, pad_metrics()) |>
      unlist()
    pad_names <- names(pad)
    pad_heights <- parse_pad_heights(pad_names)
    zval <- pad_heights$z_bottom
    dz <- unique(pad_heights$dz)
    PADval <- pad[pad_heights$idx]
    metrics <- ffuelmetrics2(
      PADval = PADval,
      FMAcan = 1,
      FMAshrub = 1,
      zval = zval,
      dz = dz
    )
    expect_length(metrics, 45)
  }
)