test_that(
  "ffuelmetrics2",
  {
    path2laz <- small_laz_file()
    las <- path2laz |> lidR::readLAS()
    traj <- get_traj(las)
    nlas <- fPCpretreatment(las, traj = traj)

    # cloud metrics
    pad <- lidR::cloud_metrics(nlas, pad_metrics()) |>
      unlist()
    pad_names <- names(pad)
    pad_heights <- parse_pad_heights(pad_names)
    zval <- pad_heights$z_bottom
    dz <- unique(pad_heights$dz)
    PADval <- pad[pad_heights$idx]
    metrics <- ffuelmetrics2(
      PADval = PADval,
      zval = zval,
      dz = dz
    )
    expect_length(metrics, 45)

    # raster metrics
    pad <- lidR::pixel_metrics(nlas, pad_metrics(), res = 10)
    pad_names <- names(pad)
    pad_heights <- parse_pad_heights(pad_names)
    zval <- pad_heights$z_bottom
    dz <- unique(pad_heights$dz)
    PADval <- pad["PAD"]
    metrics <- ffuelmetrics2(
      PADval = PADval,
      zval = zval,
      dz = dz
    )
    expect_true(terra::nlyr(metrics) == 45)
  }
)
