is_raster <- function(raster) {
  is(raster, "stars") || is(raster, "SpatRaster")
}

raster_bbox <- function(raster) {
  if (is_raster(raster)) {
    return(sf::st_bbox(raster))
  }
  stop("Argument raster should be a stars or SpatRaster object")
}

raster_res <- function(raster) {
  if (is(raster, "stars")) {
    return(stars::st_res(raster)["x"])
  }
  if (is(raster, "SpatRaster")) {
    return(terra::res(raster)[1])
  }
  stop("Argument raster should be a stars or SpatRaster object")
}

#' adjust start as the nearest start inferior or equal to xmin
#' @noRd
adjust_start <- function(start, xmin, res) {
  xmin - ((xmin - start) %% res)
}