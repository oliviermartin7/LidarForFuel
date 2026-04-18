#' Compute fuel mass area from leaf mass area and wood density
#'
#' @param LMA numeric. Leaf mass area in g/m2
#' @param WD numeric. Wood density in kg/m3
#' @param SVR numeric. Surface volume ratio (SVR: m2/m3), see notes.
#' For twigs of radius r, SVR = 2.pi.r.l*(1/2)/pi.r2.l = 1/r.
#' Default is for twigs of radius 2mm.
#' @param partW numeric. Wood mass area fraction
#' @param partL numeric. Leaf mass area fraction
#' @return numeric. Fuel mass area in kg/m2
#' @export
fuel_mass_area <- function(LMA = 140, WD = 591, SVR = 1 / 0.002, partW = 0.51, partL = 0.49) {
  WMA <- WD / SVR
  FMA <- 1 / ((partW / WMA) + (partL / LMA))
  return(FMA)
}
