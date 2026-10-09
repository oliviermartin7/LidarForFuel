#' Plot profile & metrics
#' @description Plot the bulk density profile and the corresponding fuel metrics
#' @param see ffuelmetrics2()
#' @export
plot_profile <- function(
  PADval = NULL,
  CBDval = NULL,
  zval,
  Fuel_metrics,
  plotname,
  FMAcan = 0.25,
  FMAshrub = 0.25,
  CanopyHeightThreshold = 1,
  dz = 1,
  smooth_pad = FALSE,
  bdmax = 1
) {
  BD_tot <- height_y <- y <- label <- NULL
  # convert zval to center height of strata
  zval <- zval + 0.5 * dz

  # 1. Initialization
  # top of the surface fuel strata (default 1m)
  zlow <- CanopyHeightThreshold # limitation of surface fuel strata in m,
  ztmp <- dz * (0.5 + c(1:5))
  zval_1mplus <- min(ztmp[ztmp > zlow]) # center height of first cell above zlow

  # when PAD not provided use CBD instead
  if (is.null(PADval)) {
    PADval <- CBDval / FMAcan # recomputation of PAD profile assuming a constant FMA has been applied to the whole profile
  }
  stopifnot(length(PADval) == length(zval)) # check if length of CBDval and zval are consistent

  # TODO should understand why PAD can be infinite or NA or negative
  PADval[!is.finite(PADval)] <- NA_real_
  invalid <- is.na(PADval) | !is.finite(PADval) | PADval < 0
  # if(any(invalid)) stop("PADval contains NA, infinite or negative values")
  PADval[!is.finite(PADval)] <- 0
  PADval[is.na(PADval)] <- 0
  PADval[PADval == -1] <- 0
  PADval[PADval < 0] <- 0
  n <- length(zval)

  # smoothing
  if (smooth_pad) {
    # largeur de la fenêtre pour la moyenne mobile
    k <- 3 # sur 3 points
    PADval <- slider::slide_dbl(PADval, mean, .before = 1, .after = 1)
  }

  # computation of CBDval from PAD with FMAcan and FMAshrub below heightThreshold and bdmax
  CBDval <- pmin(PADval * ifelse(zval > CanopyHeightThreshold, FMAcan, FMAshrub), bdmax) # max value at 1kg/m3

  tab_CBD <- data.table::data.table(cbind(BD_tot = CBDval, height_y = zval))
  tab_CBD <- tab_CBD[BD_tot > 0]

  CBH_lff <- Fuel_metrics[["CBH2"]]
  CDH_lff <- Fuel_metrics[["CDH"]]
  # CBH_terrain=Metrics_lff_field_i$'field_lcbh (avg)_m'
  #  CDH_field=Metrics_lff_field_i$'field_heightOverstory (top3 avg)_m'
  ladder_bot <- Fuel_metrics[["LadderBot_0p050"]]
  ladder_top <- Fuel_metrics[["LadderTop_0p050"]]

  # Créer un data.frame pour les lignes horizontales
  lines_data <- data.frame(
    y = c(CBH_lff, CDH_lff, ladder_bot, ladder_top, CanopyHeightThreshold),
    label = c("CBH_lff", "CDH_lff", "Ladder bottom", "Ladder top", "Surface top"),
    color = c("#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "grey")
  )

  myplot <- ggplot2::ggplot(tab_CBD, ggplot2::aes(x = BD_tot, y = height_y)) +
    ggplot2::geom_path(lwd = 1) +
    ggplot2::geom_hline(
      data = lines_data,
      ggplot2::aes(yintercept = y, color = label, linetype = label),
      lwd = 0.8
    ) +
    ggplot2::scale_color_manual(
      name = "Hauteurs",
      values = stats::setNames(lines_data$color, lines_data$label)
    ) +
    ggplot2::scale_linetype_manual(
      name = "Hauteurs",
      values = c(
        "CBH_lff" = "solid",
        # "CBH terrain" = "solid",
        "CDH_lff" = "solid",
        # "CDH terrain" = "solid",
        "Ladder bottom" = "dashed",
        "Ladder top" = "dashed",
        "Surface top" = "dotted"
      )
    ) +
    ggplot2::ggtitle(plotname) +
    ggthemes::theme_few() +
    ggplot2::theme(legend.position = "right")
  print(myplot)
}
