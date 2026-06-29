#' Fuel metrics LiDAR
#' @description Computes fuel metrics from a bulk density profile. Produces similar outputs to the fCBDprofile_fuelmetrics function. This function is intended for use with bulk density profiles derived from field data, MLS, TLS, or model outputs.
#' @param profile_table A table (e.g., data.frame or data.table) with at least two columns: H (height of the strata) and BD (bulk density in kg/m³). If BD is not available provide three more columns PAD (plant area density in (m²/m3) LMA (leaf mass area in g/m2. use value the average value 141 if unknown) and WD (wood density in kg/m3 use the average value 591 if unknown)
#' @param threshold Numeric or character. Default is 0.012. A critical bulk density threshold used to identify different strata limits such as midstorey height, canopy base, and canopy top. Can be either: Numeric: a bulk density value (in kg/m³) or Character: a percentage of the maximum CBD value (cf example).
#' @param H_PAI Numeric. Height from which PAI and VCI_PAD is estimated. Default is 0 (means from the ground to the top).
#' @return a list of two elements: 1) a named vector with all fuel metrics 2) a data.table with the PAD and CBD profile value plus additional columns if BD not provided (i.e BD_rollmean, LMA, WD, WMA FMA if BD not provided) (three columns: H, PAD and CBD),
#' @details
#' In addition to fuel metrics, this function also estimates structural metrics such as total height, plant area index above 1 meter (PAI_tot), and vertical complexity index (VCI) based on the plant area density profile.
#' @examples
#' \donttest{
#' path2profile <- system.file("extdata", "BD_profile.txt", package = "lidarforfuel")
#'
#' # read a table
#' Profile <- data.table::fread(path2profile)
#'
#' # Run the function with a relative threshold
#' Fuel_metrics <- ffuelmetrics(profile_table = Profile, threshold = "10%")
#' # Run the function with an absolute threshold
#' Fuel_metrics <- ffuelmetrics(profile_table = Profile, threshold = 0.02)
#' }
#' @export
ffuelmetrics <- function(profile_table, threshold, H_PAI = 0) {
  if (any(stringr::str_detect(names(profile_table), "BD")) == F) {
    if (any(str_detect(names(profile_table), "LMA")) == F | any(str_detect(names(profile_table), "WD")) == F) {
      stop("No BD (bulk density) column found. Please provide LMA and WD column in order to compute BD. Use value LMA = 141 and WD = 591 if unknown")
    }
    ### LMA g/m² => kg/m²
    profile_table$LMA <- profile_table$LMA / 1000

    ### Surface volume ratio (SVR: m²/m3) for 4mm diameter twigs (=> wood fuel) = 2.pi.r.l*(1/2)/pi.r².l = 1/r
    SVR <- 1 / 0.002
    ### Wood mass area (WMA)
    profile_table$WMA <- profile_table$WD / SVR

    ### Partition of wood and leaves => M. Soma phd thesis data
    partW <- 0.51
    partL <- 0.49
    ## Fuel mass area  ----

    profile_table$FMA <- 1 / ((partW / profile_table$WMA) + (partL / profile_table$LMA))

    ## CBD in kg/m3 ----
    profile_table$CBD <- profile_table$PAD * (profile_table$FMA)
    profile_table[!is.finite(CBD)] <- NA_real_
    # TODO should understand why PAD can be infinite or NA
    profile_table[!is.finite(CBD)] <- 0
    profile_table[is.na(CBD)] <- 0
  }


  # 2. Work on profile to get FPT and fuel metrics ----
  PAD_CBD_Profile <- data.table::data.table(profile_table)
  d <- profile_table$H[2] - profile_table$H[1]
  ##  Define threshold when threshold is a proportion of CBD max----
  if (stringr::str_detect(threshold, "%")) {
    threshold_prop <- as.numeric(stringr::str_split(threshold, "%", simplify = T)[, 1]) / 100
    threshold <- max(PAD_CBD_Profile[H > 1]$CBD) * threshold_prop
  }

  ### no data above 0.5m
  if (max(PAD_CBD_Profile$H) < 0.5) {
    warning("NULL return: no data above 0.5m height")
    VVP_metrics <- c(Profil_Type = -1, Profil_Type_L = -1, threshold = -1, Height = -1, CBH = -1, FSG = -1, Top_Fuel = -1, H_Bush = -1, continuity = -1, VCI_PAD = -1, VCI_lidr = -1, entropy_lidr = -1, PAI_tot = -1, CBD_max = -1, CFL = -1, TFL = -1, MFL = -1, FL_1_3 = -1, GSFL = -1, FL_0_1 = -1, FMA = -1, date = date, Cover = -1)
    VVP_metrics_CBD <- rep(-1, 150)
    VVP_metrics <- c(VVP_metrics, VVP_metrics_CBD)
    names(VVP_metrics) <- c("Profil_Type", "Profil_Type_L", "threshold", "Height", "CBH", "FSG", "Top_Fuel", "H_Bush", "continuity", "VCI_PAD", "VCI_lidr", "entropy_lidr", "PAI_tot", "CBD_max", "CFL", "TFL", "MFL", "FL_1_3", "GSFL", "FL_0_1", "FMA", "date", "Cover", paste0("CBD_", rep(1:150)))
    if (class(datatype)[1] == "LAS") {
      return(list(VVP_metrics, PAD_CBD_Profile))
    }
    if (datatype == "Pixel") {
      return(as.list(VVP_metrics))
    }
  }
  ## Get CBD roll mean to smooth the profiles & get the profile above CBD threshold  ----
  ### Organise roll mean CBD depending on number of 0.5m strata >3
  if (nrow(PAD_CBD_Profile) > 3) {
    PAD_CBD_Profile$CBD_rollM <- data.table::frollmean(PAD_CBD_Profile$CBD, 3, algo = "exact")
    PAD_CBD_Profile$CBD_rollM[1:3] <- PAD_CBD_Profile$CBD[1:3]
    PAD_CBD_Profile_threshold <- PAD_CBD_Profile[CBD_rollM > threshold]
  }
  ### Organise roll mean CBD depending on number of 0.5m strata <= 3
  if (nrow(PAD_CBD_Profile) <= 3) {
    PAD_CBD_Profile$CBD_rollM <- PAD_CBD_Profile$CBD
    PAD_CBD_Profile_threshold <- PAD_CBD_Profile[CBD_rollM > threshold]
  }
  ### No data
  if (nrow(PAD_CBD_Profile_threshold) == 0) {
    VVP_metrics <- c(Profil_Type = -1, Profil_Type_L = -1, threshold = -1, Height = -1, CBH = -1, FSG = -1, Top_Fuel = -1, H_Bush = -1, continuity = -1, VCI_PAD = -1, VCI_lidr = -1, entropy_lidr = -1, PAI_tot = -1, CBD_max = -1, CFL = -1, TFL = -1, MFL = -1, FL_1_3 = -1, GSFL = -1, FL_0_1 = -1, FMA = -1, date = date, Cover = -1)
    VVP_metrics_CBD <- rep(-1, 150)
    VVP_metrics <- c(VVP_metrics, VVP_metrics_CBD)
    names(VVP_metrics) <- c("Profil_Type", "Profil_Type_L", "threshold", "Height", "CBH", "FSG", "Top_Fuel", "H_Bush", "continuity", "VCI_PAD", "VCI_lidr", "entropy_lidr", "PAI_tot", "CBD_max", "CFL", "TFL", "MFL", "FL_1_3", "GSFL", "FL_0_1", "FMA", "date", "Cover", paste0("CBD_", rep(1:150)))
    if (class(datatype)[1] == "LAS") {
      return(list(VVP_metrics, PAD_CBD_Profile))
    }
    if (datatype == "Pixel") {
      return(as.list(VVP_metrics))
    }
  }

  ## Get number of discontinuity (FSG) of 1m or more ----
  shift_H <- data.table::shift(PAD_CBD_Profile_threshold$H)
  shift_H[1] <- d / 2
  delta_layer <- PAD_CBD_Profile_threshold$H - shift_H
  Discontinuity <- delta_layer[which(delta_layer > 1)]

  ## GEt the FPT ----

  ### one discontinuity = Discontinuous = Stratified = Profile 1
  if (length(Discontinuity) == 1) {
    delta_ID <- which(delta_layer == Discontinuity)
    Profil_Type <- 1
    Profil_Type_L <- 1
  }
  ###  If more than one discontinuities
  if (length(Discontinuity) > 1) {
    ##### if all the gap are <= 1 keep only the first => slightly Complex with small discontinuities = almost continuous = Profile type 2
    if (all(Discontinuity <= 1)) {
      Discontinuity <- Discontinuity[1]
      delta_ID <- which(delta_layer == Discontinuity)[1]
      Profil_Type <- 2
      Profil_Type_L <- 3
    }

    #### If only one of the continuities is >1 keep this one.  Complex : One big discontinuities and other small = almost stratified but small complexity =  Profile type 3
    if (length(which(Discontinuity > 1)) == 1) {
      Discontinuity <- Discontinuity[which(Discontinuity > 1)]
      delta_ID <- which(delta_layer == Discontinuity)
      Profil_Type <- 3
      Profil_Type_L <- 3
    }

    ##### If more than one disconitnuities is above one keep the first = Complex : Multilayered = Profil_Type= 4
    if (length(which(Discontinuity > 1)) > 1) {
      Discontinuity <- Discontinuity[1]
      delta_ID <- which(delta_layer == Discontinuity)

      if (length(delta_ID) > 1) {
        delta_ID <- delta_ID[1]
      }
      Profil_Type <- 4
      Profil_Type_L <- 3
    }
  }
  ## Get metrics ----
  ### Profil continue = Profil 5
  if (length(Discontinuity) == 0) {
    #
    CBH <- 0
    FSG <- 0
    Top_Fuel <- max(PAD_CBD_Profile_threshold$H)
    H_Bush <- Top_Fuel
    continuity <- 1
    Profil_Type <- 5
    Profil_Type_L <- 4
  }

  ### Profil discontinue
  if (length(Discontinuity) > 0) {
    #### profil discontinue without understory strata
    if (min(PAD_CBD_Profile_threshold$H) > 1.25) {
      CBH <- PAD_CBD_Profile_threshold$H[delta_ID]
      FSG <- Discontinuity
      H_Bush <- 0
    }
    #### profil discontinue with understory strata
    if (min(PAD_CBD_Profile_threshold$H) <= 1.25) {
      CBH <- PAD_CBD_Profile_threshold$H[delta_ID]
      FSG <- Discontinuity
      H_Bush <- CBH - FSG
      if (Profil_Type_L == 1) {
        Profil_Type_L <- 2
      }
    }
    Top_Fuel <- max(PAD_CBD_Profile_threshold$H)
    continuity <- 0
  }

  # get metrics (above 0.5m)
  PAI_tot <- sum(PAD_CBD_Profile[H >= H_PAI]$PAD) * d
  VCI_PAD <- -sum(PAD_CBD_Profile[H >= H_PAI]$PAD / sum(PAD_CBD_Profile[H >= H_PAI]$PAD) * log(PAD_CBD_Profile[H >= H_PAI]$PAD / sum(PAD_CBD_Profile[H >= H_PAI]$PAD))) / log(length(PAD_CBD_Profile[H >= H_PAI]$PAD))
  Height <- max(PAD_CBD_Profile$H)
  CBD_max <- max(PAD_CBD_Profile[H >= H_PAI]$CBD_rollM)
  CFL <- sum(PAD_CBD_Profile[H > 1 & H >= CBH & H <= Height]$CBD_rollM) * d
  TFL <- sum(PAD_CBD_Profile[H > 1 & H <= Height]$CBD_rollM) * d
  if (CBH == 0) {
    MFL <- TFL
  } else {
    (MFL <- sum(PAD_CBD_Profile[H > 1 & H <= H_Bush]$CBD_rollM) * d)
  }
  FL_0_1 <- sum(PAD_CBD_Profile[H <= 1]$CBD_rollM) * d
  FL_1_3 <- sum(PAD_CBD_Profile[H > 1 & H <= 3]$CBD_rollM) * d
  if (FSG == 0) {
    GSFL <- 0
  } else {
    GSFL <- sum(PAD_CBD_Profile[H > H_Bush & H <= CBH]$CBD_rollM) * d
  }


  VVP_metrics <- c(Profil_Type, Profil_Type_L, threshold, Height, CBH, FSG, Top_Fuel, H_Bush, continuity, VCI_PAD, PAI_tot, CBD_max, CFL, TFL, MFL, FL_1_3, GSFL, FL_0_1)

  names(VVP_metrics) <- c("Profil_Type", "Profil_Type_L", "threshold", "Height", "CBH", "FSG", "Top_Fuel", "H_Bush", "continuity", "VCI_PAD", "PAI_tot", "CBD_max", "CFL", "TFL", "MFL", "FL_1_3", "GSFL", "FL_0_1")

  return(list(VVP_metrics, PAD_CBD_Profile))
}
