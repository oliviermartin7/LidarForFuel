# The ffuelmetrics2 function computes from PADval  (vertical profiles) and zval (elevation of PADval, typically (0.25,0.75,...) or (0.5,1.5,...))
# if PADval is not provided CBDval can be used (assuming a constant FMA has been used in the whole profile)
# Typical usage of the fonction when applied to multiple tiles is shown just after the function (2s par tiles of 500m approximately)

# CANOPY METRICS (CTH, CDH, CBH2, CBDcan)
# • CTH, in m corresponding to canopy top height in m : CTH corresponds to highest value with CBD>=  CanopyTopBDThreshold=0.0012 kg/m3 or 0.01kg/m3
# • CDH, in m corresponding to canopy dominant height in m : CTH corresponds to highest value with CBD>=0.1*canBDmax and min(CDH,CTH_0p0100)
# • CBH2, in m corresponding to canopy base height, computed from a relative CBD threshold : it can be either :
#     - Representative of the dominant canopy : located above CanopyHeightThreshold (typically =1 to 3m), derived from a maximum of CBD (CanBDmax) measured in [min(CanopyHeightThreshold, CTH*CBHThresholdRatio), CTH] and a minimum
# CBD(CanBDmin) measured in in [min(CanopyHeightThreshold, CTH*CBHThresholdRatio). For determination of CBH2, we use a threshold of CBD corresponding to a weighted average of CBDmin and CBDmax
# (CanopyBDTreshFrac),   fraction threshold to combine CBDmin and CBDmax to look for CBH (0.5 is average, 0.1 is closer to CBDmin)). Moreover CBD is > CanBDThresh continuously between[CBH2,height of CBHmax]
# Default parameters are CanopyHeightThreshold = 1m; CTH*CBHThresholdRatio = CTH/3; CanopyBDTreshFrac = 0.1
#     - When no obvious CBH exists in the upper part of the canopy (ie CBD is decreasing above CTH * CBHThresholdRatio), CBDmax is looked for in [CanopyHeightThreshold, CTH*CBHThresholdRatio] (called CBHlow in the code)
#     - When none of the above exists, CBH2 = CanopyHeightThreshold (default it 1m)
# • CBDcan, in kg/m3 corresponding to sum(CBDval)/(CH-CBH2)

# LADDER FUEL METRICS (LadderTop, LadderBot, ProfType, SFLeq0, SFLeq_int)
# • LadderBot and LadderTop, in m corresponding to the top and bottom of intermediate fuel strata with CBH > LadderFuelBDthreshold, typically ranging between 0.01 and 0.1 kg/m3 (LadderFuelBDthresholds = c(0.01, 0.02, 0.05))
#     Forced to be in [CanopyHeightThreshold; CBH2]
# • FSG, in m Fuel Strata Gap  = CBH2 - LadderTop + 0.5*dz
# • ProfType :
#    -1: No dominant strata : CBH2= CanopyHeightThreshold (1m)
#    -2: Two strata: Dominant (CBH2>1m) + <CanopyHeightThreshold (1m) : with no LadderFuel (LadderTop=LadderBot=CanopyHeightThreshold)
#    -3: Two strata: Dominant + Continuous LadderFuel [0, LadderTop] (ie LadderBot=CanopyHeightThreshold)
#    -4: Three strata: Dominant + Intermediate LadderFuel strictly above [0-1m] (LadderFuelBot>CanopyHeightThreshold)

# FUEL LOAD METRICS in kg/m2
# • FL_k1 and FL_k2 : fuel loads in first and second layer (should be corrected by post treatment because underestimated)
# • SFL, surface fuel load (uncorrected, without liter) below CanopyHeightThreshold : it is equal to FL_k1+FL_k2 (if dz=0.5) or FL_k1 (if dz=1m)
# • CLFL, canopy + ladderfuel above CanopyHeightThreshold
# • CFL is canopy fuel load (that burns in case of crowning) (above can be computed from CLFL-LadderFuelLoad (computed only for median threshold)
# • LadderFuelLoad (fuel load between CanopyHeightThreshold and LadderTop) : this is the amount that is supposed to burn when ladder fuel burns so it should incorporate biomass between 1 and LadderBot (if any)
# • LFL is fuel load between 1m and LadderTop (computed only for median threshold) : should be identical to LadderFuelLoad_0p020, can be 0 is no ladderfuel
#     NB : total fuel load = SFL + LFL + CFL = SFL + CLFL
# •SFLeq0 and  SFLeq_int are equivalent surface fuel derived from ladder fuel role in crowning proba following perrakis 2022 idea (computed from CanopyHeightThreshold to LadderTop)


# divers notes
# NB on borne toutes CBD autour de 1kg/m3 (sur les points DFCI (grandes placettes 20m le q99 vaut 1.26 en dessous de 0.5; 1.07 entre 0.5 et 1 et 0.298 au dessus de 1)
#- CDH vaut CTH quand pas de définition (en pratique strate de 1m); En pratique, il semble utile de prendre le min des deux CDH et CTH
# modification 15/01/2026 : default CanopyBDTreshFrac = 0.1 et dz=1.0
# modification 31/03/2026 : -0.5*dz bottom and top of lader fuel + SFLeq that were overwritten => v1
# modification 08/04/2026 : -0.5*dz CBHlow


#' Fuel metrics from PAD profiles or bulk density profiles
#' @param PADval numeric vector or SpatRaster. PAD values in m2/m3. Set to NULL to input directly CBDval.
#' @param CBDval numeric vector or SpatRaster. CBD values in kg/m3, only used if PADval is NULL.
#' @param FMAcan numeric or SpatRaster. Fuel mass area in kg/m2 for canopy
#' @param FMAshrub numeric or SpatRaster. Fuel mass area in kg/m2 for shrub
#' @param smooth_pad boolean. If true PADval is smoothed by a moving average with a window of 3.
#' Recommended for dz=0.5 or if point number is limited
#' @param zval numeric vector. Bottom height of the PAD layers in m, e.g. for PAD_1_10, zval should be 10.5.
#' @param dz numeric. Vertical resolution of the PADval or CBDval in m, e.g. for PAD_1_10, dz should be 1.
#' @param LadderFuelBDthresholds numeric vector. Minimal bulk density thresholds for Ladder fuels (kg/m3)
#' @param CanopyHeightThreshold numeric. Default minimal value of height in m for canopy/ladder (versus surface fuel) to look for CBH and CTH, typically 1 to 3 m (default 1m)
#' @param CanopyBDTreshFrac numeric. Fraction threshold to combine CBDmin and CBDmax to look for CBH (0.5 is average, 0.1 is close to CBDmin)
#' @param CanopyTopBDThresholds numeric vector. Minimal bulkDensity to look for CTH (canopy height but in terms of fuel<Height)
#' @param CBHThresholdRatio numeric. Ratio of total height above which we look for CBH
#' @param bdmax numeric. Max value of bulk density kg/m3 for canopy (above CanopyHeightThreshold=1 m only)
#' @param M numeric. M factor from Perrakis 2025
#' @param as_tibble logical. If TRUE, returns a tibble. Not used in the raster case.
#' @return a data frame, a tibble or a SpatRaster.
#' @name ffuelmetrics2
#' @export
ffuelmetrics2 <- function(
  PADval = NULL,
  CBDval = NULL,
  FMAcan = 1,
  FMAshrub = 1,
  smooth_pad = FALSE, # default is false
  zval,
  dz,
  LadderFuelBDthresholds = c(0.01, 0.02, 0.05), # CBD thresholds for ladder fuels
  CanopyHeightThreshold = 1, # height threshold for considering canopy/ladder versus surface fuel
  CanopyBDTreshFrac = 0.1, # fraction between CBHmin and CBHmax
  CanopyTopBDThresholds = c(0.0012, 0.01), # CBD threshold to look for canopy top
  CBHThresholdRatio = 1 / 3, # fraction of canopy top to look for CBHmax in dominant canopy
  bdmax = 1.00, # CBD bound
  M = 2.5, # M factor perrakis
  as_tibble = FALSE
) { # two different output formats depending on the need (plots or raster)

  if (inherits(PADval, "SpatRaster") || inherits(CBDval, "SpatRaster")) {
    result <- ffuelmetrics2Rast(
      PADval = PADval,
      CBDval = CBDval,
      FMAcan = FMAcan,
      FMAshrub = FMAshrub,
      smooth_pad = smooth_pad,
      zval = zval,
      dz = dz,
      LadderFuelBDthresholds = LadderFuelBDthresholds,
      CanopyHeightThreshold = CanopyHeightThreshold,
      CanopyBDTreshFrac = CanopyBDTreshFrac,
      CanopyTopBDThresholds = CanopyTopBDThresholds,
      CBHThresholdRatio = CBHThresholdRatio,
      bdmax = bdmax,
      M = M
    )
    return(result)
  }

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

  CBHlow <- NA
  CBH <- NA
  CBH2 <- NA
  CanBDmax <- NA
  CanBDmin <- NA
  CanBDlowmax <- NA
  CanBDlowmin <- NA
  CBD_cbhlow <- NA
  CBD_cbh <- NA
  CBD_cbh2 <- NA

  # 2. --- Canopy top from CanopyTopBDThresholds CTH ---
  # CTHs for 0.0012 and 0.01 kg/m3 threshold
  suf <- gsub("\\.", "p", sprintf("%0.4f", CanopyTopBDThresholds))
  CTHs <- stats::setNames(numeric(length(CanopyTopBDThresholds)), paste0("CTH_", suf))
  for (i in c(1:length(CanopyTopBDThresholds))) {
    above_CTH <- (CBDval >= CanopyTopBDThresholds[i]) & !is.na(CBDval)
    CTHs[[i]] <- if (any(above_CTH)) max(zval[above_CTH]) else NA_real_ # max height with CBD>0.012kg/m3
  }


  # 3A. Computation of  CBH2 for the upper canopy case
  # first computing CBH2 from a maxbd in [1/3CTH,CTH] (can be inf when CanBDmin<CanBDmax) (using the upper CTH...)
  suf <- gsub("\\.", "p", sprintf("%0.4f", max(CanopyTopBDThresholds)))
  CTH <- as.numeric(CTHs[paste0("CTH_", suf)])
  idx_true <- which(zval > max(CanopyHeightThreshold, CTH * CBHThresholdRatio) & zval <= CTH) #>CanopyHeightThreshold and CTH/3 and <CTH
  if (length(idx_true) > 0) { # there are points in the canopy
    CanBDmax <- max(CBDval[idx_true]) # CBD max >CanopyHeightThreshold and CTH/3 and <CTH
    idmax <- idx_true[which(CBDval[idx_true] == CanBDmax)[1]] # lower id of max between max(CanopyHeightThreshold 1/3CTH) and height of CBDmax

    idx_true2 <- which(zval > CanopyHeightThreshold & zval < zval[idmax]) #>CanopyHeightThreshold and <idmax
    if (length(idx_true2) > 0) { # there is space between maxCBD and canopyHeightThreshold
      CanBDmin <- min(CBDval[idx_true2])
      if (!is.na(CanBDmin) && !is.na(CanBDmax) && CanBDmin < CanBDmax) { # normal configuration with a minimum below CanBDmax
        idmin <- idx_true2[which(CBDval[idx_true2] == CanBDmin)[1]] # lower id of CBDmin
        CanBDTresh <- CanBDmin * (1 - CanopyBDTreshFrac) + CanopyBDTreshFrac * CanBDmax # Threshold is a fraction of CanBDmax between CanBDmin and max
        above_CBH <- CBDval >= CanBDTresh & zval > max(CanopyHeightThreshold, zval[idmin]) & zval < zval[idmax]
        if (any(above_CBH)) {
          CBH <- min(zval[above_CBH])
          CBD_cbh <- CBDval[zval == CBH][1]
          for (k in rev(seq.int(idmin + 1, idmax))) { # on part du max et on descend (CBH2>CBH)
            if (CBDval[k - 1] < CanBDTresh) {
              CBH2 <- zval[k][1] - 0.5 * dz # -0.5*dz added after Olivier's test on portuguese dataset
              break()
            }
          }
          CBD_cbh2 <- CBDval[zval == CBH2][1]
        }
      }
    }
  }
  # 3B. Computing of CBH2 from CBHlow  between CanopyHeightThreshold and CTH/3
  idx_true <- which(zval > CanopyHeightThreshold & zval <= CTH * CBHThresholdRatio) #>CanopyHeightThreshold and CTH/3
  if (length(idx_true) > 0) { # there is space between  CanopyHeightThreshold and CTH/3
    CanBDlowmax <- max(CBDval[idx_true]) # CBD max
    idmax <- idx_true[which(CBDval[idx_true] == CanBDlowmax)[1]] # lower id of max between max(CanopyHeightThreshold CTH/3) and height of CBDmax
    idx_true2 <- which(zval > CanopyHeightThreshold & zval < zval[idmax]) #>CanopyHeightThreshold and <idmax
    if (length(idx_true2) > 0) { #
      CanBDlowmin <- min(CBDval[idx_true2])
      idmin <- idx_true2[which(CBDval[idx_true2] == CanBDlowmin)[1]] # lower id of CBDmin
      CanBDTresh <- CanBDlowmin * (1 - CanopyBDTreshFrac) + CanopyBDTreshFrac * CanBDlowmax # Threshold is a fraction of CanBDlowmax between CanBDlowmin and max
      above_CBH <- CBDval >= CanBDTresh & zval > max(CanopyHeightThreshold, zval[idmin]) & zval < zval[idmax]
      if (any(above_CBH)) {
        CBHlow <- min(zval[above_CBH]) - 0.5 * dz # added by fp for consistency with CBH2 7/04/2026
        CBD_cbhlow <- CBDval[zval == CBHlow][1]
      }
    }
  }
  # when CBH2 does not exist above CTH/3, we use CBHlow (if it exists)
  if (is.na(CBH2)) {
    CBH2 <- CBHlow
  }
  # 3C. when CBH2 still not exist => CanopyHeightThreshold
  if (is.na(CBH2)) {
    CBH2 <- CanopyHeightThreshold
  }

  # 4. Correction of CBDval from FMAcan and FMAshrub depending on CBH2
  CBDval[zval > CBH2] <- pmin(PADval[zval > CBH2] * FMAcan, bdmax) # max value at 1kg/m3
  CBDval[zval <= CBH2] <- pmin(PADval[zval <= CBH2] * FMAshrub, bdmax)


  # 5. CDH canopy dominant height and CBDcan (4/12/2025) and min value with CTH
  above_CDH <- (CBDval >= 0.1 * CanBDmax) & !is.na(CanBDmax) & !is.na(CBDval)
  CDH <- if (any(above_CDH)) max(zval[above_CDH]) else NA_real_ # max height with CBD>0.1*CanBDmax
  if (is.na(CDH)) {
    CDH <- CTH
  } # (nb missing data for CDH consist in 0.75m strata...)}
  CDH <- min(CDH, CTH)

  # 6. CBDcan
  height <- min(CDH, CTH)
  inCan <- (!is.na(CBH2) & zval >= CBH2 & zval <= height)
  CBDcan <- ifelse(any(inCan), sum(CBDval[inCan]), 0)

  # 7. no treshold for CBDval in cell k1 and k2, but a treatment will be applied afterwards (to correct bias - quantile mapping)
  CLFL <- sum(CBDval[zval > zlow]) * dz # canopy + ladderfuel
  FL_k1 <- CBDval[1] * dz
  FL_k2 <- CBDval[2] * dz


  # 8. LadderTop, bot et charges

  # --- Bande du bas (LadderBot / LadderTop) ---
  # ladderMax <- ifelse(is.na(CBH2), Inf, CBH2) # change on 31/03/2026 now LadderTop can reach CBH2 when exist (but still<=)
  ladderMax <- CBH2

  # fonction interne qui calcule les seuils de ladderFuels and ProfTypes pour différents seuils
  .ladder_properties_for_threshold <- function(thr) {
    LadderBot <- LadderTop <- CanopyHeightThreshold # NA_real_ # now initialised at 1m (8/04/2026)
    LadderLoad <- SFLeq0 <- SFLeq_int <- 0
    ProfType <- ifelse(CBH2 == CanopyHeightThreshold, 1, 2) # initialised to 1 or 2 depending on CBH2 (8/04/2026)

    above <- (CBDval >= thr) & !is.na(CBDval) & zval > 1 & zval <= ladderMax # inclus le filtre zval>1 et <=CBH2 (4/12/2025)
    idx_true <- which(above)
    if (length(idx_true) > 0) {
      i <- idx_true[1]
      LadderBot <- zval[i] - 0.5 * dz # -0.5dz added 31/03/2026 to get bottom of the layer (zval is cell-centered)
      j <- i
      while (j <= n && isTRUE(above[j])) j <- j + 1 # isTRUE évite NA
      LadderTop <- zval[j - 1] + 0.5 * dz # +0.5dz added 31/03/2026 to get upper of the layer (zval is cell-centered)

      ProfType <- ifelse(LadderBot <= CanopyHeightThreshold, 3, 4) # set to 3 or 4 depending on LadderBot (8/04/2026)

      k <- which(zval > zlow & zval <= LadderTop) # between zlow=1m and LadderTop
      LadderLoad <- sum(CBDval[k]) * dz
      SFLeq0 <- M * LadderLoad * CBH2 / (CBH2 - 0.5 * LadderTop) # basic formulae as in Perrakis 2025
      SFLeq_int <- M * sum(CBDval[k] * CBH2 / (CBH2 - zval[k])) * dz # integrated formulae
    }
    CFL <- sum(CBDval[zval > LadderTop]) * dz # Canopy fuel load kg/m2 (everything above ladder top)  = TL2-LadderLoad0p020
    FSG <- CBH2 - LadderTop + 0.5 * dz
    c(LadderBot = LadderBot, LadderTop = LadderTop, LadderLoad = LadderLoad, SFLeq0 = SFLeq0, SFLeq_int = SFLeq_int, ProfType = ProfType, CFL = CFL, FSG = FSG)
  }
  # --- Ladders pour chaque seuil ---
  thresholds <- sort(unique(LadderFuelBDthresholds))
  # noms sûrs de colonnes (0.01 -> p010 ; 0.05 -> p050 ; 0.1 -> p100)
  suf <- gsub("\\.", "p", sprintf("%0.3f", thresholds))
  ladder_list <- lapply(thresholds, .ladder_properties_for_threshold)
  ladders <- unlist(ladder_list, use.names = FALSE)
  names(ladders) <- as.vector(rbind(
    paste0("LadderBot_", suf), paste0("LadderTop_", suf), paste0("LadderLoad_", suf),
    paste0("SFLeq0_", suf), paste0("SFLeq_intot_", suf), paste0("ProfType_", suf), paste0("CFL_", suf), paste0("FSG_", suf)
  ))


  # 9. compute default ladder metrics for median threshold ladderFuel0.02
  suf <- gsub("\\.", "p", sprintf("%0.3f", stats::median(LadderFuelBDthresholds)))
  # LadderTop <- ladders[paste0("LadderTop_", suf)]
  # LadderBot <- ladders[paste0("LadderBot_", suf)]
  LFL <- ladders[[paste0("LadderLoad_", suf)]] # default ladder fuel load
  CFL <- ladders[[paste0("CFL_", suf)]] #
  FSG <- ladders[[paste0("FSG_", suf)]] #
  ProfType <- ladders[[paste0("ProfType_", suf)]]
  # CFL <- sum(CBDval[zval > LadderTop]) * dz # Canopy fuel load kg/m2 (everything above ladder top)  = TL2-LadderLoad0p020


  # tibble(!!!as.list(ladder_named),#LadderBot = LadderBot, LadderTop = LadderTop,
  out <- c(
    CLFL = CLFL, FL_k1 = FL_k1, FL_k2 = FL_k2,
    ladders, CTHs, CDH = CDH, CBH = CBH, CBH2 = CBH2, FSG = FSG, CBHlow = CBHlow,
    CanBDmax = CanBDmax, CanBDmin = CanBDmin, CanBDlowmax = CanBDlowmax, CanBDlowmin = CanBDlowmin,
    CBDcan = CBDcan, CBD_cbh = CBD_cbh, CBD_cbh2 = CBD_cbh2, CBD_cbhlow = CBD_cbhlow,
    LFL = LFL, CFL = CFL, ProfType = ProfType
  ) # SFLeq0 = SFLeq0, SFLeq_int = SFLeq_intLFLlow=LFLlow, )
  if (as_tibble) {
    return(tibble::as_tibble_row(as.list(out)))
  } else {
    out
  }
}


ffuelmetrics2Rast <- function(
  PADval = NULL, # PAD values in m2/m3; either PAD or CBD should be provided
  CBDval = NULL, # CBD values in kg/m3
  FMAcan = 1, # fuel mass area in kg/m2 for canopy
  FMAshrub = 1, # fuel mass area in kg/m2 for shrub
  smooth_pad = FALSE, # default is false
  zval, # height values in m
  dz, # FP remove the default to avoid errors =0.5, # vertical resolution of the CBDval in m
  LadderFuelBDthresholds = c(0.01, 0.02, 0.05), # minimal bulk density thresholds for Understorey and Ladder fuels (kg/m3)
  CanopyHeightThreshold = 1, # default minimal value of height in m to look for CBH and CTH, typically 1 to 3 m
  CanopyBDTreshFrac = 0.1, # fraction threshold to combine CBDmin and CBDmax to look for CBH (0.5 is average, 0.3 is closer to CBDmin)
  CanopyTopBDThresholds = c(0.0012, 0.01), # minimal bulkDensity to look for CTH (canopy height but in terms of fuel<Height)
  CBHThresholdRatio = 1 / 5, # ratio of total height above which we look for CBH
  bdmax = 1.00, # max value of bulk density kg/m3 for canopy (above 1 m only)
  M = 2.5, # M factor from Perrakis 2025
  as_tibble = FALSE
) {
  # préparation du PAD_rast
  if (is.null(PADval)) {
    PADval <- CBDval / FMAcan # recomputation of PAD profile assuming a constant FMA has been applied to the whole profile
    # print(global(is.na(FMAcan_rast), sum))
    # print(global(!is.finite(FMAcan_rast), sum))
    # print(global(FMAcan_rast == 0, sum))
    # print(global(is.na(PAD_rast), sum))
  }
  if (inherits(FMAcan, "numeric")) {
    FMAcan <- terra::rast(PADval, nlyrs = 1, names = "FMAcan", vals = FMAcan)
  }
  if (inherits(FMAshrub, "numeric")) {
    FMAshrub <- terra::rast(PADval, nlyrs = 1, names = "FMAshrub", vals = FMAshrub)
  }
  PAD_FMA <- c(PADval, FMAcan, FMAshrub) # on empile les FMA à PAD pour utiliser terra:app
  nPAD <- terra::nlyr(PADval)
  # calcul des metrics2 avec terra::app
  metrics2 <- terra::app(
    PAD_FMA,
    fun = function(v) { # v = valeurs pour un pixel donné, pour toutes les couches y compris les FMA
      ffuelmetrics2(
        PADval = v[1:nPAD], # profil PAD du pixel,
        FMAcan = v[nPAD + 1], # FMAcan
        FMAshrub = v[nPAD + 2], # FMAshrub
        zval = zval, dz = dz,
        LadderFuelBDthresholds = LadderFuelBDthresholds,
        CanopyHeightThreshold = CanopyHeightThreshold,
        CanopyBDTreshFrac = CanopyBDTreshFrac,
        CanopyTopBDThresholds = CanopyTopBDThresholds,
        bdmax = bdmax,
        M = M,
        as_tibble = FALSE
      )
    }
  )
  # just to get names of outputs, because terra::app does not keep names
  name_template <- names(ffuelmetrics2(CBDval = rep(0, length(zval)), FMAcan = 1, FMAshrub = 1, zval = zval, dz = dz, as_tibble = FALSE))
  names(metrics2) <- name_template
  return(metrics2)
}
