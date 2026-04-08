# modification 31/03/2026 : bottom and top of lader fuel + SFLeq that were overwritten => v1
# modification 15/01/2026 : CanopyBDTreshFrac = 0.1 et dz=1.0

# TODO should be passed entirely in PAD (with FMAshrub and can)
# The computeFuelMetrics function computes from PADval  (vertical profiles) and zval (elevation of PADval, typically (0.25,0.75,...))
# if PADval is not provided CBDval can be used (assuming a constant FMA has been used in the whole profile)
# typical usage of the fonction when applied to multiple tiles is shown just after the function (2s par tiles of 500m approximately)
# - CTH, in m corresponding to canopy top height in m : CTH corresponds to highest value with CBD>=  CanopyTopBDThreshold=0.0012 kg/m3 or 0.01kg/m3
# - CDH, in m corresponding to canopy dominant height in m : CTH corresponds to highest value with CBD>=0.1*canBDmax
# In practice the best top height estimates seems to be min(CDH,CTH_0p0100) [See expert rules]
# - CBH [not used], in m corresponding to canopy base height located above CanopyHeightThreshold (typically =1 to 3m), derived from a maximum of CBD (CanBDmax) measured in [min(CanopyHeightThreshold, CTH/3), CTH] and a minimum
# CBD(CanBDmin) measured in in [min(CanopyHeightThreshold, CTH/3), height of CBH==canBDmax[. For determination of CBH, we use a threshold of CBD corresponding to a weighted average of CBDmin and CBDmax
# (CanopyBDTreshFrac = 0.3, # fraction threshold to combine CBDmin and CBDmax to look for CBH (0.5 is average, 0.3 is closer to CBDmin))
# in some rare cases CBD is decreasing above CTH/3 so in this case CBH and CBDmax and min are looked for below CTH/3
# - CBH2 is similar to CBH but sometimes higher as CBH2 expects that CBD is > CanBDThresh continuously between[CBH2,height of CBHmax]; this is not the case for CBH in case of multiple maximum
# CBDThreshold = 0.05
# - TFL2: sum of bulk density >1m (complementary to FL_k1+FL_k2 (if dz=0.5) or FL_k1 is dz=1m)
# - LadderBot and LadderTop, in m corresponding to the lower fuel strata with CBH > LadderFuelBDthreshold, typically ranging between 0.01 and 0.1 kg/m3
# - LadderFuelLoad (fuel load between 1m and LadderTop) : this is the amount that is supposed to burn when ladder fuel burns so it should incorporate biomass between 1 and LadderBot (if any)
# - SFLeq0 and  SFLeq_int are equivalent surface fuel derived from ladder fuel role in crowning proba following perrakis 2022 idea (computed from 1m to LadderTop)
# - ProfType :
#    *1: No dominant strata (1a: no CBH2, 1b: CBHLow &(no CBH2 or TODO CBH2<CBHLow): in this case LadderFuel are computed up to CBHLow)
#    *2: Two strata: Dominant + <1m, with no LadderFuel>1m avec et sans cover low
#    *3: Two strata: Dominant +Continuous LadderFuel, with LadderFuelBot==1.25m
#    *4: Three strata: Dominant + Intermediate LadderFuel (LadderFuelBot>1.25) avec et sans cover low
# LFL is fuel load between 1m and LadderTop (computed only for median threshold) : should be identical to LadderFuelLoad_0p020
# CFL is canopy fuel load (that burns in case of crowning) can be computed from TFL2-LadderFuelLoad (computed only for median threshold)


# on peut globalement borner les CBD autour de 1kg/m3
# sur les points DFCI (grandes placettes 20m le q99 vaut 1.26 en dessous de 0.5; 1.07 entre 0.5 et 1 et 0.298 au dessus de 1)

# Expert rules;règles expertes issues des analyses du 5/12/2025
#- CDH vaut CTH quand pas de définition (en pratique strate de 1m); En pratique, il semble utile de prendre le min des deux CDH et CTH
#- use CBH2 rather than CBH (CBH2 is higher as it requires the canopy to be continuously above the threshold below its maximum)
#- Parfois CBH2<CBHlow (1/3% des placettes) => CBHlow semble mieux adapté à ces cas => proposition de remplacement de CBH2 par CBHlow

#' Fuel metrics from PAD profiles or bulk density profiles
#'
#' @param PADval numeric vector. PAD values in m2/m3. Set to NULL to input directly CBDval.
#' @param CBDval numeric vector. CBD values in kg/m3, only used if PADval is NULL.
#' @param FMAcan numeric. Fuel mass area in kg/m2 for canopy
#' @param FMAshrub numeric. Fuel mass area in kg/m2 for shrub
#' @param zval numeric vector. Middle height of the PAD layers in m, e.g. for PAD_1_10, zval should be 10.5.
#' @param dz numeric. Vertical resolution of the PADval or CBDval in m, e.g. for PAD_1_10, dz should be 1.
#' @param LadderFuelBDthresholds numeric vector. Minimal bulk density thresholds for Understorey and Ladder fuels (kg/m3)
#' @param CanopyHeightThreshold numeric. Default minimal value of height in m to look for CBH and CTH, typically 1 to 3 m
#' @param CanopyBDTreshFrac numeric. Fraction threshold to combine CBDmin and CBDmax to look for CBH (0.5 is average, 0.3 is closer to CBDmin)
#' @param CanopyTopBDThresholds numeric vector. Minimal bulkDensity to look for CTH (canopy height but in terms of fuel<Height)
#' @param CBHThresholdRatio numeric. Ratio of total height above which we look for CBH
#' @param bdmax numeric. Max value of bulk density kg/m3 for canopy (above 1 m only)
#' @param M numeric. M factor from Perrakis 2025
#' @param asTibble logical. If TRUE, returns a tibble.
#'
#' @name ffuelmetrics2
#' @export
ffuelmetrics2 <- function(
  PADval = NULL,
  CBDval = NULL,
  FMAcan = NULL,
  FMAshrub = NULL,
  zval,
  dz = 0.5,
  LadderFuelBDthresholds = c(0.01, 0.02, 0.05),
  CanopyHeightThreshold = 1,
  CanopyBDTreshFrac = 0.1,
  CanopyTopBDThresholds = c(0.0012, 0.01),
  CBHThresholdRatio = 1 / 5,
  bdmax = 1.00,
  M = 2.5,
  asTibble = FALSE
) { # two different output formats depending on the need (plots or raster)

  zlow <- 1.0 # limitation of lower uncertain strata in m, for TFL2 computation, etc
  ztmp <- dz * (0.5 + c(1:3))
  zval_1mplus <- min(ztmp[ztmp > zlow]) # center height of first cell above zlow

  if (is.null(PADval)) {
    PADval <- CBDval / FMAcan # recomputation of PAD profile assuming a constant FMA has been applied to the whole profile
  }
  stopifnot(length(PADval) == length(zval)) # check if length of CBDval and zval are consistent

  PADval[!is.finite(PADval)] <- NA_real_
  # TODO should understand why PAD can be infinite or NA or negative
  invalid <- is.na(PADval) | !is.finite(PADval) | PADval < 0
  # if(any(invalid)) stop("PADval contains NA, infinite or negative values")
  PADval[!is.finite(PADval)] <- 0
  PADval[is.na(PADval)] <- 0
  PADval[PADval == -1] <- 0
  PADval[PADval < 0] <- 0
  n <- length(zval)

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

  # 1. --- Bande du haut (CTH / CBH) ---
  # CTHs for 0.0012 and 0.01 kg/m3 threshold
  suf <- gsub("\\.", "p", sprintf("%0.4f", CanopyTopBDThresholds))
  CTHs <- setNames(numeric(length(CanopyTopBDThresholds)), paste0("CTH_", suf))
  for (i in c(1:length(CanopyTopBDThresholds))) {
    above_CTH <- (CBDval >= CanopyTopBDThresholds[i]) & !is.na(CBDval)
    CTHs[[i]] <- if (any(above_CTH)) max(zval[above_CTH]) else NA_real_ # max height with CBD>0.012kg/m3
  }


  # 2. Computation of CBH and CBH2 for the canopy
  # computing CBH and CBH2 from a maxbd in [1/3CTH,CTH] (can be inf when CanBDmin<CanBDmax) (using the upper CTH...)
  suf <- gsub("\\.", "p", sprintf("%0.4f", max(CanopyTopBDThresholds)))
  CTH <- as.numeric(CTHs[paste0("CTH_", suf)])
  idx_true <- which(zval > max(CanopyHeightThreshold, CTH * CBHThresholdRatio) & zval <= CTH) #>CanopyHeightThreshold and CTH/5 and <CTH
  if (length(idx_true) > 0) { # there are points in the canopy
    CanBDmax <- max(CBDval[idx_true]) # CBD max >CanopyHeightThreshold and CTH/3 and <CTH
    idmax <- idx_true[which(CBDval[idx_true] == CanBDmax)[1]] # lower id of max between max(CanopyHeightThreshold 1/3CTH) and height of CBDmax

    idx_true2 <- which(zval > CanopyHeightThreshold & zval < zval[idmax]) #>CanopyHeightThreshold and <idmax
    if (length(idx_true2) > 0) { # there is space between maxCBD and canopyHeightThreshold
      CanBDmin <- min(CBDval[idx_true2])
      if (!is.na(CanBDmin) & !is.na(CanBDmax) & CanBDmin < CanBDmax) { # normal configuration with a minimum below CanBDmax
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
  # we assume that below CBH and CBH2 we have shrub (when exists) (otherwise we keep the original localisation of FMAshrub below CanopyHeightThreshold)
  if (!is.na(CBH)) {
    CBDval[zval > CBH] <- pmin(PADval[zval > CBH] * FMAcan, bdmax) # max value at 1kg/m3
    CBDval[zval <= CBH] <- pmin(PADval[zval <= CBH] * FMAshrub, bdmax)
  } else if (!is.na(CBH2)) {
    CBDval[zval > CBH2] <- pmin(PADval[zval > CBH2] * FMAcan, bdmax) # max value at 1kg/m3
    CBDval[zval <= CBH2] <- pmin(PADval[zval <= CBH2] * FMAshrub, bdmax)
  }

  # 3. CDH canopy dominant height and CBDcan (4/12/2025)
  above_CDH <- (CBDval >= 0.1 * CanBDmax) & !is.na(CanBDmax) & !is.na(CBDval)
  CDH <- if (any(above_CDH)) max(zval[above_CDH]) else NA_real_ # max height with CBD>0.1*CanBDmax
  if (is.na(CDH)) {
    CDH <- CTH
  } # (nb missing data for CDH consist in 0.75m strata...)}

  # CBDcan
  height <- min(CDH, CTH)
  inCan <- (!is.na(CBH2) & zval >= CBH2 & zval <= height)
  CBDcan <- if (any(inCan)) sum(CBDval[inCan]) else 0

  # 4. no treshold for CBDval in cell k1 and k2, but a treatment will be applied afterwards (to correct bias - quantile mapping)
  TFL2 <- sum(CBDval[zval > zlow]) * dz
  FL_k1 <- CBDval[1] * dz
  FL_k2 <- CBDval[2] * dz

  # 5. CBHlow  between CanopyHeightThreshold and CTH/3
  # else { # we are in a special case where the CBH should be looking for in [CanopyHeightThreshold;CTH/3]
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
        CBHlow <- min(zval[above_CBH])
        CBD_cbhlow <- CBDval[zval == CBHlow][1]
      }
    }
  }
  if (!is.na(CBHlow)) {
    if (CBHlow <= zval_1mplus + 0.001) {
      CBHlow <- NA_real_
    }
  } # on remet à NA les CBHlow correspondant à maille 1.25, car pas intéressant

  # TODO consistency between CBH2 and CBHlow (CBHlow should not be larger than CBH2) à étudier
  # if (!is.na(CBHlow)&!is.na(CBH2)) {CBHlow}

  # 6. LadderTop, bot et charges
  # = NA;LadderBot= NA;

  # --- Bande du bas (LadderBot / LadderTop) ---
  # ladderMax = ifelse (is.na(CBH2),Inf,CBH2-dz) # now LadderTop to be lower than CBH2 when exist
  ladderMax <- ifelse(is.na(CBH2), Inf, CBH2) # change on 31/03/2026 now LadderTop can reach CBH2 when exist (but still<=)
  # ladderMax = ifelse (is.na(CBH2),ifelse (is.na(CBHlow),Inf,CBHlow-dz),CBH2-dz) # now LadderTop to be lower than CBH2/CBHlow
  # ifelse (is.na(CBHlow),CBH2-dz,CBHlow-dz)) # now LadderTop to be lower than CBH2/CBHlow

  # fonction interne qui calcule les seuils de ladderFuels pour différentes métriques
  .ladder_properties_for_threshold <- function(thr) {
    above <- (CBDval >= thr) & !is.na(CBDval) & zval > 1 & zval <= ladderMax # inclus le filtre zval>1 et CBH2-dz (4/12/2025)
    LadderBot <- LadderTop <- NA_real_
    LadderLoad <- SFLeq0 <- SFLeq_int <- 0
    idx_true <- which(above)
    if (length(idx_true) > 0) {
      i <- idx_true[1]
      LadderBot <- zval[i] - 0.5 * dz # -0.5dz added 31/03/2026 to get bottom of the layer (zval is cell-centered)
      j <- i
      while (j <= n && isTRUE(above[j])) j <- j + 1 # isTRUE évite NA
      LadderTop <- zval[j - 1] + 0.5 * dz # +0.5dz added 31/03/2026 to get upper of the layer (zval is cell-centered)
      # LadderLoad = sum(CBDval[c(i:j-1)]) * dz # initially load between the bot and top, now between 1m and top
      LadderLoad <- sum(CBDval[zval > zlow & zval <= LadderTop]) * dz
      # if(!is.na(CBH2)) { # NB this section was overwritten later so commented here (31/03/2026)
      #  # ladder fuel equivalent in terms of surface fuel for crowning
      #  SFLeq0 = M * LadderLoad * CBH2/(CBH2-0.5*LadderTop) # basic formulae as in Perrakis 2025
      #  k=zval>zlow & zval<=LadderTop
      #  SFLeq_int = M * sum(CBDval[k]*CBH2/(CBH2-zval[k]))*dz   # integrated formulae
      # }
    }
    c(LadderBot = LadderBot, LadderTop = LadderTop, LadderLoad = LadderLoad, SFLeq0 = SFLeq0, SFLeq_int = SFLeq_int)
  }
  # --- Ladders pour chaque seuil ---
  thresholds <- sort(unique(LadderFuelBDthresholds))
  # noms sûrs de colonnes (0.01 -> p010 ; 0.05 -> p050 ; 0.1 -> p100)
  suf <- gsub("\\.", "p", sprintf("%0.3f", thresholds))
  ladder_list <- lapply(thresholds, .ladder_properties_for_threshold)
  ladders <- unlist(ladder_list, use.names = FALSE)
  names(ladders) <- as.vector(rbind(
    paste0("LadderBot_", suf), paste0("LadderTop_", suf), paste0("LadderLoad_", suf),
    paste0("SFLeq0_", suf), paste0("SFLeq_intot_", suf)
  ))

  # ---  ProfTypes pour chaque seuil ---
  # fonction interne qui calcule les seuils de ladderFuels pour différentes métriques
  .profType_for_threshold <- function(thr) {
    suf <- gsub("\\.", "p", sprintf("%0.3f", thr))
    LadderTop <- ladders[paste0("LadderTop_", suf)]
    LadderBot <- ladders[paste0("LadderBot_", suf)]
    if (is.na(CBH2)) { # Type 1
      ProfType <- ifelse(is.na(CBHlow), 1.1, 1.2)
    } else { # Type>=2
      if (is.na(LadderBot)) { # no ladder fuel
        ProfType <- 2
      } else if (LadderBot <= (zval_1mplus + 0.001)) { # connected ladderfuel
        ProfType <- 3
      } else { # disconnected ladder fuel
        ProfType <- 4
      }
    }
    c(ProfType = ProfType)
  }
  ProfType_list <- lapply(thresholds, .profType_for_threshold)
  ProfTypes <- unlist(ProfType_list, use.names = FALSE)
  names(ProfTypes) <- as.vector(rbind(paste0("ProfType_", suf)))


  # compute ladder metrics ladderFuel0.02
  suf <- gsub("\\.", "p", sprintf("%0.3f", median(LadderFuelBDthresholds)))
  LadderTop <- ladders[paste0("LadderTop_", suf)]
  LadderBot <- ladders[paste0("LadderBot_", suf)]
  if (is.na(LadderTop)) {
    LFL <- 0
    SFLeq0 <- 0
    SFLeq_int <- 0
    #    k = which(zval>1)
    CFL <- sum(CBDval[zval > zlow]) * dz # Canopy fuel load kg/m2 (everything above 1m)
  } else {
    k <- which(zval > zlow & zval <= LadderTop) # between zlow=1m and LadderTop
    # }
    #  if (any(k)) {
    # kmax=k[length(k)]
    LFL <- sum(CBDval[k]) * dz # LadderFuelLoad in kg/m2
    if (!is.na(CBH2)) { # uncommented 31/03/2026
      # ladder fuel equivalent in terms of surface fuel for crowning
      SFLeq0 <- M * LFL * CBH2 / (CBH2 - 0.5 * LadderTop) # basic formulae as in Perrakis 2025
      SFLeq_int <- M * sum(CBDval[k] * CBH2 / (CBH2 - zval[k])) * dz # integrated formulae
    }
    # SFLeq0 = M * LFL * CBH2/(CBH2-0.5*zval[kmax]) # basic formulae as in Perrakis 2025
    # SFLeq_int = M * sum(CBDval[k]*CBH2/(CBH2-zval[k]))*dz   # integrated formulae
    CFL <- sum(CBDval[zval > LadderTop]) * dz # Canopy fuel load kg/m2 (everything above ladder top)  = TL2-LadderLoad0p020
    # } else {
    #  LFL=0;SFLeq0=0; SFLeq_int=0
    #  if (!is.na(CBH2)) {
    #    CFL = sum(CBDval[zval > CBH2])*dz
    #  } else  {CFL =0 }
  }


  # if (is.na(LadderTop)) {
  #    LFLlow = 0
  #  } else {
  #    k = which(zval>zlow & zval<=LadderBot)
  #    LFLlow = sum(CBDval[k])*dz #LadderFuelLoad in kg/m2
  #  }


  # tibble(!!!as.list(ladder_named),#LadderBot = LadderBot, LadderTop = LadderTop,
  out <- c(
    TFL2 = TFL2, FL_k1 = FL_k1, FL_k2 = FL_k2,
    ladders, ProfTypes, CTHs, CDH = CDH, CBH = CBH, CBH2 = CBH2, CBHlow = CBHlow,
    CanBDmax = CanBDmax, CanBDmin = CanBDmin, CanBDlowmax = CanBDlowmax, CanBDlowmin = CanBDlowmin,
    CBDcan = CBDcan, CBD_cbh = CBD_cbh, CBD_cbh2 = CBD_cbh2, CBD_cbhlow = CBD_cbhlow,
    LFL = LFL, CFL = CFL
  ) # SFLeq0 = SFLeq0, SFLeq_int = SFLeq_intLFLlow=LFLlow, )
  if (asTibble) {
    return(tibble::as_tibble_row(as.list(out)))
  } else {
    out
  }
}


#' @rdname ffuelmetrics2
#' @export
ffuelmetrics2Rast <- function(
  PAD_rast = NULL, # PAD values in m2/m3; either PAD or CBD should be provided
  CBD_rast = NULL, # CBD values in kg/m3
  FMAcan_rast = NULL, # fuel mass area in kg/m2 for canopy
  FMAshrub_rast = NULL, # fuel mass area in kg/m2 for shrub
  zval, # height values in m
  dz, # FP remove the default to avoid errors =0.5, # vertical resolution of the CBDval in m
  LadderFuelBDthresholds = c(0.01, 0.02, 0.05), # minimal bulk density thresholds for Understorey and Ladder fuels (kg/m3)
  CanopyHeightThreshold = 1, # default minimal value of height in m to look for CBH and CTH, typically 1 to 3 m
  CanopyBDTreshFrac = 0.1, # fraction threshold to combine CBDmin and CBDmax to look for CBH (0.5 is average, 0.3 is closer to CBDmin)
  CanopyTopBDThresholds = c(0.0012, 0.01), # minimal bulkDensity to look for CTH (canopy height but in terms of fuel<Height)
  CBHThresholdRatio = 1 / 5, # ratio of total height above which we look for CBH
  bdmax = 1.00, # max value of bulk density kg/m3 for canopy (above 1 m only)
  M = 2.5, # M factor from Perrakis 2025
  asTibble = FALSE
) {
  # préparation du PAD_rast
  if (is.null(PAD_rast)) {
    PAD_rast <- CBD_rast / FMAcan_rast # recomputation of PAD profile assuming a constant FMA has been applied to the whole profile
    # print(global(is.na(FMAcan_rast), sum))
    # print(global(!is.finite(FMAcan_rast), sum))
    # print(global(FMAcan_rast == 0, sum))
    # print(global(is.na(PAD_rast), sum))
  }
  PAD_FMA <- c(PAD_rast, FMAcan_rast, FMAshrub_rast) # on empile les FMA à PAD pour utiliser terra:app
  nPAD <- nlyr(PAD_rast)
  # calcul des metrics2 avec terra::app
  metrics2 <- terra::app(
    PAD_FMA,
    fun = function(v) { # v = valeurs pour un pixel donné, pour toutes les couches y compris les FMA
      ffuelmetrics2(
        PADval = v[1:nPAD], # profil PAD du pixel,
        FMAcan = v[nPAD + 1], # FMAcan
        FMAshrub = v[nPAD + 2], # FMAshrub
        zval = zval, dz = dz, LadderFuelBDthresholds = LadderFuelBDthresholds, CanopyHeightThreshold = CanopyHeightThreshold, CanopyBDTreshFrac = CanopyBDTreshFrac,
        CanopyTopBDThresholds = CanopyTopBDThresholds, bdmax = bdmax, M = M, asTibble = asTibble
      )
    }
  )
  # just to get names of outputs, because terra::app does not keep names
  name_template <- names(ffuelmetrics2(CBDval = rep(0, length(zval)), FMAcan = 1, FMAshrub = 1, zval = zval))
  names(metrics2) <- name_template
  return(metrics2)
}
