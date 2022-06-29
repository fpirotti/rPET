#'Calculate PET
#'
#'@description Calculate PET, Physiological Equivalent Temperature, check also https://comfort.cbe.berkeley.edu/
#'
#'@param Tair numeric, air temperature in degrees celsius, DEFAULT=21
#'@param Tmrt numeric, Mean radiant temperature in degrees celsius, DEFAULT=21
#'@param v_air numeric, air speed meters per second, DEFAULT=0.1
#'@param pvap numeric, vapour pressure as relative umidity in percentage, DEFAULT=21
#'@param M_activity numeric, metabolic rate (W/m²), DEFAULT=80 (about 1.4 met) see
#'    [https://en.wikipedia.org/wiki/Thermal_comfort#Metabolic_rate](https://en.wikipedia.org/wiki/Thermal_comfort#Metabolic_rate)
#'
#'Below some values (met) - (1 met = 58.2 W/m² )
#'
#' * Sleeping: 0.7 met,  (41 W/m²)
#' * Reclining: 0.8 met, (47 W/m²)
#' * Seated, quiet, Reading, seated, Writing: 1.0 (58.2 W/m²)
#' * Typing: 1.1 met, (64 W/m²)
#' * Standing, relaxed,  seated: 1.2 (70 W/m²)
#' * Walking slow: 1.4 met,  (81 W/m²)
#' * Driving a car: 1.5 met,  (87 W/m²)
#' * Walking medium speed: 1.7 met,  (99 W/m²)
#' * Walking 2mph (3.2km/h): 2.0 met,  (116 W/m²)
#' * Light machine work: 2.2 met,  (128 W/m²)
#' * Walking 3mph (4.8km/h): 2.6 met,  (151 W/m²)
#' * House cleaning: 2.7 met,  (157 W/m²)
#' * Driving, heavy vehicle: 3.2 met,  (186 W/m²)
#' * Dancing: 3.4 met,  (198 W/m²)
#' * Walking 4mph (6.4km/h): 3.8 met,  (221 W/m²)
#' * Heavy machine work: 4.0 met,  (233 W/m²)
#'
#' @param icl numeric, Clothing level, DEFAULT 0.9
#'
#' @return PET value
#' @export
#'
#' @examples
#' PETcorrected(Tair=21, Tmrt=21, v_air=0.1, pvap=50, M_activity=80, icl=0.9)
PETcorrected <- function(Tair=21, Tmrt=21, v_air=0.1, pvap=21, M_activity=80, icl=0.9 ){
  po <- 1013.25  # atmospheric pressure [hPa]
  p <- 1013.25  # real pressure [hPa]
  rob <- 1.06  # Blood density kg/L
  cb <- 3640.0  # Blood specific heat [j/kg/k]
  emsk <- 0.99  # Skin emissivity [-]
  emcl <- 0.95  # Clothes emissivity [-]
  Lvap <- 2.42 * 10.0^6.0  # Latent heat of evaporation [J/Kg]
  sigm <- 5.67 * 10.0^-8.0  # Stefan-Boltzmann constant [W/(m2*K^(-4))]
  cair <- 1010.0  # Air specific heat  [J./kg/K-]
  rdsk <- 0.79 * 10.0^7.0  # Skin diffusivity
  rdcl <- 0.0 # Clothes diffusivity

  pos <- 1
  age <- 35
  mbody <- 75 # Subject weight[kg]
  ht <- 1.80 # Subject size[m]
  Adu <- 0.203*mbody^0.425*ht^0.725 #Dubois body area
  bodyPosition<-"standing"
  feff <- 0.725
  sex <-"male"

  # Initialisation of the temperature set values
  tc_set<-36.6
  tsk_set<-34
  tbody_set<-0.1*tsk_set+0.9*tc_set


  systemp <- function(ta = 23,
                     tmrt = 22,
                     pvap = 80,
                     v_air = 0.1,
                     M = 80,
                     Icl = 0.9) {
    vpa <- pvap # pvap==RH relative humidity
    # Area parameters of the body: #
    if (Icl < 0.03) {
      Icl <- 0.02
    }

    icl <- Icl  # [clo] Clothing level
    eta <- 0.0 # Body efficiency
    # Calculation of the Burton coefficient, k <- 0.31 for Hoeppe:
    fcl <-
      1 + (0.31 * icl) # Increasment of the exchange area depending on the clothing level:
    if (bodyPosition == "sitting") {
      feff <- 0.696
    } else if (bodyPosition == "standing") {
      feff <- 0.725
    }  else if (bodyPosition == "crouching") {
      feff <- 0.67
    } else {
      feff <- 0.725
    }
    facl <-
      (173.51 * icl - 2.36 - 100.76 * icl * icl + 19.28 * `^`(icl, 3.0)) / 100.0

    # Basic metabolism for men and women in [W/m2] #
    # Attribution of internal energy depending on the sex of the subject
    if (sex == "male") {
      met_base <-
        3.45 * `^`(mbody, 0.75) * (1.0 + 0.004 * (30.0 - age) + 0.01 * (ht * 100.0 / `^`(mbody, 1.0 / 3.0) - 43.4))
    } else{
      met_base <-
        3.19 * `^`(mbody, 0.75) * (1.0 + 0.004 * (30.0 - age) + 0.018 * (ht * 100.0 / `^`(mbody, 1.0 / 3.0) - 42.1))
    } # Source term : metabolic activity
    he <- M + met_base
    h <- he * (1.0 - eta)

    # Respiratory energy losses #
    # Expired air temperature calculation:
    texp <- 0.47 * ta + 21.0

    # Pulmonary flow rate
    rtv <- he * 1.44 * `^`(10.0, -6.0)

    # Sensible heat energy loss:
    Cres <- cair * (ta - texp) * rtv

    # Latent heat energy loss:
    vpexp <-
      6.11 * `^`(10.0, 7.45 * texp / (235.0 + texp)) # Partial pressure of the breathing air
    Eres <- 0.623 * Lvap / p * (vpa - vpexp) * rtv
    # total breathing heat loss
    qresp <- (Cres + Eres)

    c <- rep(0, 11)
    tcore <- rep(0, 7)  # Core temperature list
    hc <- 2.67 + 6.5 * `^`(v_air, 0.67) #Convection coefficient
    hc <- hc * `^`(p / po, 0.55) # Correction with pressure

    # Clothed fraction of the body approximation #
    rcl <- icl / 6.45 # conversion in m2.K/W
    y <- 0
    if (facl > 1.0) {
      facl <- 1.0
    }
    rcl <- icl / 6.45 # conversion clo --> m2.K/W
    # y : equivalent clothed height of the cylinder
    # High clothing level : all the height of the cylinder is covered
    if (icl >= 2.0) {
      y <- 1.0
    }
    if (icl > 0.6 && icl < 2.0) {
      y <- (ht - 0.2) / ht
    }
    if (icl <= 0.6 && icl > 0.3) {
      y <- 0.5
    }
    if (icl <= 0.3 && icl > 0.0) {
      y <- 0.1
    }
    # calculation of the closing radius depending on the clothing level (6.28 <- 2* pi !)
    r2 <-
      Adu * (fcl - 1.0 + facl) / (6.28 * ht * y)  # External radius
    r1 <- facl * Adu / (6.28 * ht * y)  # Internal radius
    di <- r2 - r1
    # clothed surface
    Acl <- Adu * facl + Adu * (fcl - 1.0)
    # skin temperatures
    for (j in 1:7) {
      tsk <- tsk_set
      count1 <- 0
      tcl <-
        (ta + tmrt + tsk) / 3.0 # Average value between the temperatures to estimate Tclothes
      enbal2 <- 0.0
      while (T) {
        for (count2 in 1:100) {
          # Estimation of the radiation losses
          rclo2 <-
            emcl * sigm * ((`^`(tcl + 273.2, 4.0)) - (`^`(tmrt + 273.2, 4.0))) * feff
          # Calculation of the thermal resistance of the body:
          htcl <-
            (6.28 * ht * y * di) / (rcl * `log`(r2 / r1) * Acl)
          tsk <-
            (hc * (tcl - ta) + rclo2) / htcl + tcl  # Skin temperature calculation

          # Radiation losses #
          Aeffr <-
            Adu * feff  # Effective radiative area depending on the position of the subject
          # For bare skin area:
          rbare <-
            Aeffr * (1.0 - facl) * emsk * sigm * (`^`(tmrt + 273.2, 4.0) - `^`(tsk + 273.2, 4.0))
          # For dressed area:
          rclo <-
            feff * Acl * emcl * sigm * (`^`(tmrt + 273.2, 4.0) - `^`(tcl + 273.2, 4.0))
          rsum <- rbare + rclo #[W]

          # Convection losses #
          cbare <- hc * (ta - tsk) * Adu * (1.0 - facl)
          cclo  <- hc * (ta - tcl) * Acl
          csum  <- cbare + cclo  #[W]

          # Calculation of the Terms of the second order polynomial :
          K_blood <- Adu * rob * cb
          c[[1]] <- h + qresp
          c[[3]] <- tsk_set / 2 - 0.5 * tsk
          c[[4]] <- 5.28 * Adu * c[[3]]
          c[[5]] <- 13.0 / 625.0 * K_blood
          c[[6]] <- 0.76275 * K_blood
          c[[7]] <-  c[[4]] - c[[6]] - tsk * c[[5]]
          c[[8]] <- -c[[1]] * c[[3]] - tsk * c[[4]] + tsk * c[[6]]
          c[[10]] <-
            5.28 * Adu - 0.76275 * K_blood -  13.0 / 625.0 * K_blood * tsk
          # discriminant #1 (b^2 - 4*a*c)
          c[[11]] <-
            `^`((5.28 * Adu - 0.76275 * K_blood -  13.0 / 625.0 * K_blood * tsk),
                2) - 4.0 * c[[5]] * (c[[6]] * tsk - c[[1]] - 5.28 * Adu * tsk)
          # discriminant #2 (b^2 - 4*a*c)
          c[[9]] <- c[[7]] * c[[7]] - 4.0 * c[[5]] * c[[8]]
          if (tsk == tsk_set) {
            tsk <- tsk_set + 0.01
          }
          #Calculation of Tcore[]:
          # case 7 : Set blood flow only
          tcore[[7]] <-
            (h + qresp) / (5.28 * Adu + K_blood * 6.3 / 3600.0) + tsk
          # cas 3 : Set blood flow + regulation
          tcore[[3]] <-
            (h + qresp) / (5.28 * Adu + K_blood * 6.3 / 3600.0 / (1.0 + 0.5 * (tsk_set - tsk))) +
            tsk
          # case 4 : Maximum blood flow only
          tcore[[4]] <-
            c[[1]] / (5.28 * Adu + K_blood * 1.0 / 40.0) + tsk #  max flow <- 90 [L/m2/h]/3600 <=> 1/40
          # Roots calculation #1
          if (c[[11]] >= 0.0) {
            tcore[[6]] <-
              (-c[[10]] - `^`(c[[11]], 0.5)) / (2.0 * c[[5]]) # Numerical safety to avoid negative roots
            tcore[[1]] <-
              (-c[[10]] + `^`(c[[11]], 0.5)) / (2.0 * c[[5]])
          }
          # Roots calculation #2
          if (c[[9]] >= 0.0) {
            tcore[[2]] <- (-c[[7]] + `^`(abs(c[[9]]), 0.5)) / (2.0 * c[[5]])
            tcore[[5]] <-
              (-c[[7]] - `^`(abs(c[[9]]), 0.5)) / (2.0 * c[[5]])
          }

          # Calculation of sweat losses  #
          tbody <- 0.1 * tsk + 0.9 * tcore[[j]]
          # Sweating flow calculation
          swm <- 304.94 * (tbody - tbody_set) * Adu / 3600000.0
          # Saturation vapor pressure at temperature Tsk and for 100% HR
          vpts <- 6.11 * `^`(10.0, 7.45 * tsk / (235.0 + tsk))
          if (tbody <= tbody_set) {
            swm <- 0.0
          }
          if (sex == 2) {
            swm <- 0.7 * swm
          }
          esweat <- -swm * Lvap
          hm <-
            0.633 * hc / (p * cair) # Evaporation coefficient [W/(m^2*Pa)]
          fec <- 1.0 / (1.0 + 0.92 * hc * rcl)
          emax <-
            hm * (vpa - vpts) * Adu * Lvap * fec # Max latent flux
          wetsk <- esweat / emax # skin wettedness
          # esw: Latent flux depending on w [W.m-2]
          if (wetsk > 1.0) {
            wetsk <- 1.0
          }
          eswdif <-
            esweat - emax # difference between sweating and max capacity
          if (eswdif <= 0.0) {
            esw <- emax
          }
          if (eswdif > 0.0) {
            esw <- esweat
          }
          if (esw > 0.0) {
            esw <- 0.0
          }
          ed <-
            Lvap / (rdsk + rdcl) * Adu * (1.0 - wetsk) * (vpa - vpts) # diffusion heat flux

          vb1 <-
            tsk_set - tsk # difference for the volume blood flow calculation
          vb2 <- tcore[[j]] - tc_set #  idem
          if (vb2 < 0.0) {
            vb2 <- 0.0
          }
          if (vb1 < 0.0) {
            vb1 <- 0.0
          }
          # Calculation of the blood flow depending on the difference with the set value
          vb <- (6.3 + 75 * vb2) / (1.0 + 0.5 * vb1)
          # energy balance MEMI modele
          enbal <- h + ed + qresp + esw + csum + rsum
          # clothing temperature
          if (count1 == 0)
            xx <- 1.0
          if (count1 == 1)
            xx <- 0.1
          if (count1 == 2)
            xx <- 0.01
          if (count1 == 3)
            xx <- 0.001
          if (enbal > 0.0)
            tcl <- tcl + xx
          if (enbal < 0.0)
            tcl <- tcl - xx
          if ((enbal > 0.0 ||
               enbal2 <= 0.0) && (enbal < 0.0 || enbal2 >= 0.0)) {
            enbal2 <- enbal
            count2 <- count2 + 1
          }  else {
            break
          }
        }
        if (count1 == 0.0 || count1 == 1.0 || count1 == 2.0) {
          count1 <- count1 + 1
          enbal2 <- 0.0
        } else{
          break
        }
      }# end "While TRUE" (using 'break' statements)

      for (k in 1:20) {
        g100 <- 0
        if (count1 == 3.0 && (j != 2 && j != 5)) {
          if (j != 6 && j != 1) {
            if (j != 3) {
              if (j != 7) {
                if (j == 4) {
                  g100 <- TRUE
                  break
                }
              }
              else {
                if (tcore[[j]] >= tc_set || tsk <= tsk_set) {
                  g100 <- FALSE
                  break
                }
                g100 <- TRUE
                break
              }
            }
            else{
              if (tcore[[j]] >= tc_set || tsk > tsk_set) {
                g100 <- FALSE
                break
              }
              g100 <- TRUE
              break
            }
          }
          else{
            if (c[[11]] < 0.0 || (tcore[[j]] < tc_set || tsk <= 33.85)) {
              g100 <- FALSE
              break
            }
            g100 <- TRUE
            break
          }
        }
        if (c[[9]] < 0.0 ||
            (tcore[[j]] < tc_set || tsk > tsk_set + 0.05)) {
          g100 <- FALSE
          break
        }
      }
      if (g100 == FALSE) {
        next
      }
      else{
        if ((j == 4 || vb < 91.0) && (j != 4 || vb >= 89.0)) {
          # Maximum blood flow
          if (vb > 90.0) {
            vb <- 90.0
          }
          # water loss in g/m2/h
          ws <- swm * 3600.0 * 1000.0
          if (ws > 2000.0)
            ws <- 2000.0
          wd <- ed / Lvap * 3600.0 * (-1000.0)
          wr <- Eres / Lvap * 3600.0 * (-1000.0)
          wsum <- ws + wr + wd
          return( list(tc=tcore[[j]], tsk=tsk, tcl=tcl, esw=esw))

        }
        # water loss
        ws <- swm * 3600.0 * 1000.0 # sweating
        wd <-
          ed / Lvap * 3600.0 * (-1000.0) # diffusion <- perspiration
        wr <- Eres / Lvap * 3600.0 * (-1000.0) # respiration latent
        wsum <- ws + wr + wd
        if (j - 3 < 1) {
          index <- 4
        } else{
          index <- j - 3
        }

        return( list(tc=tcore[[index]], tsk=tsk, tcl=tcl, esw=esw))

      }

      warning("Should not arrive here")
    }
    warning("Should not arrive here2")
  }



  pet <- function(tc,tsk,tcl,ta_init, esw_real){
    # Input variables of the PET reference situation:
    icl_ref<- 0.9 # clo
    M_activity_ref <- 80 # W
    v_air_ref <- 0.1 # m/s
    vpa_ref <- 12 # hPa
    icl <- icl_ref

    tx  <-  ta_init
    tbody <- 0.1*tsk+0.9*tc
    enbal2  <-  0.0
    count1  <-  0

    # base metabolism
    if (sex == "male"){
      met_base  <-  3.45 * `^`(mbody, 0.75) * (1.0 + 0.004 * (30.0 - age) + 0.01 * (ht * 100.0 / `^`(mbody, 1.0 / 3.0) - 43.4))
    } else{
      met_base  <-  3.19 * `^`(mbody, 0.75) * (1.0 + 0.004 * (30.0 - age) + 0.018 * (ht * 100.0 / `^`(mbody, 1.0 / 3.0) - 42.1))
    }
    # breathing flow rate
    rtv_ref  <-  (M_activity_ref + met_base) * 1.44 * `^`(10.0, -6.0)

    swm  <-  304.94 * (tbody - tbody_set) * Adu / 3600000.0 #sweating flow rate
    vpts  <-  6.11 * `^`(10.0, 7.45 * tsk / (235.0 + tsk)) # saturated vapour pressure at skin surface
    if( tbody <= tbody_set){
      swm  <-  0.0
    }
    if( sex == "female"){
      swm  <-  swm*0.7
    }
    esweat  <-  -swm * Lvap
    esweat <- esw_real
    # standard environment
    hc  <-  2.67 + 6.5 * `^`(v_air_ref, 0.67)
    hc  <-  hc * `^`(p/po, 0.55)
    # radiation saldo
    Aeffr  <-  Adu * feff
    facl  <-  (173.51 * icl - 2.36 - 100.76 *icl*icl + 19.28 * `^`(icl, 3.0)) / 100.0
    if (facl > 1.0){
      facl  <-  1.0
    }
    # Increase of the exchange area depending on the clothing level
    fcl  <-  1 + (0.31 * icl)
    Acl  <-  Adu * facl + Adu * (fcl - 1.0)
    hm  <-  0.633 * hc / (p * cair) # Evaporation coefficient [W/(m^2*Pa)]
    fec  <-  1.0 / (1.0 + 0.92 * hc * 0.155*icl_ref) # vapour transfer efficiency for reference clothing
    emax  <-  hm * (vpa_ref - vpts) * Adu * Lvap * fec # max latetn flux for the reference vapour pressure 12 hPa
    wetsk  <-  esweat / emax
    # skin wettedness
    if (wetsk > 1.0){
      wetsk  <-  1.0
    }
    eswdif  <-  esweat - emax
    # diffusion
    ediff  <-  Lvap / (rdsk + rdcl) * Adu * (1.0 - wetsk) * (vpa_ref-vpts)
    # esw: sweating [W.m-2] from the actual environment : in depends only on the difference with the core set temperature
    if( eswdif <= 0.0){
      esw  <-  emax
    }
    if( eswdif > 0.0){
      esw  <-  esweat
    }
    if (esw > 0.0){
      esw  <-  0.0
    }
    while( count1 != 4){
      rbare  <-  Aeffr * (1.0-facl)*emsk*sigm*(`^`(tx + 273.2, 4.0) - `^`(tsk + 273.2, 4.0))
      rclo  <-  feff * Acl * emcl * sigm*(`^`(tx + 273.2, 4.0) - `^`(tcl + 273.2, 4.0))
      rsum  <-  rbare + rclo # Recalculation of the radiative losses
      # convection
      cbare  <-  hc * (tx - tsk) * Adu * (1.0 - facl)
      cclo  <-  hc * (tx - tcl) * Acl
      csum  <-  cbare + cclo # Recalculation of the convective losses
      # breathing
      texp  <-  0.47 * tx + 21.0
      Cres  <-  cair * (tx - texp) * rtv_ref
      vpexp  <-  6.11 * `^`(10.0, 7.45 * texp / (235.0 + texp))
      Eres  <-  0.623 * Lvap / p * (vpa_ref - vpexp) * rtv_ref
      qresp  <-  (Cres + Eres)
      # ----------------------------------------
      # energy balance
      enbal  <-  (M_activity_ref + met_base) + ediff + qresp + esw + csum + rsum
      if (count1 == 0)
        xx <- 1.0
      if (count1 == 1)
        xx <- 0.1
      if (count1 == 2)
        xx <- 0.01
      if (count1 == 3)
        xx <- 0.001

      if( enbal > 0.0){
        tx  <-  tx - xx
      }
      if( enbal < 0.0){
        tx <- tx+xx
      }
      if( (enbal > 0.0 || enbal2 <= 0.0) && (enbal < 0.0 || enbal2 >= 0.0)){
        enbal2  <-  enbal
      }  else{
        count1  <-  count1 + 1
      }
    }
    return( list(tsk=tsk, enbal=enbal, esw=esw, ediff=ediff, PET=tx, tcl=tcl) )
  }


  s<-  systemp(Tair, Tmrt, pvap, v_air, M_activity, icl)
  pet <-  pet(s$tc, s$tsk, s$tcl, Tair, s$esw)
  pet
}






