#' Calculate PET
#'
#' @description Calculate PET
#'
#' @param Tair numeric DEFAULT 21
#' @param Tmrt numeric DEFAULT 21
#' @param v_air numeric DEFAULT 0.1
#' @param pvap numeric DEFAULT 21
#' @param M_activity numeric DEFAULT 80
#' @param icl numeric DEFAULT 0.9
#'
#' @return PET value
#' @export
#'
#' @examples
#' PETcorrected(1,2)
PETcorrected <- function(Tair, Tmrt, v_air, pvap, M_activity, icl ){
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

  sex <- 1
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
}
