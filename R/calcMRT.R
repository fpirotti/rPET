#' calcMRT
#' @description calculates mean radiant temperature from air temperature
#'              and
#'
#' @param Ta numeric, Air temperature in °C, DEFAULT=21
#' @param Tg numeric, globe temperature in °C, DEFAULT=21
#' @param WV numeric, air velocity at the level of the globe in meters per second,
#' DEFAULT=0.1
#' method  is from globe temperature but is very sensible to wind speed (wind=0 the MRT >>>100)
#'
#'
#' @return mean radiant temperature in °C
#' @export
#'
#' @examples
#' Ta=30; WV=2.681666667;
#' #Tg <- calcGT(Ta=Ta, Td=20.556, WV=WV, S=336, Fdiff=0.3333, Fd=0.6666, Z=38.44, P=992.83 )
#' #calcMRT(Tg=21, WV=0, Ta=21 )
calcMRT <- function(Tg=21, WV=0.1, Ta=21) {
  a<-((Tg  +  273.15)^4+ 2.5E8 *(WV^0.6)*(Tg-Ta))^0.25-273.15
  a
}
#' calcMRT2
#' @description calculates mean radiant temperature estimating globe temperature
#'
#' @param Ta numeric, Air temperature (°C)
#' @param Td numeric, dew point temperature (°C)
#' @param WV numeric, wind speed (meters per second)
#' @param S numeric, solar radiation (Watts per meter squared)
#' @param Fdiff numeric, diffuse solar radiation (Watts per meter squared)
#' @param Fd  numeric, direct solar radiation (Watts per meter squared)
#' @param Z  numeric, zenith angle of sun (degrees)
#' @param P  numeric, pressure in millibars
#' per second , DEFAULT=0
#'
#' @return mean radiant temperature in °C
#' @export
#'
#' @examples
#' Ta=30; WV=2.681666667;
#'
calcMRT2 <- function(Ta, Td , WV, S, Fdiff, Fd, Z, P ) {
  Tg <- calcGT(Ta, Td , WV, S, Fdiff, Fd, Z, P )
   ((Tg  +  273.15)^4+ 2.5E8 *(WV^0.6)*(Tg-Ta))^0.25-273.15
}

#' calcMRT3
#' @description calculates mean radiant temperature using method from
#' https://link.springer.com/content/pdf/10.1007/s00484-020-01900-5.pdf
#'
#' @param Ta numeric, Air temperature (°C)
#' @param Td numeric, dew point temperature (°C)
#' @param WV numeric, wind speed (meters per second)
#' @param S numeric, solar radiation (Watts per meter squared)
#' @param Fdiff numeric, diffuse solar radiation (Watts per meter squared)
#' @param Fd  numeric, direct solar radiation (Watts per meter squared)
#' @param Z  numeric, zenith angle of sun (degrees)
#' @param P  numeric, pressure in millibars
#' per second , DEFAULT=0
#'
#' @return mean radiant temperature in °C
#' @export
#'
#' @examples
#' Ta=30; WV=2.681666667;
#'
calcMRT3 <- function(Ta, Td , WV, S, Fdiff, Fd, Z, P ) {
  Tg <- calcGT(Ta, Td , WV, S, Fdiff, Fd, Z, P )
  ((Tg  +  273.15)^4+ 2.5E8 *(WV^0.6)*(Tg-Ta))^0.25-273.15
}

#' calcGT
#' @description calculates Globe Temperature (Tg)
#'
#' @param Ta numeric, Air temperature (°C)
#' @param Td numeric, dew point temperature (°C)
#' @param WV numeric, wind speed (meters per second)
#' @param S numeric, solar radiation (Watts per meter squared)
#' @param Fdiff numeric, diffuse solar radiation (Watts per meter squared)
#' @param Fd  numeric, direct solar radiation (Watts per meter squared)
#' @param Z  numeric, zenith angle of sun (degrees)
#' @param P  numeric, pressure in millibars
#'
#' @return globe temperature (°C)
#' @export
#'
#' @examples
#' # From APPENDIX of Dimiceli, V.E.; Piltz, S.F.; Amburn, S.A.
#' # Estimation of Black Globe Temperature for Calculation of the
#' # Wet Bulb Globe Temperature Index.
#' # In Proceedings of the Proceedings of the World Congress on Engineering
#' # and Computer Science; 2011; Vol. II.
#' gt1 <- calcGT(Ta=30, Td=20.556, WV=2.681666667, S=336, Fdiff=0.3333, Fd=0.6666, Z=38.44, P=992.83 )
#' gt2 <- calcGT(Ta=33.889,  Td=24.444, WV=11263/3600, S=754, Fdiff=0.25, Fd=0.75, Z=36.65, P=981.94 )
calcGT <- function(Ta, Td , WV, S=336, Fdiff, Fd, Z, P ) {
  if( abs((Fdiff+Fd)-1)>1E-1  ){
    warning("the fraction of direct and diffuse solar radiation
            (Fdiff + Fd) should sum to 1.0")
  }
  Z<- Z*pi/180
  C <- (0.315*(WV*3600)^0.58) / 5.3865E-8
  VP<-  exp( (17.67*(Td-Ta))/(Td+243.5) ) * (1.0007 + 0.00000346*P) * 6.112*exp(  (17.502*Ta)/(240.97+Ta)  )
  Ea <- 0.575*VP^(1/7)
  B <- S*( Fd/(4*5.670374E-8*cos(Z)) + ( (1.2/5.670374E-8) * Fdiff ) ) + Ea*Ta^4
  (B+C*Ta+7680000)/(C+256000)
}


