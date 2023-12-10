#' @title Kelvin to Celsius conversion
#' @description convert temperature in Kelvin (K) into degree Celsius (°C)
#' @param K temperature in Kelvin (K)
#' @return numeric temperature in degree Celsius (°C)
#' @seealso \code{\link{C2K}}.
#' @author Jun Cai (\email{cai-j12@@mails.tsinghua.edu.cn}), PhD candidate from
#' Department of Earth System Science, Tsinghua University
#' @export
#' @examples
#' K2C(0)
K2C <- function(K) {
  # check parameter
  stopifnot(is.numeric(K))

  return(K - T0)
}

#' @title Celsius to Kelvin conversion
#' @description convert temperature in degree Celsius (°C) into Kelvin (K)
#' @param C temperature in degree Celsius (°C)
#' @return numeric temperature in Kelvin (K)
#' @seealso \code{\link{K2C}}.
#' @author Jun Cai (\email{cai-j12@@mails.tsinghua.edu.cn}), PhD candidate from
#' Department of Earth System Science, Tsinghua University
#' @export
#' @examples
#' T0 # absolute zero in Kelvin (K)
#' C2K(T0)
C2K <- function(C) {
  # check parameter
  stopifnot(is.numeric(C))

  return(C + T0)
}
#' @title Absolute zero
#' @description  \href{https://en.wikipedia.org/wiki/Absolute_zero}{Absolute zero} in Kelvin \eqn{T_0} (K)
#' @export
T0 <- 273.15

#' @title Saturation vapor pressure at absolute zero (hPa)
#' @description  \eqn{e_s(T_0) = 6.11hPa} is the saturation vapor pressure at the absolute zero \eqn{T_0 = 273.15K}.
#' @seealso \code{\link{T0}}
#' @export
Es.T0 <- 6.11

#' Specific gas constant of water vapor
#' @description  \href{https://en.wikipedia.org/wiki/Gas_constant#Specific_gas_constant}{Specific gas constant} of water vapor \eqn{R_w = \frac{1000R}{M_w} = 461.52J/(kgK)}, where \eqn{R = 8.3144621J/(molK)} is the molar \href{https://en.wikipedia.org/wiki/Gas_constant}{gas constant} and \eqn{M_w = 18.01528g/mol} is the molecular weight of water vapor.
#' @seealso \code{\link{Mw}}
#' @export
Rw <- 461.52

#' Molecular weight of water vapor
#' @description  \href{https://en.wikipedia.org/wiki/Molar_mass}{Molecular weight} of water vapor \eqn{M_w = 18.01528g/mol}
#' @seealso \code{\link{Md}}
#' @export
Mw <- 18.01528

#' Molecular weight of dry air
#' @description  Molecular weight of dry air \eqn{M_d = 28.9634g/mol}
#' @seealso \code{\link{Mw}}
#' @export
Md <- 28.9634

#' Latent heat of water vapor
#' @description  \href{https://en.wikipedia.org/wiki/Latent_heat}{Latent heat} of water vapor \eqn{L = 2.5 \times 10^6J/kg}
#' @export
L <- 2.5e6
#' @title calculate saturation vapor pressure using the Murray equation
#' @description calculate saturation vapor pressure \eqn{E_s} at temperature \eqn{t}, per the equation proposed by Murray (1967).
#' @param t temperature in Kelvin (K)
#' @return numeric saturation vapor pressure in hectopascal (hPa) or millibar (mb)
#' @references Murray, F. W. (1967). \emph{On the Computation of Saturation Vapor Pressure}. Journal of Applied Meteorology, 6(1), 203-204.
#' @author Jun Cai (\email{cai-j12@@mails.tsinghua.edu.cn}), PhD candidate from
#' Department of Earth System Science, Tsinghua University
#' @export
#' @examples
#' T0 # absolute zero in Kelvin (K)
#' SVP.Murray(T0)
SVP.Murray <- function(t) {
  # check parameter
  stopifnot(is.numeric(t))
  a <- rep(17.2693882, length(t) )
  b <- rep(35.86, length(t) )
  # if (t < T0) {
    a[t < T0] <- 21.8745584
    b[t < T0] <- 7.66
  # }
  Es <- 6.1078 * exp(a * (t - T0) / (t - b))
  return(Es)
}

#' @title calculate saturation vapor pressure using the Clausius-Clapeyron equation
#' @description calculate saturation vapor pressure \eqn{E_s} at temperature \eqn{t}, using the Clausius-Clapeyron equation.
#' @param t temperature in Kelvin (K)
#' @return numeric saturation vapor pressure in hectopascal (hPa) or millibar (mb)
#' @references Shaman, J., & Kohn, M. (2009). \emph{Absolute humidity modulates influenza survival, transmission, and seasonality}. Proceedings of the National Academy of Sciences, 106(9), 3243-3248.
#'
#' Wallace, J. M., & Hobbs, P. V. (2006). \emph{Atmospheric science: an introductory survey} (Vol. 92). Academic press.
#' @author Jun Cai (\email{cai-j12@@mails.tsinghua.edu.cn}), PhD candidate from
#' Department of Earth System Science, Tsinghua University
#' @export
#' @examples
#' T0 # absolute zero in Kelvin (K)
#' SVP.ClaCla(T0)
SVP.ClaCla <- function(t) {
  # check parameter
  stopifnot(is.numeric(t))

  Es <- Es.T0 * exp((L / Rw) * (1 / T0 - 1 / t))
  return(Es)
}
