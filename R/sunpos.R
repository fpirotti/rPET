
#' sunpos
#' @description Returns a matrix of azimuth and zenith angles of the sun given the
#' unit vectors from the observer to the direction of the sun.
#'
#' @param sunv coordinates x, y, z of the unit vector in the direction
#' of the sun.
#'
#' @author Javier G. Corripio
#'
#' @return A matrix of azimuth and zenith angles in degrees.
#' @export
#'
#' @examples
#'  sunpos(matrix( (1:6)/6, nrow=2, ncol=3))
sunpos <-   function (sunv)
{
  if (nargs() < 1) {
    cat("USAGE: sunpos(sunvector)\n 3D vector\n")
    return()
  }
  azimuth = degrees(pi - atan2(sunv[, 1], sunv[, 2]))
  zenith = degrees(acos(sunv[, 3]))
  return(cbind(azimuth, zenith))
}

#' radians
#' @description Converts angles to radians from degrees
#'
#' @param degree angle in degrees
#'
#' @author Javier G. Corripio
#'
#' @return angle in radians.
#' @export
#'
#' @examples
#'  radians(90)
radians <- function (degree)
{
  radian = degree * (pi/180)
  return(radian)
}


#' degrees
#' @description Converts angles from radians to degrees
#'
#' @param radian angle in radians
#'
#' @author Javier G. Corripio
#'
#' @return angle in degrees
#' @export
#'
#' @examples
#'  degrees(seq(0,2*pi,pi/2))
degrees <- function (radian)
{
  degree = radian * (180/pi)
  return(degree)
}


#' sunvector
#' @description Calculates a unit vector in the direction of the sun from
#' the observer position.
#'
#'
#' @param jd Julian Day and decimal fraction.
#' @param latitude Latitude of observer in degrees and decimal fraction.
#' @param longitude	 Longitude of observer in degrees and decimal fraction.
#' @param timezone	 Time zone, west is negative.
#'
#' @details To calculate the sunvector to the nearest hour, give the Julian Day
#'  with a precission better than 1/24; to approximate it to the nearest minute
#'  use decimal fractions smaller than 1/(24*60), and so on.
#'
#' @author Javier G. Corripio
#'
#' @return 3 column matrix with the x, y , z coordinates of the sun vector.
#' @export
#'
#' @examples
#'  sunvector(JD(Sys.time()),51.4778,-0.0017,0)
sunvector <- function (jd, latitude, longitude, timezone)
{
  if (nargs() < 4) {
    cat("USAGE: sunvector(jd,latitude,longitude,timezone)\n values in jd, degrees, hours\n")
    return()
  }
  omegar = hourangle(jd, longitude, timezone)
  deltar = radians(declination(jd))
  lambdar = radians(latitude)
  svx = -sin(omegar) * cos(deltar)
  svy = sin(lambdar) * cos(omegar) * cos(deltar) - cos(lambdar) *
    sin(deltar)
  svz = cos(lambdar) * cos(omegar) * cos(deltar) + sin(lambdar) *
    sin(deltar)
  return(cbind(svx, svy, svz))
}



#' JD
#' @description Computes Julian Day from dates as POSIXct object.
#'
#' @usage JD(x, inverse=FALSE)
#'
#' @param x	 POSIXct object.
#' @param inverse Logical. If false (default) returns the Julian Days
#' corresponding to given dates. If TRUE returns the date corresponding
#' to input Julian days
#'
#' @details Class "POSIXct" represents the (signed) number of seconds since the
#' beginning of 1970 (in the UTC timezone) as a numeric vector, and Julian Day is
#' the number of days since January 1, 4713 BCE at noon UTC, so the Julian Day
#' is calculated as numeric(POSIXct)+2440587.5 days.
#'
#' @author Javier G. Corripio
#'
#' @return Julian Day
#' @export
#'
#' @examples
#'  JD(Sys.time())
JD <- function (x, inverse = FALSE)
{
  if (inverse) {
    return(as.POSIXct((x - 2440587.5) * 86400, origin = ISOdate(1970,
                                                                1, 1, 0, 0, 0), format = "%Y-%m-%d %H:%M:%S"))
  }
  else {
    return(as.numeric(x)/86400 + 2440587.5)
  }
}




#' hourangle
#' @description Hour angle, internal function for solar position.
#'
#' @usage hourangle(jd, longitude, timezone)
#'
#' @param jd	Julian Day.
#' @param longitude	 Longitude.
#' @param timezone	 Timezone in hours, west of Greenwich is negative.
#'
#'
#' @author Javier G. Corripio
#'
#' @return Hour angle
#' @export
#'
hourangle <- function (jd, longitude, timezone)
{
  if (nargs() < 3) {
    cat("USAGE: hourangle(jd,longitude,timezone)\n julian day, degrees, hours. Return radians \n")
    return()
  }
  hour = ((jd - floor(jd)) * 24 + 12)%%24
  eqtime = eqtime(jd)
  stndmeridian = timezone * 15
  deltalontime = longitude - stndmeridian
  deltalontime = deltalontime * 24/360
  omegar = pi * (((hour + deltalontime + eqtime/60)/12) -
                   1)
  return(omegar)
}



#' declination
#' @description Computes the declination of the Sun for a given Julian Day.
#'
#' @usage declination(jd)
#'
#' @param jd	Julian Day.
#'
#' @author Javier G. Corripio
#'
#' @return Declination in degrees and decimal fraction
#' @export
#'
#' @examples
#'  declination(JDymd(2019,1,1))
declination  <- function (jd)
{
  if (nargs() < 1) {
    cat("USAGE: declination(jd) \n jd = Julian day \n")
    return()
  }
  T = (jd - 2451545)/36525
  epsilon = (23 + 26/60 + 21.448/3600) - (46.815/3600) * T -
    (0.00059/3600) * T^2 + (0.001813/3600) * T^3
  L0 = 280.46645 + 36000.76983 * T + 0.0003032 * T^2
  M = 357.5291 + 35999.0503 * T - 0.0001559 * T^2 - 4.8e-07 *
    T^3
  e = 0.016708617 - 4.2037e-05 * T - 1.236e-07 * T^2
  C = (1.9146 - 0.004817 * T - 1.4e-05 * T^2) * sin(radians(M)) +
    (0.019993 - 0.000101 * T) * sin(2 * radians(M)) + 0.00029 *
    sin(3 * radians(M))
  Theta = L0 + C
  v = M + C
  Omega = 125.04452 - 1934.136261 * T + 0.0020708 * T^2 +
    (T^3)/450000
  lambda = Theta - 0.00569 - 0.00478 * sin(radians(Omega))
  delta = asin(sin(radians(epsilon)) * sin(radians(lambda)))
  return(degrees(delta))
}


#' JDymd
#' @description Computes Julian Day from a given date.
#'
#' @usage JDymd(year,month,day,hour=12,minute=0,sec=0)
#'
#' @param year	numeric year
#' @param month	1-12: number of the month.
#' @param day	1-31: day of the month.
#' @param hour 0-23: hour of the day.
#' @param minute 0-59: minutes.
#' @param sec	0-59: seconds.
#'
#' @author Javier G. Corripio
#'
#'
#' @return Julian Day, or number of days since January 1, 4713 BCE at noon UTC
#' @export
#'
#' @examples
#'  declination(JDymd(2019,1,1))
JDymd <- function (year, month, day, hour = 12, minute = 0, sec = 0)
{
  if (nargs() < 3) {
    cat("USAGE: declination(year,month,day,hour=12,minute=0,sec=0) \n")
    return()
  }
  hour = hour + minute/60 + sec/3600
  jd = 367 * year - (7 * (year + (month + 9)%/%12))%/%4 +
    (275 * month)%/%9 + day + 1721013.5 + hour/24
  return(jd)
}


#' eqtime
#' @description Computes the equation of time for a given Julian Day.
#'
#' @usage eqtime(jd)
#'
#' @param jd	numeric Julian Day
#'
#' @author Javier G. Corripio
#'
#'
#' @return Equation of time in minutes.
#' @export
#'
#' @examples
#'  # plot the equation of time for 2013 at daily intervals
#' jdays = seq(ISOdate(2013,1,1),ISOdate(2013,12,31),by='day')
#' jd = JD(jdays)
#' plot(eqtime(jd))
#' abline(h=0,col=8)
#'
eqtime <- function (jd)
{
  if (nargs() < 1) {
    cat("USAGE: eqtime(jd)\n")
    return()
  }
  jdc = (jd - 2451545)/36525
  sec = 21.448 - jdc * (46.815 + jdc * (0.00059 - jdc * (0.001813)))
  e0 = 23 + (26 + (sec/60))/60
  ecc = 0.016708634 - jdc * (4.2037e-05 + 1.267e-07 * jdc)
  oblcorr = e0 + 0.00256 * cos(radians(125.04 - 1934.136 *
                                         jdc))
  y = (tan(radians(oblcorr)/2))^2
  l0 = 280.46646 + jdc * (36000.76983 + jdc * (0.0003032))
  l0 = (l0 - 360 * (l0%/%360))%%360
  rl0 = radians(l0)
  gmas = 357.52911 + jdc * (35999.05029 - 0.0001537 * jdc)
  gmas = radians(gmas)
  EqTime = y * sin(2 * rl0) - 2 * ecc * sin(gmas) + 4 * ecc *
    y * sin(gmas) * cos(2 * rl0) - 0.5 * y^2 * sin(4 * rl0) -
    1.25 * ecc^2 * sin(2 * gmas)
  return(degrees(EqTime) * 4)
}

