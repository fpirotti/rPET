
#' sunpos
#' @description Returns a matrix of azimuth and zenith angles of the sun given the
#' unit vectors from the observer to the direction of the sun.
#'
#' @param sunv coordinates x, y, z of the unit vector in the direction
#' of the sun.
#'
#' @author Javier G. Corripio
#'
#' @return A matrix of azimuth and zenith angles.
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
#'  sunpos(matrix( (1:6)/6, nrow=2, ncol=3))
radians <- function (degree)
{
  radian = degree * (pi/180)
  return(radian)
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



#' radians
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
