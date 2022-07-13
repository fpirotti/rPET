#include <Rcpp.h>
#include <RProgress.h>
using namespace Rcpp;

inline double bilinearinterp(double q11, double q12, double q21, double q22, double x1, double x2, double y1, double y2, double x, double y) {
  double x2x, y2y, yy1, xx1;
  x2x = x2 - x;
  y2y = y2 - y;
  yy1 = y - y1;
  xx1 = x - x1;
  return(q11 * x2x * y2y + q21 * xx1 * y2y + q12 * x2x * yy1 + q22 * xx1 * yy1);
}



bool ray_intersects(NumericMatrix& heightmap,
                    NumericMatrix& addressmap,
                    NumericMatrix& pointcloud,
                    NumericVector& tanangles,
                    int i, int j, int angentry,
                    double maxheight, double precisionval,
                    double cossunangle, double sinsunangle,
                    int numbercols, int numberrows,
                    double zscale, double maxdist) {
  double xcoord, ycoord, tanangheight;
  double ceilxcoord,ceilycoord,floorxcoord,floorycoord;
  int nZ=0;
  double curZ, terrainZ;
  double prevCurZ= DBL_MAX;
  int address=0;
  for(int k = 1; k < maxdist; k++) {
    xcoord = i + sinsunangle * k;
    ycoord = j + cossunangle * k;
    curZ = terrainZ = heightmap(i, j);
    tanangheight = curZ + tanangles[angentry] * k * zscale;
    address = addressmap(i, j);

    if(xcoord > numberrows-1 || ycoord > numbercols-1 ||
       xcoord < 0 || ycoord < 0 ||
       tanangheight > maxheight) {
      Rcout << "\n -- Braking at  tan=" << tanangheight << " max=" << maxheight << std::endl;
      break;
    }

    while(curZ < prevCurZ){

      prevCurZ = curZ;

      if(xcoord > numberrows-1 || ycoord > numbercols-1 ||
         xcoord < 0 || ycoord < 0 ||
         tanangheight > maxheight) {
        Rcout << "\nBraking at  tan=" << tanangheight << " max=" << maxheight << std::endl;
        break;
      } else {
        ceilxcoord = ceil(xcoord);
        ceilycoord = ceil(ycoord);
        floorxcoord = floor(xcoord);
        floorycoord = floor(ycoord);

        // Get case where xcoord and ycoord integer number
        if(floorxcoord == ceilxcoord && floorycoord == ceilycoord) {
          if (tanangheight < heightmap(xcoord,ycoord)) {
            return(true);
          }
        }

        if(fabs(floorxcoord - ceilxcoord) < precisionval && floorycoord != ceilycoord) {
          if (tanangheight < (heightmap(floorxcoord,ceilycoord) - heightmap(floorxcoord,floorycoord))*(ycoord-floorycoord) + heightmap(floorxcoord,floorycoord)) {
            return(true);
          }
        }

        if(floorxcoord != ceilxcoord && fabs(floorycoord - ceilycoord) < precisionval) {
          if (tanangheight < (heightmap(ceilxcoord,floorycoord) - heightmap(floorxcoord,floorycoord))*(xcoord-floorxcoord) + heightmap(floorxcoord,floorycoord)) {
            return(true);
          }
        }

        if (heightmap(ceilxcoord, ceilycoord) < tanangheight &&
            heightmap(floorxcoord, ceilycoord) < tanangheight &&
            heightmap(ceilxcoord, floorycoord) < tanangheight &&
            heightmap(floorxcoord, floorycoord) < tanangheight) {
          continue;
        }

        if (tanangheight < bilinearinterp(heightmap(floorxcoord, floorycoord),
                                          heightmap(floorxcoord, ceilycoord),
                                          heightmap(ceilxcoord, floorycoord),
                                          heightmap(ceilxcoord, ceilycoord),
                                          floorxcoord, ceilxcoord,
                                          floorycoord, ceilycoord,
                                          xcoord, ycoord)) {
          return(true);
        }
      }

      curZ = curZ + pointcloud(address,2);
      tanangheight = curZ + tanangles[angentry] * k * zscale;
      address++;

    }

  }
  return(false);
}

//
// // [[Rcpp::export]]
// NumericMatrix rayshade_cpp(double sunangle, NumericVector anglebreaks,
//                             NumericMatrix& heightmap,
//                             NumericMatrix& addressmap,
//                             NumericMatrix& pointcloud,
//                             double zscale, double maxsearch, const NumericMatrix cache_mask,
//                             bool progbar) {
//
//   double precisionval = 1e-10;
//
//   //Rprintf("the value of nrow=%i    ncol=%i \n", pointcloud.nrow(), pointcloud.ncol() );
//
//
//   //Cache trig functions
//   double sinsunangle = sin(sunangle);
//   double cossunangle = cos(sunangle);
//   int numberangles = anglebreaks.size();
//   NumericVector tanangles(numberangles);
//   for(int i = 0; i < numberangles; i++) {
//     tanangles(i) = tan(anglebreaks[i]);
//   }
//
//   int numbercols = heightmap.ncol();
//   int numberrows = heightmap.nrow();
//   NumericMatrix shadowmatrix(numberrows,numbercols);
//   std::fill(shadowmatrix.begin(), shadowmatrix.end(), 1.0);
//   double maxdist = maxsearch;
//   int current_min_entry = 0;
//   int current_max_entry = numberangles - 1;
//   int current_entry = current_max_entry/2;
//   bool anyfound;
//   double maxheight = max(heightmap);
//   char str[100];
//   sprintf(str, "Raytracing with %.4f [:bar] ETA: :eta", maxdist);
//   Rprintf(str);
//   RProgress::RProgress pb( str );
//   double invnumberangles = 1 / (double)numberangles;
//
//   if(progbar) {
//     pb.set_total(pointcloud.rows()*numberangles);
//   }
//
//   //  if(numberangles < 3) {
//   for(int ang = 0; ang < numberangles; ang++) {
//     if(90-anglebreaks[ang]<0.00001){
//       Rcout << "\nSun elevation must be > 0 " << anglebreaks[ang] << std::endl;
//       continue;
//     }
//
//     //sprintf(str, "\nANGBreaks %.2f  ", anglebreaks[ang])str;
//     Rcout << "\nANGBreaks   " << anglebreaks[ang] << std::endl;
//
//
//     for(int i = 0; i < pointcloud.rows(); i++) {
//       Rcpp::checkUserInterrupt();
//       if(progbar) {
//         pb.tick();
//       }
//       for(int j = 0; j < numbercols; j++) {
//         if(cache_mask(i,j)) {
//           anyfound = false;
//
//           if(ray_intersects(heightmap,
//                              addressmap,
//                              tanangles,
//                             i, j, ang,
//                             maxheight, precisionval,
//                             cossunangle, sinsunangle,
//                             numbercols, numberrows,
//                             zscale, maxdist)) {
//             shadowmatrix(i,j) = 1 - ((double)ang + 1) * invnumberangles;
//           }
//         }
//         //   }
//       }
//     }
//   }
//   return(shadowmatrix);
// }

// [[Rcpp::export]]
NumericMatrix rayshade_cpp(double sunangle, NumericVector anglebreaks,
                           NumericMatrix& heightmap,
                           NumericMatrix& addressmap,
                           NumericMatrix& pointcloud,
                           double zscale, double maxsearch, const NumericMatrix cache_mask,
                           bool progbar) {

  double precisionval = 1e-10;

  //Cache trig functions
  double sinsunangle = sin(sunangle);
  double cossunangle = cos(sunangle);
  int numberangles = anglebreaks.size();
  NumericVector tanangles(numberangles);
  for(int i = 0; i < numberangles; i++) {
    tanangles(i) = tan(anglebreaks[i]);
  }

  int numbercols = heightmap.ncol();
  int numberrows = heightmap.nrow();
  NumericMatrix shadowmatrix(numberrows,numbercols);
  std::fill(shadowmatrix.begin(), shadowmatrix.end(), 1.0);
  double maxdist = maxsearch;
  int current_min_entry = 0;
  int current_max_entry = numberangles - 1;
  int current_entry = current_max_entry/2;
  bool anyfound;
  double maxheight = max(heightmap);
  char str[100];
  sprintf(str, "Raytracing with %.4f [:bar] ETA: :eta", maxdist);
  Rprintf(str);
  RProgress::RProgress pb( str );
  //RProgress::RProgress pb("Raytracing [:bar] ETA: :eta");
  double invnumberangles = 1 / (double)numberangles;

  if(progbar) {
    pb.set_total(numberrows);
  }

  for(int i = 0; i < numberrows; i++) {
    Rcpp::checkUserInterrupt();
    if(progbar) {
      pb.tick();
    }
    for(int j = 0; j < numbercols; j++) {
      if(cache_mask(i,j)) {
        anyfound = false;
        if(numberangles < 3) {
          for(int ang = 0; ang < numberangles; ang++) {
            if(ray_intersects(heightmap,
                              addressmap,
                              pointcloud,
                              tanangles,
                              i, j, ang,
                              maxheight, precisionval,
                              cossunangle, sinsunangle,
                              numbercols, numberrows,
                              zscale, maxdist)) {
              shadowmatrix(i,j) = 1 - ((double)ang + 1) * invnumberangles;
            }
          }
        } else {
          while(current_min_entry != current_entry && current_max_entry != current_entry) {
            if(ray_intersects(heightmap,
                              addressmap,
                              pointcloud,
                              tanangles,
                              i, j, current_entry,
                              maxheight, precisionval,
                              cossunangle, sinsunangle,
                              numbercols, numberrows,
                              zscale, maxdist)) {
              current_min_entry = current_entry;
              current_entry = (current_max_entry + current_entry)/2;
              anyfound = true;
            } else {
              current_max_entry = current_entry;
              current_entry = (current_min_entry + current_entry)/2;
            }
          }
          if(anyfound) {
            shadowmatrix(i,j) = 1 - ((double)current_entry + 1) * invnumberangles;
          }
          current_min_entry = 0;
          current_max_entry =  numberangles - 1;
          current_entry = current_max_entry/2;
        }
      }
    }
  }
  return(shadowmatrix);
}
