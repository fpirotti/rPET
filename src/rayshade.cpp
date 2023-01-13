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
                    NumericMatrix& lengthmap,
                    NumericMatrix& pointcloud,
                    NumericVector& tanangles,
                    int i, int j, int angentry,
                    double maxheight, double precisionval,
                    double cossunangle, double sinsunangle,
                    int numbercols, int numberrows,
                    double zscale, double maxdist,
                    double height) {

  double xcoord, ycoord, tanangheight, tanangheight2;
  double ceilxcoord,ceilycoord,floorxcoord,floorycoord;
  int nZ=0;
  double curZ, terrainZ;
  double prevCurZ= DBL_MAX;
  int address,nPointsAboveGroundPlane=0;
  int iii=0;


  for(int k = 0; k < maxdist; k++) {
    xcoord = i + sinsunangle * k;
    ycoord = j + cossunangle * k;
    tanangheight = heightmap(i, j) + height + tanangles[angentry] * k * zscale;
    tanangheight2 = tanangheight + tanangles[angentry];

    if(xcoord > numberrows-1 || ycoord > numbercols-1 ||
       xcoord < 0 || ycoord < 0 // ||  tanangheight > maxheight
    ) {
      continue;
    }
    // this first part checks the ground plane
    // therefore it does not make sense to test if it
    // intersects itself
    if(k>0){
      if(  tanangheight > maxheight) {
        // there is no element that can cast shadow in this pixel
        break;
      }
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

      // if (heightmap(ceilxcoord, ceilycoord) < tanangheight &&
      //     heightmap(floorxcoord, ceilycoord) < tanangheight &&
      //     heightmap(ceilxcoord, floorycoord) < tanangheight &&
      //     heightmap(floorxcoord, floorycoord) < tanangheight) {
      //   continue;
      // }

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


    ///////
    //
    //
    //
    //
    //
    //
    //
    //
    //
    //



    address = addressmap(xcoord, ycoord);
    if(address==0){
      continue;
    }
    nPointsAboveGroundPlane = lengthmap(xcoord, ycoord);

    iii=0;

    while(iii < nPointsAboveGroundPlane){

        if (tanangheight < pointcloud(address+iii,1)
              && tanangheight2  > pointcloud(address+iii,1)
              ) {

          // Rcout << " iii=" << iii << " nPointsAboveGroundPlane=" << nPointsAboveGroundPlane << std::endl;

          return(true);
        }
        iii++;
    }

   // Rcout << " iii=" << iii << " nPointsAboveGroundPlane=" << nPointsAboveGroundPlane << std::endl;





  }



  return(false);
}

// [[Rcpp::export]]
NumericMatrix rayshade_cpp(double sunangle,
                           NumericVector anglebreaks,
                           NumericMatrix& heightmap,
                           NumericMatrix& addressmap,
                           NumericMatrix& lengthmap,
                           NumericMatrix& pointcloud,
                           double zscale,
                           double maxsearch,
                           double maxheight,
                           const NumericMatrix cache_mask,
                           double height,
                           bool progbar)  {

  double precisionval = 1e-10;

  //Cache trig functions
  double sinsunangle = sin(sunangle);
  double cossunangle = cos(sunangle);
  int numberangles = anglebreaks.size();
  NumericVector tanangles(numberangles);
  for(int i = 0; i < numberangles; i++) {
    tanangles(i) = tan(anglebreaks[i]);
  }

  // printf("Hello there");
  int numbercols = heightmap.ncol();
  int numberrows = heightmap.nrow();
  NumericMatrix shadowmatrix(numberrows,numbercols);
  std::fill(shadowmatrix.begin(), shadowmatrix.end(), 1.0);
  double maxdist = maxsearch;
  int current_min_entry = 0;
  int current_max_entry = numberangles - 1;
  int current_entry = current_max_entry/2;
  bool anyfound;
 // double maxheight = max(heightmap);
  char str[100];
  // Rcout << " STARTING HERE nrows=" << pointcloud.nrow() << " cols=" << pointcloud.ncol() << std::endl;
  //sprintf(str, "Raytracing with MaxDist=%.4f MaxHeight=%.4f  [:bar] ETA: :eta", maxdist, maxheight);
  //Rprintf(str);
  //RProgress::RProgress pb( str );
  RProgress::RProgress pb("Raytracing [:bar] ETA: :eta");
  double invnumberangles = 1 / (double)numberangles;

  if(progbar) {
    pb.set_total(numberrows);
  }

  for(int i = 0; i < numberrows; i++) {
    Rcpp::checkUserInterrupt();
    if(progbar) {
      //Rcout << "  i=" <<  i << " j=" <<  " - " << std::endl;
      pb.tick();
    }
    for(int j = 0; j < numbercols; j++) {

      if(   NumericVector::is_na(heightmap(i,j))  ) {
        // Rcout << " inan " << i << "x" << j  <<  std::endl;
        shadowmatrix(i,j) = NA_REAL ;

        continue;
      }


      if(cache_mask(i,j)) {
        anyfound = false;
        if(numberangles < 30) {
          for(int ang = 0; ang < numberangles; ang++) {

            if(ray_intersects(heightmap,
                              addressmap,
                              lengthmap,
                              pointcloud,
                              tanangles,
                              i, j, ang,
                              maxheight, precisionval,
                              cossunangle, sinsunangle,
                              numbercols, numberrows,
                              zscale, maxdist,
                              height)) {
              shadowmatrix(i,j) = 1 - ((double)ang + 1) * invnumberangles;
            }
          }
        } else {
          while(current_min_entry != current_entry && current_max_entry != current_entry) {

            if(ray_intersects(heightmap,
                              addressmap,
                              lengthmap,
                              pointcloud,
                              tanangles,
                              i, j, current_entry,
                              maxheight, precisionval,
                              cossunangle, sinsunangle,
                              numbercols, numberrows,
                              zscale, maxdist, height)) {
              current_min_entry = current_entry;
              current_entry = (current_max_entry + current_entry)/2;
              anyfound = true;

            } else {
              current_max_entry = current_entry;
              current_entry = (current_min_entry + current_entry)/2;
            }

            // Rcout << " c min "  << current_min_entry << " curr "  <<  current_entry
            //       << " c max "  << current_max_entry <<  std::endl;

          }
          if(anyfound) {
            shadowmatrix(i,j) = 1.0 - ((double)current_entry + 1.0) * invnumberangles;
            // Rcout << " current_entry "  << ((double)current_entry + 1.0)
            //       << " invnumberangles "  <<  invnumberangles
            //       <<  std::endl;
          }
          // else {
          //   // Rcout << " i " << i << "x" << j  <<  std::endl;
          // }
          current_min_entry = 0;
          current_max_entry =  numberangles - 1;
          current_entry = current_max_entry/2;
        }
      }

      // Rcout << " BREAKING "  << std::endl;

    }
  }
  return(shadowmatrix);
}
