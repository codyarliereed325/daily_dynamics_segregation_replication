#include <cmath>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector haversine_distance(NumericVector lat1, NumericVector lon1, NumericVector lat2, NumericVector lon2)
{
  // Inputs are given in degrees.
  constexpr double earth_radius = 6371.0; // Earth's radius in km
  constexpr double pi = 3.14159265358979323846;
  constexpr double pi_180 = pi / 180.0;
  
  int n = lat1.size();
  
  // Check that all vectors have the same length
  if ((lon1.size() != n) || (lat2.size() != n) || (lon2.size() != n)) {
    stop("Input vectors must all have the same length.");
  }
  
  NumericVector out(n);
  
  for (int i = 0; i < n; i++)
  {
    double lat1_rad = lat1[i] * pi_180;
    double lon1_rad = lon1[i] * pi_180;
    double lat2_rad = lat2[i] * pi_180;
    double lon2_rad = lon2[i] * pi_180;
    
    double delta_lat = lat2_rad - lat1_rad;
    double delta_lon = lon2_rad - lon1_rad;
    
    double a = std::sin(delta_lat / 2.0) * std::sin(delta_lat / 2.0) +
      std::cos(lat1_rad) * std::cos(lat2_rad) *
      std::sin(delta_lon / 2.0) * std::sin(delta_lon / 2.0);
    double c = 2.0 * std::atan2(std::sqrt(a), std::sqrt(1.0 - a));
    
    out[i] = earth_radius * c;
  }
  
  return out;
}