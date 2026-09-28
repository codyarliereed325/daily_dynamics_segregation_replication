#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// Helper function: Gaussian weighting
double gaussian_weight(double x, double mu = 0, double sigma = 1)
{
    return std::exp(-std::pow(x - mu, 2) / (2 * std::pow(sigma, 2)));
}

// Helper function: Haversine distance
double haversine_distance(double lat1, double lon1, double alt1, double lat2, double lon2, double alt2)
{
    constexpr double earth_radius = 6371000.0; // Earth's radius in meters
    constexpr double pi = 3.14159265358979323846;
    constexpr double pi_180 = pi / 180;

    double lat1_rad = lat1 * pi_180;
    double lon1_rad = lon1 * pi_180;
    double lat2_rad = lat2 * pi_180;
    double lon2_rad = lon2 * pi_180;

    double delta_lat = lat2_rad - lat1_rad;
    double delta_lon = lon2_rad - lon1_rad;
    double a = std::sin(delta_lat / 2) * std::sin(delta_lat / 2) +
               std::cos(lat1_rad) * std::cos(lat2_rad) *
                   std::sin(delta_lon / 2) * std::sin(delta_lon / 2);
    double c = 2 * std::atan2(std::sqrt(a), std::sqrt(1 - a));
    double distance = earth_radius * c;

    double delta_alt = alt2 - alt1;
    double dist = std::sqrt(std::pow(distance, 2) + std::pow(delta_alt, 2)) / 1000;

    return dist;
}

// Integrated function
// [[Rcpp::export]]
arma::mat adjustRacialFrequenciesTracts(arma::vec lat, arma::vec lon, arma::vec alt, arma::vec timestamps, arma::mat racialFrequencies, double distSigma = 1, double timeSigma = 1)
{
    int n = lat.size();
    int racialGroups = racialFrequencies.n_cols;
    arma::mat weightedMatrix(n, n, fill::zeros);
    arma::mat adjustedFrequencies(n, racialGroups, fill::zeros);

    // Calculate weighted distance-time matrix
    for (int i = 0; i < n; ++i)
    {
        for (int j = 0; j < n; ++j)
        {
            if (i != j)
            {
                double dist = haversine_distance(lat(i), lon(i), alt(i), lat(j), lon(j), alt(j));
                double timeDiff = std::abs(timestamps(j) - timestamps(i));
                double weightedDist = gaussian_weight(dist, 0, distSigma);
                double weightedTimeDiff = gaussian_weight(timeDiff, 0, timeSigma);
                weightedMatrix(i, j) = weightedDist * weightedTimeDiff;
            }
            else
            {
                weightedMatrix(i, j) = 1; // Assign full weight to self
            }
        }
    }

    // Adjust racial group frequencies using the weighted matrix
    adjustedFrequencies = weightedMatrix * racialFrequencies;

    return adjustedFrequencies;
}