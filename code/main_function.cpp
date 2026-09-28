#include <RcppArmadillo.h>
#include <RcppParallel.h>
// [[Rcpp::depends(RcppArmadillo, RcppParallel)]]
using namespace Rcpp;
using namespace arma;
using namespace RcppParallel;

// Helper function: Gaussian weighting
inline double gaussian_weight(double x, double mu = 0, double sigma = 1)
{
    return std::exp(-std::pow(x - mu, 2) / (2 * std::pow(sigma, 2)));
}

// Helper function: Haversine distance
inline double haversine_distance(double lat1_rad, double lon1_rad, double alt1, double lat2_rad, double lon2_rad, double alt2)
{
    constexpr double earth_radius = 6371000.0; // Earth's radius in meters

    double delta_lat = lat2_rad - lat1_rad;
    double delta_lon = lon2_rad - lon1_rad;
    double a = std::sin(delta_lat / 2) * std::sin(delta_lat / 2) +
               std::cos(lat1_rad) * std::cos(lat2_rad) *
                   std::sin(delta_lon / 2) * std::sin(delta_lon / 2);
    double c = 2 * std::atan2(std::sqrt(a), std::sqrt(1 - a));
    double distance = earth_radius * c;

    double delta_alt = alt2 - alt1;
    double dist = std::sqrt(std::pow(distance, 2) + std::pow(delta_alt, 2)) / 1000; // Distance in kilometers

    return dist;
}

class WeightComputationWorker : public Worker
{
private:
    const arma::vec &lat;
    const arma::vec &lon;
    const arma::vec &alt;
    const arma::vec &timestamps;
    const double distSigma;
    const double timeSigma;
    arma::mat &weightedMatrix;

public:
    WeightComputationWorker(const arma::vec &lat, const arma::vec &lon, const arma::vec &alt,
                            const arma::vec &timestamps, double distSigma, double timeSigma, arma::mat &weightedMatrix)
        : lat(lat), lon(lon), alt(alt), timestamps(timestamps), distSigma(distSigma), timeSigma(timeSigma),
          weightedMatrix(weightedMatrix) {}

    void operator()(std::size_t begin, std::size_t end)
    {
        for (std::size_t i = begin; i < end; ++i)
        {
            for (std::size_t j = i; j < lat.size(); ++j)
            {
                double dist = haversine_distance(lat(i), lon(i), alt(i), lat(j), lon(j), alt(j));
                double timeDiff = std::abs(timestamps(j) - timestamps(i));
                double weight = gaussian_weight(dist, 0, distSigma) * gaussian_weight(timeDiff, 0, timeSigma);
                weightedMatrix(i, j) = weight;
                weightedMatrix(j, i) = weight; // Use symmetry
            }
        }
    }
};

// Integrated function
// [[Rcpp::export]]
arma::mat adjustRacialFrequencies(arma::vec lat, arma::vec lon, arma::vec alt, arma::vec timestamps, arma::mat racialFrequencies, double distSigma = 1, double timeSigma = 1)
{
    int n = lat.size();
    arma::mat weightedMatrix(n, n, fill::zeros);

    // Precompute radians
    lat *= datum::pi / 180;
    lon *= datum::pi / 180;

    // Calculate weighted distance-time matrix in parallel
    WeightComputationWorker worker(lat, lon, alt, timestamps, distSigma, timeSigma, weightedMatrix);
    parallelFor(0, n, worker);

    // Adjust racial group frequencies using the weighted matrix
    return weightedMatrix * racialFrequencies;
}
