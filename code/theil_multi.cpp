#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double theil_index_multi(NumericMatrix data) {
    int n = data.nrow();
    int r = data.ncol();
    double T = sum(data); // Total population
    double epsilon = 1e-10; // Avoid exceeding float memory threshold

    // Calculate total CZ entropy E
    double E = 0;
    NumericVector groupTotals(r, 0.0);
    for(int j = 0; j < r; ++j) {
        for(int i = 0; i < n; ++i) {
            groupTotals[j] += data(i, j);
        }
        double groupProportion = groupTotals[j] / T;
        if(groupProportion > epsilon) {
            E += groupProportion * log(1.0 / groupProportion);
        }
    }

    // Calculate Theil index H
    double H = 0;
    for(int i = 0; i < n; ++i) {
        double t_i = sum(data.row(i)); // Population of unit i
        double E_i = 0; // Entropy of unit i
        for(int j = 0; j < r; ++j) {
            double prop = data(i, j) / t_i;
            if(prop > epsilon) {
                E_i += prop * log(1.0 / prop);
            }
        }
        H += (t_i / T) * ((E - E_i) / E);
    }

    return H;
}
