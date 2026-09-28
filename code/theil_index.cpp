#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector theil_index(NumericMatrix data)
{
    int M = data.nrow();  // Number of observations
    int J = data.ncol();  // Number of racialized groups
    double T = sum(data); // Total population in window
    double epsilon = 1e-10; // Avoid exceeding float memory threshold

    NumericVector result(J);
    NumericVector pi_m = colSums(data) / T;
    NumericMatrix px(M, J);
    NumericVector Etot(J);
    NumericMatrix E(M, J);

    // Compute Etot for each group
    for (int k = 0; k < J; ++k)
    {
        Etot[k] = pi_m[k] * log(1 / pi_m[k]) + (1 - pi_m[k]) * log(1 / (1 - pi_m[k]));
    }

    // Compute px and E matrices
    for (int i = 0; i < M; ++i)
    {
        NumericVector row = data(i, _);
        double rowSum = sum(row);
        if (rowSum > epsilon)
        {
            px(i, _) = row / rowSum;
            for (int k = 0; k < J; ++k)
            {
                E(i, k) = px(i, k) > epsilon ? px(i, k) * log(1 / px(i, k)) : 0;
                E(i, k) += (1 - px(i, k)) > epsilon ? (1 - px(i, k)) * log(1 / (1 - px(i, k))) : 0;
            }
        }
    }

    // Compute the result for each group
    for (int k = 0; k < J; ++k)
    {
        NumericVector E_col = E(_, k);
        result[k] = sum(rowSums(data) * (Etot[k] - E_col) / (Etot[k] * T));
    }
    return result;
}
