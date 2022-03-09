
#ifndef RCPP_BIRDNET_CPP_GET_OBSERVATION_PROBABILITY
#define RCPP_BIRDNET_CPP_GET_OBSERVATION_PROBABILITY

#include <Rcpp.h>



// Computes the probability of observations given the birds count from 
// trajectories : P(O | Pi_[0:T])
double get_observation_probability_C(const Rcpp::NumericMatrix& obs, 
                  const Rcpp::NumericMatrix& hidden_count, 
                  bool use_log);

// Computes the probability of observations given the birds count 
// at time t to compute weights. See document p.9 : P(O | Pi_t^m)
double get_observation_probability_t(const Rcpp::NumericVector& obs_t, 
                        const Rcpp::NumericVector& hidden_count_t, 
                        bool use_log);

#endif
