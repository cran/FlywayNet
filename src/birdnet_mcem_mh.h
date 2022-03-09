#ifndef RCPP_BIRDNET_CPP_MCEM_MH
#define RCPP_BIRDNET_CPP_MCEM_MH

#include <Rcpp.h>

// Re-simulates a subset of (partial) individual trajetories.
void resimulate_trajectories(Rcpp::NumericMatrix& trajs, 
                             const Rcpp::List& migration, 
                             int end_time, double prob);

#endif
