#ifndef RCPP_BIRDNET_CPP_GET_COUNTS
#define RCPP_BIRDNET_CPP_GET_COUNTS

#include <Rcpp.h>


// Counts birds on each state for every time step.
Rcpp::NumericMatrix get_counts_C (Rcpp::List migration, 
                                  Rcpp::NumericMatrix trajs);

// Count birds on each site for a specific time step in a trajectory.
Rcpp::NumericVector get_counts_t (const Rcpp::List& migration, 
                                  Rcpp::NumericMatrix::Column trajs_col);
  
#endif
