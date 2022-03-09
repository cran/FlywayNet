#ifndef RCPP_BIRDNET_CPP_GENERATE_TRAJECTORIES
#define RCPP_BIRDNET_CPP_GENERATE_TRAJECTORIES

#include <Rcpp.h>


// Simulates trajectories of individual birds.
Rcpp::NumericMatrix generate_trajectories_C(
    Rcpp::List                          migration, 
    Rcpp::Nullable<Rcpp::NumericMatrix> trajectory,
    Rcpp::Nullable<int>                 end_time );

// initializes the trajectories of birds
Rcpp::NumericMatrix init_trajectories(const Rcpp::List& migration);

// simulate (partial) individual trajectories.
void simulate_trajectories(Rcpp::NumericMatrix& trajs, 
                           const Rcpp::List& migration, 
                           int end_time);

// --------
int c_sample_site(Rcpp::NumericMatrix::Row probs); 
#endif
