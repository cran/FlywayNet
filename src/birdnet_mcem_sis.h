#ifndef RCPP_BIRDNET_CPP_MCEM_SIS
#define RCPP_BIRDNET_CPP_MCEM_SIS

#include <Rcpp.h>


// Compute likelihoods (or log likelihoods) of particles until time t ie:
void compute_observation_probabilities(Rcpp::NumericMatrix likelihoods, 
                         bool use_log, Rcpp::List migration,
                         Rcpp::NumericMatrix particles, 
                         int t, int nb_birds, int nb_particles);

// Initializes the observation probabilities structure
 void init_observation_probabilities(Rcpp::NumericMatrix likelihoods, bool use_log);


// Initialize the particles structure. A particle is a set of birds 
// trajectories. The birds are allocated for each particle in their initial 
// state.
void init_particles(Rcpp::NumericMatrix particles,
                    const Rcpp::List migration, 
                    int nb_particles = 1,
                    int t = -1);

// Resampling particles, resamples particles at current time. 
Rcpp::IntegerVector  resampling_particles(
    Rcpp::NumericMatrix likelihoods,
    bool use_log, int t, int nb_particles);

// Apply the resampling of particles. Used only in MCEM SIS.
void apply_resampling(
    Rcpp::NumericMatrix particles_new,
    Rcpp::NumericMatrix likelihoods_new,
    Rcpp::IntegerVector resampling,
    Rcpp::NumericMatrix particles,
    Rcpp::NumericMatrix likelihoods,
    int nb_birds, int t, int nb_particles);

// maxlog_softmax
void maxlog_softmax(Rcpp::NumericVector v);

#endif
