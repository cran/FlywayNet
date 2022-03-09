#ifndef RCPP_BIRDNET_MCEM_UTILS
#define RCPP_BIRDNET_MCEM_UTILS

#include <Rcpp.h>

// Update parameters of the migration. Use in MCEM methods and estimation 
// from trajectories
void update_parameters(Rcpp::List migration, Rcpp::NumericMatrix particles, 
                       bool estimate_transitions, 
                       Rcpp::LogicalVector estimate_sojourns,
                       Rcpp::List sojourn_domain, 
                       double min_prob);


/**
 * maxlog function between two log obs. probabilities
 * @param [IN] l1, first val
 * @param [IN] l2, second val
 * @return ln(e^l1+e^l2) (p92 chap4. Yu)
 */
double maxlog(double l1, double l2);


/**
* maxlog_vec
* @param [IN] v, vector of log obs. probabilities
* @return ln(e^v[1]+...+e^v[n]) (p92 chap4. Yu)
*/
double maxlog_vec(Rcpp::NumericVector v);

/**
 * maxlog_softmax
 * 
 * @param [IN/OUT] v, vector of n log obs. probabilities as input. In output : 
 * multinomial distribution on the 1:n particles
 *  
 *  e^v[i] / (e^v[1] + ...+ e^v[n])
 *  = exp(log(e^v[i]) - log(e^v[1] + ... + e^v[n]))
 *  = exp(v[i] - maxlog_vec(v))
 */
void maxlog_softmax(Rcpp::NumericVector v);

#endif
