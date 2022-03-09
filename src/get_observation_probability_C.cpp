#include <Rcpp.h>

#include "get_observation_probability_C.h"


//' Internal C function (do not use). 
//' 
//' Computes the probability of observations given the birds count from 
//' trajectories : P(O | Pi_[0:T])
//' 
//' @param obs Observations. Matrix of I+2 rows and T+1 columns with observed 
//'   bird counts. I is the numbers of sites, to which one adds the virtual 
//'   sites 'death' and 'flight' and T is the horizon of the migration. 
//'   structure.
//' @param hidden_count Hidden counts. Matrix of I+2 rows and T+1 columns with 
//'   simulated bird counts. As computed by function get_count. 
//' @param use_log Boolean that states if the log probability should be returned.
//' 
//' @return the probability (or log-probability) of the observed data given 
//'   simulated counts.
//'   
//' @export
// [[Rcpp::export]]

double get_observation_probability_C(const Rcpp::NumericMatrix& obs, 
                        const Rcpp::NumericMatrix& hidden_count, 
                        bool use_log)
{
  if (obs.ncol() != hidden_count.ncol()) {
    Rcpp::stop(" internal error get_observation_probability");
  }
  double ret = 1;
  if (use_log) {
    ret = 0;
  }
  for (int t=0; t<obs.ncol(); t++){
    if (use_log) {
      ret += get_observation_probability_t(obs.column(t), hidden_count.column(t), use_log);
    } else {
      ret = ret* get_observation_probability_t(obs.column(t), hidden_count.column(t), use_log);
    }
  }
  return ret;
}


/**
 * Computes the probability of observations given the birds count 
 * at time t to compute weights. See document p.9 : P(O | Pi_t^m)
 * 
 * @param [IN] obs_t : vector (of size I+2) of observed bird counts at time t.
 *   (I numbers of sites, plus the virutal sites death and flight)
 * @param[IN] hidden_count_t: vector (of size I+2) of simulated bird counts 
 *   at time t.
 * @param[IN] use_log, should the log be returned.
 * @return probability of the observed data given simulated counts at time t.
 */
double get_observation_probability_t(const Rcpp::NumericVector& obs_t, 
                        const Rcpp::NumericVector& hidden_count_t, 
                        bool use_log)
{
  double ret = 1;
  if (use_log) {
    ret = 0;
  }
  for (int i=0; i<obs_t.size(); i++) {
    double obs_ti = obs_t[i];
    double hidden_count_ti = hidden_count_t[i];
    if (not std::isnan(obs_ti)) {
      if (hidden_count_ti == 0)  {
        if (use_log) {
          ret += R::dnbinom(obs_ti, 1, 0.8, use_log);
        } else {
          ret = ret* R::dnbinom(obs_ti, 1, 0.8, use_log);
        }
      } else {
        if (use_log) {
          ret += R::dpois(obs_ti, hidden_count_ti, use_log);
        } else {
          ret = ret*R::dpois(obs_ti, hidden_count_ti, use_log);
        }
      }
    }
  }
  return ret;
}
