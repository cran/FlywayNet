#include <Rcpp.h>

#include "birdnet_mcem_utils.h"

/***
 * Update parameters of the migration. Use only in MCEM methods.
 * 
 * @param [IN/OUT] migration, the migration with the current values of 
 *  parameters.
 * @param [IN] particles, resampled particles that must be simulated 
 *  until horizon time.
 * @param [IN] estimate_transitions, boolean that equals TRUE if matrix 
 *  transitions are updated.
 * @param [IN] estimate_sojourns, vector of boolean (of size I) that  tells if
 *  sojourns should be updated.
 * @param [IN] sojourn_domain, range of sojourn mean time.
 * @param [IN] min_prob, add minimal probability according link_knowledge
 */
void update_parameters(Rcpp::List migration, Rcpp::NumericMatrix particles, 
                       bool estimate_transitions, 
                       Rcpp::LogicalVector estimate_sojourns,
                       Rcpp::List sojourn_domain,
                       double min_prob)
{
  Rcpp::StringVector site_name = migration["site_name"];
  Rcpp::LogicalMatrix link_knowledge = migration["link_knowledge"];
  Rcpp::NumericVector death_probability = migration["death_probability"];
  Rcpp::NumericMatrix trans_law_param = migration["transition_law_param"];
  Rcpp::NumericVector soj_law_param = migration["sojourn_law_param"];
  
  int site_nb = site_name.length();
  //first column is sum of sojourns, second is number of sojourns
  Rcpp::NumericMatrix sojourns_eff(site_nb, 2);
  //matrix of simulated transitions
  Rcpp::NumericMatrix transitions_eff(site_nb, site_nb);
  
  for (int i =0; i< particles.rows(); i++){
    Rcpp::NumericMatrix::Row traj_i= particles.row(i);
    int last_site = traj_i[0];
    bool flight = false;
    sojourns_eff(last_site-1,1) = sojourns_eff(last_site-1,1)+1;
    for (int t=1;t<traj_i.size();t++) {
      int curr_pos = traj_i[t];
      if (flight) {
        if (curr_pos != 0){
          transitions_eff(last_site-1, curr_pos-1) = 
            transitions_eff(last_site-1, curr_pos-1)+1; 
          sojourns_eff(curr_pos-1,1) = sojourns_eff(curr_pos-1,1)+1;
          last_site = curr_pos;
          flight = false;
        }
      } else if (curr_pos ==  0){
        flight = true;
      } else if (curr_pos ==  -1){
        t = traj_i.size();
      } else {
        sojourns_eff(last_site-1,0) = sojourns_eff(last_site-1,0)+1;
      }
    }
  }
  /*************/
  //estimate transitions
  if (estimate_transitions) {
    //double min_prob = 0.05;
    for (int s =0; s<site_nb; s++) {
      Rcpp::NumericMatrix::Row tr_eff_s = transitions_eff.row(s);
      double sumtrans = Rcpp::sum(tr_eff_s);
      if (sumtrans > 0) {
        Rcpp::LogicalMatrix::Row links_s= link_knowledge.row(s);
        double dp = death_probability[s];
        std::vector<bool> is_minimal(site_nb, false);
        double norm_factor = 1 - dp; 
        double sumtrans_tonorm = 0;
        for (int d=0; d<site_nb;d++) {
          if (links_s[d] and (double(tr_eff_s[d])*(1-dp)/sumtrans) < min_prob){
            is_minimal[d] = true;
            norm_factor -= min_prob;
          } else {
            sumtrans_tonorm += double(tr_eff_s[d]);
          }
        }
        for (int d=0; d<site_nb;d++) {
          if (is_minimal[d]) {
            trans_law_param(s,d) = min_prob;
          } else {
            trans_law_param(s,d) = double(tr_eff_s[d])*
              norm_factor/sumtrans_tonorm;
          }
        }
      }
    }
    //check 
    for (int s =0; s<site_nb; s++) {
      Rcpp::NumericMatrix::Row trans_law_s = trans_law_param.row(s);
      double sumtrans = Rcpp::sum(trans_law_s);
      if (std::abs(sumtrans - (1-death_probability[s])) > 0.0000001){
        Rcpp::stop("std::abs(sumtrans - (1-death_probability[s])) > 0.0000001\n");
      }
    }
  }
  /*************/
  //update sojourns
  for (int s =0; s<site_nb; s++) {
    if (estimate_sojourns[s] and sojourns_eff(s,1) > 0) {
      soj_law_param[s] = std::max(Rcpp::as<Rcpp::NumericVector>(sojourn_domain[s])[0], 
                                  std::min(Rcpp::as<Rcpp::NumericVector>(sojourn_domain[s])[1], 
                                                              double(sojourns_eff(s,0))/
                                                                double(sojourns_eff(s,1))));
    }
  }
}


/**
 * maxlog function between two log obs. probabilities
 * @param [IN] l1, first val
 * @param [IN] l2, second val
 * @return ln(e^l1+e^l2) (p92 chap4. Yu)
 */
double maxlog(double l1, double l2)
{
  return std::max(l1, l2) + std::log(1+std::exp(-std::abs(l1-l2)));
}


/**
* maxlog_vec
* @param [IN] v, vector of log obs. probabilities
* @return ln(e^v[1]+...+e^v[n]) (p92 chap4. Yu)
*/
double maxlog_vec(Rcpp::NumericVector v)
{
  unsigned int n = v.size();
  if (n == 0){
    return -9999;
  }
  if (n == 1){
    return v[0];
  }
  double maxval = maxlog(v[0], v[1]);
  for (unsigned int i=2; i <n; i++){
    maxval = maxlog(maxval, v[i]);
  }
  return maxval;
}

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
void maxlog_softmax(Rcpp::NumericVector v)
{
  unsigned int n = v.size();
  if (n == 0){
    return;
  }
  if (n == 1){
    v[0] = 1.0;
    return;
  }
  double maxval = maxlog_vec(v);
  for (unsigned int i=0;i <n; i++){
    v[i] = std::exp(v[i] - maxval);
  }
}
