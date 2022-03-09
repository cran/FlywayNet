#include <Rcpp.h>

#include "birdnet_mcem_utils.h"



//' Internal C function (do not use). 
//' 
//' Estimate parameters from a set of trajectories 
//' 
//' @param migration A migration (migration structure).
//' @param trajectories Set of trajectories (trajectory structure).
//' @param estimate_transitions Boolean for requiring transition parameters estimation.
//' @param estimate_sojourns Boolean for requiring sojourn parameters estimation.
//' @param sojourn_domain Sojourn parameters domains.
//' @param min_prob Minimum probability.
//' 
//' @return migration with estimated parameters (migration structure).
//' 
//' @export
// [[Rcpp::export]]

Rcpp::List estimate_migration_from_trajectories_C(Rcpp::List migration,
                                     Rcpp::NumericMatrix trajectories, 
  Rcpp::Nullable<bool> estimate_transitions = R_NilValue, 
  Rcpp::Nullable<Rcpp::LogicalVector> estimate_sojourns = R_NilValue,
  Rcpp::Nullable<Rcpp::List> sojourn_domain = R_NilValue,
  Rcpp::Nullable<double> min_prob = R_NilValue)
{
  Rcpp::List migr = Rcpp::clone(Rcpp::as<Rcpp::List>(migration));
  bool estimate_transitions_i = true;
  Rcpp::LogicalVector estimate_sojourns_i;
  Rcpp::List sojourn_domain_i = 
    Rcpp::clone(Rcpp::as<Rcpp::List>(sojourn_domain));
  double min_prob_i = 0;
  if (not estimate_transitions.isNull()) {
    estimate_transitions_i = Rcpp::as<bool>(estimate_transitions); 
  }
  if (estimate_sojourns) {
    Rcpp::StringVector site_name = migration["site_name"];
    int site_nb = site_name.size();
    estimate_sojourns_i=Rcpp::LogicalVector(site_nb);
    estimate_sojourns_i.fill(true);
    estimate_sojourns_i[site_nb-1] = false;
  } else {
    estimate_sojourns_i=
      Rcpp::clone(Rcpp::as<Rcpp::LogicalVector>(estimate_sojourns));
  }
  if (not min_prob.isNull()) {
    min_prob_i = Rcpp::as<double>(min_prob); 
  }
  update_parameters(migr, trajectories, estimate_transitions_i, 
                    estimate_sojourns_i, sojourn_domain_i, min_prob_i);
  
  Rcpp::List estimation_method;
  estimation_method.push_back("from_trajectories", "name");
  Rcpp::List settings;
  settings.push_back(estimate_transitions_i, "estimate_transitions");
  settings.push_back(estimate_sojourns_i, "estimate_sojourns");
  settings.push_back(sojourn_domain_i, "sojourn_domain");
  settings.push_back(min_prob_i, "min_prob");
  estimation_method.push_back(settings, "settings");
  Rcpp::List output;
  output.push_back(migr["transition_law_param"], "transition_law_param");
  output.push_back(migr["sojourn_law_param"], "sojourn_law_param");
  estimation_method.push_back(output, "output");
  migration.push_back(estimation_method, "estimation_method");
  return migration;
}



