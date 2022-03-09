#include <Rcpp.h>

#include "get_counts_C.h"

 
//' Internal C function (do not use). 
//' 
//' Counts birds on each state for every time step.
//' 
//' @param migration A migration (migration structure).
//' @param trajs Set of trajectories (trajectory matrix).
//' 
//' @return counts (count matrix).
//'
//' @export
// [[Rcpp::export]]

Rcpp::NumericMatrix get_counts_C (Rcpp::List migration, 
                                  Rcpp::NumericMatrix trajs)
{
  const Rcpp::StringVector& site_name = migration["site_name"];
  int site_nb = site_name.length();
  int horizon_col = Rcpp::as<int>(migration["horizon"]) + 1 ;
  
  Rcpp::NumericMatrix counts(site_nb+2, horizon_col);
  
  for (int j = 0; j<horizon_col; j++){
    Rcpp::NumericVector count_t = get_counts_t(migration, trajs.column(j));
    for (int pos=0; pos<count_t.size();pos++){
      counts(pos, j) = count_t(pos);
    }
  }
  return counts;
}


/*
 *  Count birds on each site for a specific time step in a trajectory.
 * 
 * @param [IN] migration, a migration structure
 * @param [IN] trajs_col, a column of a trajectory
 * @return a vector of bird counts of size site_nb + 2.
 * index site_nb + 1 gives the count of flying birds
 * index site_nb + 2 gives the count of dead birds
*/
Rcpp::NumericVector get_counts_t (const Rcpp::List& migration, 
                                  Rcpp::NumericMatrix::Column trajs_col)
{
  const Rcpp::StringVector& site_name = migration["site_name"];
  int site_nb = site_name.length();
  Rcpp::NumericVector counts(site_nb+2);
  for (int site=-1; site<=site_nb;site++){
    int nbbirds = 0;
    for (int i = 0; i<trajs_col.size(); i++){
      if (trajs_col[i] == site) nbbirds ++;
    }
    if (site==-1) {
      counts(site_nb) = nbbirds;
    } else if (site == 0) {
      counts(site_nb+1) = nbbirds;
    } else {
      counts(site-1) = nbbirds;
    }
  }
  return counts;
}
