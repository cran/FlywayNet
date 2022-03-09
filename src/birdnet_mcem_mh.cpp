#include "generate_trajectories_C.h"

#include "birdnet_mcem_mh.h"

/**
 * Re-simulates a subset of (partial) individual trajetories.
 * Used only in MCEM MH.
 * 
 * @param [IN/OUT] trajs, a matrix of birds_nb * (T+1) as returned by 
 *   the function init_trajectories
 * @param [IN] migration, the migration structure
 * @param [IN] end_time, the time at which the simulation must ends. 
 *  Following time steps that are not part of the last transition are kept to NA
 * @param prob, the probability that an individual trajectory is re simulated.
 */
void resimulate_trajectories(Rcpp::NumericMatrix& trajs, 
                             const Rcpp::List& migration, 
                             int end_time, double prob)
{
  int horizon_time = Rcpp::as<int>(migration["horizon"]) ;
  int horizon_col = horizon_time + 1 ;
  Rcpp::NumericVector draws = Rcpp::runif(trajs.nrow());
  for (int i =0; i<trajs.nrow(); i++){
    if (draws[i] < prob) {
      Rcpp::NumericMatrix::Row trajrow = trajs.row(i);
      for (int t=1 ; t<horizon_col; t++){
        
        trajrow[t] = Rcpp::NumericMatrix::get_na();
      }
      
    }
  }
  simulate_trajectories(trajs, migration, end_time);
}
