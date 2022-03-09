#' Computes the probability of observations given the birds count from 
#' trajectories.
#' 
#' @description  Computes the probability of observations given the birds count from 
#'               trajectories : P(O | Pi_[0:T])
#' @param obs Matrix of I+2 rows and T+1 columns with observed 
#'   bird counts. I is the numbers of sites, to which one adds the virtual 
#'   sites 'death' and 'flight' and T is the horizon of the migration 
#'   structure.
#' @param hidden_count Matrix of I+2 rows and T+1 columns with 
#'   simulated bird counts. As computed by function get_count 
#' @param use_log Boolean that states if the log probability should 
#'   be returned.
#' @return the probability (or log-probability) of the observed data given 
#'   simulated counts.
#' @export
#'
#' @examples
#' migr <- generate_toy_migration()
#' traj <- generate_trajectories( migr )
#' get_observation_probability( migr$observation, get_counts( migr, traj ), use_log = FALSE)

get_observation_probability <- function ( obs, hidden_count, use_log=TRUE ) {
 
   prob <- get_observation_probability_C(obs, hidden_count, use_log)
 
   return(prob)
 }