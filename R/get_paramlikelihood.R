#' Computes, by simulation, the log-likelihood of parameters 
#' 
#' @description  Computes, by simulation, the log-likelihood of parameters   
#'               P(O | Lambda)
#' @param migr the migration structure which parameters likelihood 
#'  should be evaluated
#' @param obs  observation matrix used for the likelihood evaluation
#' @param n_simul number of simulations 
#' @return the log likelihood of the parameters of the migration structure migr.
#' @export
#'
#' @examples
#' migr <- generate_toy_migration()
#' get_paramlikelihood( migr, migr$observation)

get_paramlikelihood = function(migr, obs, n_simul=10)
{ 
  ### MAX*
  maxstar = function(x,y)
  {
    max(c(x,y)) + log(1+exp(-abs(x-y)))
  }
  
  ## recursive MAX*
  maxstar_rec = function(vec)
  {
    if (length(vec) < 3) {
      stop()
    }
    maxval = maxstar(vec[1], vec[2])
    
    for (i in 3:length(vec)) {
      maxval = maxstar(maxval, vec[i])
    }
    return(maxval)
  }
  
  logObsProb = rep(NA, n_simul);
  
  for (i in 1:n_simul) {
    logObsProb[i] <- get_observation_probability(
      obs = obs, 
      hidden_count = get_counts(migr, generate_trajectories(migr)), 
      use_log = T)
  }
  return(maxstar_rec(logObsProb)-log(n_simul))
}