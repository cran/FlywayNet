#'
#' Return a modified migration according generative parameters.
#' 
#' @param migr A migration structure. 
#' @param death_probability_mode Either "no", "mean" or "unif" 
#'  (or a vector of such values of length the number of sites). If "no", 
#'  death probability is not modified. If "mean" death probability 
#'  are set to the mean value of the death_probability_domain. If "unif", 
#'  the death probabilities are drawn uniformly into the range of the 
#'  death_probability_domain. Default is "no".
#' @param death_probability_domain Min and max of death probabilities. 
#'  Used only if death_probability_mode is not "no". Default is c(0.01,0.05)
#' @param sojourn_mode Either "no", "mean" or "unif" (or a vector of such values 
#'  of length the number of sites). If "no", sojourn mean time is not modfied. 
#'  If "mean" sojourns mean time is set to the mean value of the sojourn_domain. 
#'  If "unif", the sojourn mean time is drawn uniformly into the range of 
#'  the sojourn_domain. Default is "no".
#' @param sojourn_domain Min and max of sojourn mean time. 
#'  If vector of 2 values min and max then all domains are [min, max] interval,
#'  else contains the list of interval for each site.
#'  Used only if sojourn_type is not "no". Default is c(1,10) 
#' @param flight_duration_mode, Either "no", "mean" or "unif". If "no", flight 
#'  durations are not modified. If "mean" flight durations are set to the 
#'  closest integer of the mean value of the flight_duration_domain. 
#'  If "unif", the flight durations are drawn uniformly into the range of 
#'  the flight_duration_domain Default is "no". 
#' @param flight_duration_domain, Min and max integers of flight durations. 
#'  Used only if sojourn_type is not "no". Default is c(1,5)
#' @param transition_mode Either "no", "equi" or "unif". If "no", 
#'  the transition are not modified. If "equi" transition 
#'  are equiprobable taking into account the link_knowledge. If "unif" 
#'  transitions are drawn uniformly. Default is "no".
#' @param observation_mode Either "no", "simulation" or "hide". If "no",
#'  the observations are not modified. If "simulation", the observations as well 
#'  as the horizon are computed from a simulation of the migration. The horizon 
#'  is set to the time at which all birds are either dead or at the arrival 
#'  site. If "hide", observations are replaced with NA with probability 
#'  observation_hide. Default is "no".
#' @param observation_hide probability of hidding a single observation. Used 
#'  only if observation_mode is "hide". Default is 0.1.
#'
#' @return A migration modified according the specifications
#' @export
#'
#' @examples
#' migr <- generate_random_migration()
#' mod_migr <- generate_modified_migration(migr, transition_mode = "unif")


generate_modified_migration <- function(
  migr,
  death_probability_mode = "no",
  death_probability_domain = c(0.01,0.05),
  sojourn_mode = "no",
  sojourn_domain = c(1, 10),
  flight_duration_mode = "no",
  flight_duration_domain = c(1,5),
  transition_mode = "no",
  observation_mode = "no",
  observation_hide = 0.1)
{
  migration_mod = migr;
  link_knowledge = migration_mod$link_knowledge
  
  I = nrow(link_knowledge)
  
  if (length(death_probability_mode) == 1){
    death_probability_mode = rep(death_probability_mode, I)
  }
  if (length(sojourn_mode) == 1){
    sojourn_mode = rep(sojourn_mode, I)
  }
  #modify death probabilities
  if (! all(death_probability_mode == "no")) {
    for (i in 1:I) {
      if (death_probability_mode[i] == "mean") {
        migration_mod$death_probability[i] = mean(death_probability_domain);
      } else if (death_probability_mode[i] == "unif") {
        migration_mod$death_probability[i] = stats::runif(1, 
          min=death_probability_domain[1],max=death_probability_domain[2]);
      }
    }
  }
  
  #init sojourn time
  if (! all(sojourn_mode == "no")) {
    if (length(sojourn_domain) == 2) {
      min_sojourn_domain <- rep(sojourn_domain[1], 1, I)
      max_sojourn_domain <- rep(sojourn_domain[2], 1, I)
    } else {
      stopifnot( length(sojourn_domain) == I )
      min_sojourn_domain <- rep(0, 1, I)
      max_sojourn_domain <- rep(0, 1, I)
      for (i in 1:I) {
        min_sojourn_domain[i] <- sojourn_domain[[i]][1]
        max_sojourn_domain[i] <- sojourn_domain[[i]][2]
      }
    }
    for (i in 1:I) {
      if (sojourn_mode[i] == "mean") {
        migration_mod$sojourn_law_param[i] = mean( c(min_sojourn_domain[i], max_sojourn_domain[i]) );
      } else if (sojourn_mode[i] == "unif") {
        migration_mod$sojourn_law_param[i] = stats::runif(1, min=min_sojourn_domain[i], max=max_sojourn_domain[i]);
      }
    }
  }
  
  #modify flight durations
  if (flight_duration_mode != "no") {
    for (i in 1:I) {
      for (j in 1:I) {
        if (link_knowledge[i,j]) {
          if (flight_duration_mode == "mean") {
            migration_mod$flight_duration[i,j] = 
              round(mean(flight_duration_domain))
          } else if (flight_duration_mode == "mean") {
            migration_mod$flight_duration[i,j] = 
              sample(flight_duration_domain[1]:flight_duration_domain[2], size=1)  
          }
        }
      }
    }
  }
  
  #modif transition_law_param
  if (transition_mode != "no") {
    trans_mod = NULL
    if (transition_mode=="equi") {
      trans_mod = matrix(1, nrow=I, ncol=I)
    } else if (transition_mode=="unif"){
      trans_mod = matrix(stats::runif(I*I, 0, 1), nrow=I, ncol=I)
    }
    #normalize
    trans_mod[which(! link_knowledge)] = 0
    for (i in 1:(I-1)) {
      trans_mod[i,] = trans_mod[i,]*(1-migration_mod$death_probability[i])/
        sum(trans_mod[i,])
    }
    trans_mod[I,] = 0
    migration_mod$transition_law_param = trans_mod;
  }
  
  #modifiy observations
  if (observation_mode == "simulation") {
    virtual_horizon = as.integer(sum(migration_mod$sojourn_law_param) * 10)
    migration_mod$horizon = virtual_horizon
    traj = generate_trajectories(migration_mod)
    obs = generate_observedcounts(migration_mod, traj)
    #recalculate horizon and observations
    sink_sites = which(apply(link_knowledge, MARGIN=1, sum) == 0)
    new_horizon = NULL
    for (j in 1:ncol(traj)){
      if (is.null(new_horizon)) {
        if (all(unique(traj[,j]) %in% c(-1, sink_sites))) {
          new_horizon = j
        }
      }
    }
    if (is.null(new_horizon)) {
      stop(" generate_modified_migration : no horizon found ")
    }
    migration_mod$horizon = new_horizon
    migration_mod$observation = obs[,1:(new_horizon+1)]
  } else if (observation_mode == "hide") {
    obs_mask = sample(c(TRUE,FALSE), size=nrow(migration_mod$observation)*
                        ncol(migration_mod$observation), replace=TRUE,
                      prob=c(1-observation_hide,observation_hide))
    migration_mod$observation[!obs_mask] = NA
  }

  return (migration_mod);
}



