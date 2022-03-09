#' Generate observed counts from the set of bird trajectories.
#'
#' @description Generate observed counts from the set of bird trajectories.
#' 
#' @param migr A migration structure (attributes site_name, horizon, initial_state are used).
#' @param traj A trajectory matrix with a trajectory for each bird.
#'  For each bird, at each time step (from 0 to horizon), position of the bird.
#'  positions: sites, 0 for flying, -1 for dead.
#'  matrix N x (horizon + 1)
#' @param mask Boolean matrix (horizon x S) indicating the available observations,
#'  (mask(t,i)=TRUE if site i is observed at time t).
#'
#' @return Counts of observed birds on sites for time steps from 1 to horizon, 
#'  from the provided set of bird trajectories,
#'  matrix (S + 2) x (horizon + 1).
#'  At each time step, count of birds observed on each site.
#'  NA correspond to non observation.
#'  For compatibility with trajectory, the first column (time step 0) and 
#'  the 2 last rows ( S+1 and S+2) are kept and filled with NA.
#'  
#' @export
#'
#' @examples
#' migr <- generate_toy_migration()
#' traj <- generate_trajectories( migr )
#' obs <- generate_observedcounts( migr, traj )

generate_observedcounts <- function(migr, traj, mask) {

  stopifnot( class(migr) == "migration" )
  stopifnot( all(dim(traj) == c(sum(migr$initial_state),
                                         migr$horizon + 1)) )
  stopifnot(migr$observation_law_type == "Poisson")

  site_nb <- length(migr$site_name)
  bird_position_nb <- site_nb + 2
  horizon <- migr$horizon
  time_step_nb <- horizon + 1 # add time t0

  if (missing(mask)) {mask <- matrix(TRUE, nrow = site_nb, ncol = horizon)}

  my_mask <- matrix(FALSE, nrow = bird_position_nb, ncol = time_step_nb)
  my_mask[c(1:site_nb), c(2:time_step_nb)] <- mask

  # count birds at each time step given birds trajectories
  counts <- get_counts(migr, traj)
  counts[ , 1] <- NA # time t0 not observed
  counts[site_nb + 1, ] <- NA # dead not observed
  counts[site_nb + 2, ] <- NA # flying not observed

  # generate observations from counts
  observation <- matrix(NA,  nrow = bird_position_nb, ncol = time_step_nb)
  observation[c(1:site_nb), c(2:time_step_nb)] <- matrix(stats::rpois(site_nb * horizon,
                                                                      counts[c(1:site_nb), c(2:time_step_nb)]),
                                                         nrow = site_nb,
                                                         ncol = horizon)
  observation[ my_mask == FALSE] <- NA

  return( observation )
}

