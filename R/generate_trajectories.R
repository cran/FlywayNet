#' Generate trajectories of birds.
#' 
#' @description Generate trajectories of birds.
#' A bird trajectory specifies, at each time step, the state of the bird:
#' a site number in 1...S, 0 for flying, -1 for dead and  NA for unknown.
#' If partial trajectories (positions of birds are not known until the horizon specified in migration structure) 
#' are provided with the argument traj, they are simulated until a time limit defined by the argument
#' \emph{end_time}.
#'
#' @param migr A migration structure (attributes \emph{site_name}, \emph{initial_state}, \emph{flight_duration}, 
#'  \emph{death_probability}, \emph{transition_law_param}, \emph{sojourn_law_param}, \emph{sojourn_law_type} are used).
#' @param traj Partial trajectories, matrix N x (horizon+1) that are going to be extended in time.
#'  If NULL, it is initialized from migr structure.
#' @param end_time Time until which simulation is required.
#'  If NULL, it is initialized with migr horizon.
#'
#' @return A trajectory matrix N x (horizon+1) defining a trajectory for each bird.
#'  A trajectory for one bird, specifies at each time step
#'  (from 0 to horizon) the state of the bird. Incomplete trajectories
#'  finishing with NA are returned if the argument end_time is set a value above migr horizon.
#'
#' @export
#'
#' @examples
#' migr <- generate_toy_migration()
#' traj <- generate_trajectories( migr )

generate_trajectories <- function(migr, 
                                  traj = NULL,
                                  end_time = migr$horizon)
{
  stopifnot( class(migr) == "migration" )
  stopifnot(migr$sojourn_law_type == "Poisson")
  site_nb <- length(migr$site_name)
  bird_nb <- sum(migr$initial_state)
  stopifnot(nrow(migr$transition_law_param) == site_nb)
  stopifnot(ncol(migr$transition_law_param) == site_nb)

  traj <- generate_trajectories_C(migr, traj, end_time)

  return (traj)
}
