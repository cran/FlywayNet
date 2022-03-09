#' Estimates migration parameters using trajectories.
#' 
#' @description Estimates migration parameters using trajectories.
#' 
#' @param migr A migration structure. Required migration fields are: 
#'  site_name, link_knowledge, flight_duration, initial_state, horizon, 
#'  death_probability, observations. Field \emph{transition_law_param} is also required 
#'  if the attribute estimate_transitions is set to FALSE or the 
#'  start_transitions argument is set to NULL. Field 
#'  \emph{sojourn_law_param} is also required if the argument estimate_sojourns 
#'  contains boolean values set to FALSE or if start_sojourns is set to NULL.
#' @param trajectories A matrix of birds trajectories.
#' @param estimate_transitions If TRUE, transitions probabilities are estimated.
#'  Default value is TRUE.
#' @param estimate_sojourns Vector of bool, identifies states for which 
#'  sojourn mean times must be estimated. Default value is TRUE transformed in 
#'  TRUE for every site except for the last one.
#' @param sojourn_domain Range (min and max) of the sojourn mean time parameters.
#'   Possible values:
#'    (i) NULL, all intervals are set to [0, migr$horizon],
#'    (ii) vector of 2 values min and max then all domains are [min, max] interval,
#'    (iii) list of interval for each site.
#'   Note that are taken into account only intervals for sites with TRUE in estimate_sojourn arguments.
#'   Default value is  NULL.
#' @param min_prob A threshold on the minimal value of the transition 
#'   probabilities. Used, for example, in MCEM to avoid degeneracy. Default is 0. 
#' @return The migration structure given with a new attribute 
#' \emph{estimation_method} which is a structure with 3 attributes:
#' 
#'               . \emph{name} = "from_trajectories",
#'
#'               . \emph{settings}, a structure with attributes:
#'                   estimate_transitions,
#'                   estimate_sojourns, 
#'
#'               . \emph{output}, a structure with attributes:
#'               
#'                     - transition_law_param: estimated transition parameters,
#'                     
#'                     - sojourn_law_param: estimated sojourn parameters.
#' @export
#'
#' @examples
#' migr <- generate_toy_migration()
#' traj <- generate_trajectories( migr )
#' estimated_migr <- estimate_migration_from_trajectories( migr, traj ) 
#' print( estimated_migr$estimation_method$output$transition_law_param )

estimate_migration_from_trajectories <- function(
                                   migr, trajectories,
                                   estimate_transitions = TRUE, 
                                   estimate_sojourns = TRUE,
                                   sojourn_domain = NULL,
                                   min_prob=0)
{
  stopifnot( class(migr) == "migration" )
  stopifnot(migr$transition_law_type == "multinomial")
  stopifnot(migr$sojourn_law_type == "Poisson")
  
  if (length(estimate_sojourns) == 1 && estimate_sojourns == TRUE) {
    estimate_sojourns <- c(rep(TRUE, length(migr$site_name)-1), FALSE);
  }
  if (is.null(sojourn_domain)) {
    sojourn_domain <- lapply(1:length(migr$site_name), 
                             function(x){c(0, migr$horizon)})
  } else if (is.numeric(sojourn_domain)) {
    sojourn_domain <- lapply(1:length(migr$site_name), 
                             function(x){sojourn_domain})
  } else if (!(is.list(sojourn_domain) &
             length(sojourn_domain) == length(migr$site_name))) {
    stop("error with the parameter sojourn_domain")
  }
  
  migr <- estimate_migration_from_trajectories_C(migr, trajectories,
                                    estimate_transitions = estimate_transitions, 
                                    estimate_sojourns = estimate_sojourns,
                                    sojourn_domain = sojourn_domain, 
                                    min_prob=min_prob)
  
  # migr$estimation_method$name <- "from_trajectories"
  # migr$estimation_method$settings <- list(estimate_transitions = estimate_transitions,
  #                                         estimate_sojourns = estimate_sojourns,
  #                                         sojourn_domain = sojourn_domain)
  # migr$estimation_method$output <- list(transition_law_param = migr$output_transition_law_param,
  #                                         sojourn_law_param = migr$output_sojourn_law_param)
  class(migr) <- "migration"
  return(migr)
}


