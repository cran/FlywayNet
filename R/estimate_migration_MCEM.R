#' Estimates migration parameters using an MCEM method.
#' 
#' @description Estimates migration parameters using an MCEM (Monte Carlo Expectation–Maximization) method.
#' 
#' @param migr A migration structure. Required migration fields are: 
#'  site_name, link_knowledge, flight_duration, initial_state, horizon, 
#'  death_probability, observations. Field \emph{transition_law_param} is also required 
#'  if the attribute estimate_transitions is set to FALSE or the 
#'  start_transitions argument is set to NULL. Field 
#'  \emph{sojourn_law_param} is also required if the argument estimate_sojourns 
#'  contains boolean values set to FALSE or if start_sojourns is set to NULL.
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
#' @param start_transitions Matrix of transition probabilities between sites
#'  used as initialization in the MCEM local optimization procedure. 
#'  If NULL, the initialization of the transition matrix is the one provided 
#'  into \emph{migr}. Default is NULL.
#' @param start_sojourns Vector of mean sojourn times used as initialization in
#' the MCEM local optimization procedure. If NULL, the initialization of 
#' the mean sojourn times are the one provided into migr. 
#'  Default is NULL.
#' @param nb_particles The number of particles of the SIS resampling method or 
#'  the length of the Markov Chain of the MH method. Default is 100.
#' @param itermax The number of iterations of the EM algorithm.
#'  Default is 10.
#' @param MC_algo Its value is either 'SIS' or 'MH', it defines the Monte Carlo
#'  algorithm used to sample from the current a posteriori distribution of the 
#'  trajectories. If MC_algo='SIS' then the Sequential Importance Sampling algorithm 
#'  is used. If MC_algo='MH' the Metropolis Hastings algorithm is used. Default is 'MH'.
#' @param MH_neighborhood Probability of updating an individual bird trajectory.
#'  It defines the candidate generation function in the generic MH algorithm.
#'  Available only for MC_algo='MH'. Default is 0.1.
#' @param MH_transition_length Length of a transition (in number of candidates)
#'  for the MH method. Available only for MC_algo='MH'. Default is 1.
#' @param log_transitions If TRUE, transitions probabilities are logged 
#'  for each iteration. Default is FALSE.
#' @param log_sojourns If TRUE, mean sojourn times are logged for each iteration.
#'  Default is FALSE.
#' @param log_loglikelihood If TRUE, log-likelihood of parameters is logged. 
#' For each iteration, it is computed from the nb_particles simulations
#' resulting from the expectation step. Default is FALSE.
#' @param log_sel_particles If TRUE, the percentage of selected particles 
#'  for each time step is logged at each EM iteration. 
#'  It is available only for MC_algo='SIS'. Default is FALSE.
#' @param log_acceptance_rate If TRUE, the percentage of accepted draws
#'  during the Metropolis Hastings chain simulation is logged at 
#'  each EM iteration. It is available only for MC_algo='MH'. Default is FALSE.
#' @param verbose If TRUE, display iterations achievement. Default is TRUE.
#' @return The migration structure given with a new attribute 
#' \emph{estimation_method} which is a structure with 3 attributes:
#' 
#'               . \emph{name} = "MCEM",
#'
#'               . \emph{settings}, a structure with attributes:
#'                   estimate_transitions,
#'                   estimate_sojourns, 
#'                   start_transitions,
#'                   start_sojourns
#'                   nb_particles,
#'                   itermax,
#'                   MC_algo, 
#'                   MH_neighborhood, 
#'                   MH_transition_length, 
#'                   log_transitions, 
#'                   log_sojourns,
#'                   log_loglikelihood,
#'                   log_sel_particles, 
#'                   log_acceptance_rate,
#'
#'               . \emph{output}, a structure with attributes:
#'               
#'                   - transition_law_param: estimated transition parameters, 
#'                   
#'                   - sojourn_law_param: estimated sojourn parameters,
#'                    
#'                   - log_transitions: log of transition parameters,
#'                   
#'                   - log_sojourns: log of sojourn parameters,
#'                   
#'                   - log_loglikelihood: log of likelihood,
#'                   
#'                   - log_sel_particles: log of percentage of selected particles,
#'                   
#'                   - log_acceptance_rate: log of percentage of accepted draws
#'                    during the Metropolis Hastings chain simulation.
#' @export
#'
#' @examples
#' migr <- generate_toy_migration()
#' estimated_migr <- estimate_migration_MCEM( migr ) 
#' print( estimated_migr$estimation_method$output$transition_law_param )
#' estimated_migr <- estimate_migration_MCEM( migr, sojourn_domain = c(2,4), itermax = 15,
#'                                          estimate_sojourns = c(TRUE, TRUE, TRUE, FALSE, FALSE))
#' print( estimated_migr$estimation_method$output$transition_law_param )

estimate_migration_MCEM <- function(migr, 
                                   estimate_transitions = TRUE, 
                                   estimate_sojourns = TRUE,
                                   sojourn_domain = NULL,
                                   start_transitions = NULL,
                                   start_sojourns = NULL,
                                   nb_particles = 100, 
                                   itermax = 10,
                                   MC_algo = "MH",
                                   MH_neighborhood = 0.1,
                                   MH_transition_length = 1,
                                   log_transitions = FALSE,
                                   log_sojourns = FALSE,
                                   log_loglikelihood = FALSE,
                                   log_sel_particles = FALSE,
                                   log_acceptance_rate = FALSE,
                                   verbose = TRUE)
{
  stopifnot( class(migr) == "migration" )
  stopifnot(migr$transition_law_type == "multinomial")
  stopifnot(migr$sojourn_law_type == "Poisson")
  stopifnot(MC_algo == "SIS" | MC_algo == "MH")
  
  I <- length(migr$site_name)
  if (length(estimate_sojourns)==1) {
    if (estimate_sojourns==TRUE) {estimate_sojourns <- c(rep(TRUE, I-1), FALSE)}
  } else {stopifnot(length(estimate_sojourns)==I)}
  if (is.null(sojourn_domain)) {
    sojourn_domain <- lapply(1:I, function(x){c(0, migr$horizon)})
  } else if (is.numeric(sojourn_domain)) {
    sojourn_domain <- lapply(1:I, function(x){sojourn_domain})
  } else if (!(is.list(sojourn_domain) & length(sojourn_domain) == I)) {
    stop("error with the parameter sojourn_domain")
  }
  
  migr <- estimate_migration_MCEM_C(migr,
                                    estimate_transitions = estimate_transitions, 
                                    estimate_sojourns = estimate_sojourns,
                                    sojourn_domain = sojourn_domain,
                                    start_transitions = start_transitions,
                                    start_sojourns = start_sojourns,
                                    nb_particles = nb_particles, 
                                    itermax = itermax,
                                    MC_algo = MC_algo,
                                    MH_neighborhood = MH_neighborhood,
                                    MH_transition_length = MH_transition_length,
                                    log_transitions = log_transitions,
                                    log_sojourns = log_sojourns,
                                    log_loglikelihood = log_loglikelihood,
                                    log_sel_particles = log_sel_particles,
                                    log_acceptance_rate = log_acceptance_rate,
                                    verbose = verbose)
  class(migr) <- "migration"
  return(migr)
}


