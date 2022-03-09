#' Estimates migration parameters using an ABC method.
#' 
#' @description Estimates migration parameters using an ABC (Approximate Bayesian Approach) method.
#' Use the Lenormand method  (ABC_sequential method) from the EasyABC package.
#'
#' @param migr A migration structure. Required migration fields are: 
#'  site_name, link_knowledge, flight_duration, initial_state, horizon, 
#'  death_probability, observations. Field transition_law_param is also required 
#'  if the attribute estimate_transitions is set to FALSE. Field 
#'  sojourn_law_param is also required if the attribute estimate_sojourns 
#'  contains boolean values set to FALSE.
#' @param estimate_transitions If TRUE, transitions probabilities are estimated.
#'  Default value is TRUE.
#' @param estimate_sojourns Vector of boolean, identifies sites for which 
#'  mean sojourn times must be estimated. Default value is TRUE transformed in 
#'  TRUE for every site except for the last one (no need to estimate sojourn duration
#'  on the last site which is an arrival site).
#' @param sojourn_domain Range (min and max) of the sojourn time parameters.
#'   Possible values:
#'    (i) NULL, all intervals are set to [0, migr$horizon],
#'    (ii) vector of 2 values min and max then all domains are [min, max] interval,
#'    (iii) list of interval for each site.
#'   Note that are taken into account only intervals for sites with TRUE in estimate_sojourn arguments.
#'   Default value is  NULL.
#' @param only_likelihood Boolean. If TRUE, the vector of 
#'  statistics in ABC is composed of this log likelihood only. If FALSE, the 
#'  vector of statistics is composed of the full matrix (site*time)
#'  of simulated observations. Default is FALSE.
#' @param nb_simul Number of simulations divided by 2 to return in the 
#'   posterior distribution (see Lenormand method). A positive integer. Default value is 1000.
#' @param p_acc_min The Lenormand method parameter. A positive number between 0 and 1 (strictly). 
#'   This is the stopping criterion of the algorithm: a small number ensures a better convergence 
#'   of the algorithm, but at a cost in computing time.  Default value is 0.05. 
#' @param choice_method Name of method (in "mode", "mean", "median","density") to choose parameters
#'   from ABC distribution. Default is "mode".
#' @param venter_bw if choice_method is "mode", the bandwidth in [0, 1] to be used 
#'   with the modeest::venter method. Default value is 0.2.
#' @param n_cluster The number of cores used for simulation. Default value is 1.
#' @param verbose If TRUE, display iterations achievement. Default is TRUE.
#' 
#' @return The migration structure given with a new attribute 
#' \emph{estimation_method} which is a structure with 3 attributes:
#' 
#'               . \emph{name} = "ABC",
#'
#'               . \emph{settings}, a structure with attributes:
#'                    estimate_transitions,
#'                    estimate_sojourns, 
#'                    sojourn_domain, \cr
#'                    nb_simul,
#'                    only_likelihood,
#'                    p_acc_min,
#'                    choice_method,
#'                    venter_bw,
#'                    n_cluster, 
#'
#'               . \emph{output}, a structure with attributes:
#'               
#'                    - transition_law_param: estimated transition parameters,
#'                    
#'                    - sojourn_law_param: estimated sojourn parameters,
#'                    
#'                    - log_ABC: see EasyABC::ABC_sequential documentation 
#'                      for attributes param, stats, stats_normalization, weigths, 
#'                      epsilon, nsim, computime,
#'                    
#'                    - log_param_dist: distribution of free estimated parameters 
#'                      (dataframe size nb_simul/2 x number of estimated free parameters),
#'                    
#'                    - log_param_def: free parameters estimated with their initial values.
#' @export
#'
#' @examples
#' migr <- generate_toy_migration()
#' # Argument nb_simul just provide to reduce running time
#' estimated_migr <- estimate_migration_ABC( migr, nb_simul = 30 ) 
#' print( estimated_migr$estimation_method$output$transition_law_param )
#' estimated_migr <- estimate_migration_ABC( migr, nb_simul = 40 , sojourn_domain = c(2,4), 
#'                                          estimate_sojourns = c(TRUE, TRUE, TRUE, FALSE, FALSE))
#' print( estimated_migr$estimation_method$output$transition_law_param )


estimate_migration_ABC <- function(migr, 
                                   estimate_transitions = TRUE, 
                                   estimate_sojourns = TRUE,
                                   sojourn_domain = NULL, 
                                   nb_simul = 1000, 
                                   only_likelihood = FALSE,
                                   p_acc_min = 0.05,
                                   choice_method = "mode",
                                   venter_bw = 0.2,
                                   n_cluster = 1,
                                   verbose = TRUE)
{
  stopifnot( class(migr) == "migration" )
  stopifnot(migr$transition_law_type == "multinomial")
  stopifnot(migr$sojourn_law_type == "Poisson")
  stopifnot(choice_method == "mode" | choice_method == "mean" 
            | choice_method == "median" | choice_method == "density")
  stopifnot(is.numeric(venter_bw) & venter_bw>=0 & venter_bw<=1 )
  I <- length(migr$site_name)
  if (length(estimate_sojourns)==1) {
    if (estimate_sojourns==TRUE) {estimate_sojourns <- c(rep(TRUE, I-1), FALSE)}
  } else {stopifnot(length(estimate_sojourns)==I)}
  if (is.null(sojourn_domain)) {
    min_sojourn_domain <- rep(0, 1, I)
    max_sojourn_domain <- rep(migr$horizon, 1, I)
  } else {
    if (length(sojourn_domain)==2) {
      min_sojourn_domain <- rep( sojourn_domain[1], 1, I)
      max_sojourn_domain <- rep( sojourn_domain[2], 1, I)
    } else {
      stopifnot( length(sojourn_domain) == I )
      min_sojourn_domain <- rep(0, 1, I)
      max_sojourn_domain <- rep(0, 1, I)
      for (i in 1:I) {
        min_sojourn_domain[i] <- sojourn_domain[[i]][1]
        max_sojourn_domain[i] <- sojourn_domain[[i]][2]
      }
    }
  }
    
  ## model function, assign values to migration structure, simulate and compute stats
  abc_model_fun <- function(vals)
  {
    vals_int <- NULL
    if (n_cluster > 1) {
      set.seed(vals[1])
      vals_int <- vals[2:length(vals)]
    } else {
      vals_int <- vals
    }
    # assign values to simulate
    intMigr <- set_freeparametersvalue(migr, vals_int, estimate_transitions, 
                                       estimate_sojourns, use_adhoc_dirichlet=TRUE)

    #simulate trajectoris 
    if (only_likelihood) {
      ll = get_paramlikelihood(intMigr, intMigr$observation, n_simul=10) #100
      return(sqrt(-ll))
    } else {
      #simulate observations and return the full matrix of observations
      sim_trajs =  generate_trajectories_C(intMigr)
      sim_obs <- generate_observedcounts(intMigr, sim_trajs)
      return(abc_get_stat_full(intMigr, sim_obs))
    }
  }

  prior_dist <- abc_get_prior(migr, estimate_transitions, estimate_sojourns, min_sojourn_domain, max_sojourn_domain)
  summary_stat_opt <- NULL
  if (only_likelihood) {
    summary_stat_opt = 0
  } else {
    summary_stat_opt = abc_get_stat_full(migr, migr$observation)
  }

  ABC_out <- EasyABC::ABC_sequential(method = "Lenormand", 
                                    model = abc_model_fun,
                                    prior = prior_dist, nb_simul=nb_simul,
                                    summary_stat_target = summary_stat_opt,
                                    n_cluster = n_cluster,
                                    use_seed = (n_cluster>1),
                                    verbose = FALSE,
                                    progress_bar = verbose,
                                    p_acc_min = p_acc_min,
                                    inside_prior=TRUE)
  
  migr_output <- migr
  
  get_mode <- function (x) {dstx <- stats::density(x); return(dstx$x[ which(dstx$y == max(dstx$y))])}
  if (choice_method == "mode") {
    log_param_distr = t(apply(ABC_out$param, MARGIN=1, 
                              function(x){
                                migr_x <- set_freeparametersvalue(migr_output, x,  
                                                                  estimate_transitions, 
                                                                  estimate_sojourns, 
                                                                  use_adhoc_dirichlet=TRUE)
                                return(get_freeparametersvalue(migr_x, estimate_transitions, 
                                                               estimate_sojourns, 
                                                               use_adhoc_dirichlet=FALSE))
                              } ))
    # Chose Venter mode value in each distribution
    est_d <- matrix(0, 1, dim(log_param_distr)[2])
    for (i in 1:dim(log_param_distr)[2]) {
      est_d[i] <- modeest::mlv(log_param_distr[ ,i], method = "venter", bw = venter_bw, type = 3)
    }  
  }
  else if (choice_method == "median") {est_d <- apply(ABC_out$param, MARGIN=2, stats::median)}
  else if (choice_method == "mean") {est_d <- apply(ABC_out$param, MARGIN=2, mean)}
  else if (choice_method == "density") {est_d <- apply(ABC_out$param, MARGIN=2, get_mode)} 
  
  migr_output <- set_freeparametersvalue(migr_output, est_d, estimate_transitions, 
                                         estimate_sojourns, use_adhoc_dirichlet=TRUE)
  #validate_migration(migr_output) TODO do we need renormalization ?
  
  migr$estimation_method$name <- "ABC"
  migr$estimation_method$settings <- list(estimate_transitions = estimate_transitions,
                                          estimate_sojourns = estimate_sojourns,
                                          sojourn_domain = sojourn_domain,
                                          nb_simul = nb_simul,
                                          only_likelihood = only_likelihood,
                                          p_acc_min = p_acc_min,
                                          choice_method = choice_method,
                                          venter_bw = venter_bw,
                                          n_cluster = n_cluster)
  migr$estimation_method$output <- list(transition_law_param = migr_output$transition_law_param,
                                        sojourn_law_param = migr_output$sojourn_law_param,
                                        log_ABC = ABC_out,
                                        log_param_distr = t(apply(ABC_out$param, MARGIN=1, 
                                                                          function(x){
                                                                            migr_x <- set_freeparametersvalue(migr_output, x,  
                                                                                                              estimate_transitions, 
                                                                                                              estimate_sojourns, 
                                                                                                              use_adhoc_dirichlet=TRUE)
                                                                            return(get_freeparametersvalue(migr_x, estimate_transitions, 
                                                                                                           estimate_sojourns, 
                                                                                                           use_adhoc_dirichlet=FALSE))
                                                                          } )), 
                                        log_param_def = get_freeparametersvalue(migr, estimate_transitions,
                                                                                        estimate_sojourns, use_adhoc_dirichlet=FALSE) )          
  return(migr)
}


####
## compute the prior list
##
## @param migr, a migration structure
## @param estimate_transitions :boolean, should transitions be estimated ?
## @param estimate_sojourns : vector of boolean, identifies which sojourn times
##   should be estimated
## @param min_sojourn_domain : min (params of the uniform distrib) of the sojourn mean time parameters.
## @param max_sojourn_domain : max (params of the uniform distrib) of the sojourn mean time parameters.
####
abc_get_prior<-function(migr, estimate_transitions, estimate_sojourns, min_sojourn_domain, max_sojourn_domain)
{
  I <- length(migr$site_name)
  prior_dist <- NULL

  if (estimate_transitions){
    for (i in 1:(I-2)) {
      which_toset <- which(migr$link_knowledge[i,])
      for (j in which_toset[-length(which_toset)]){
        trans_name <- paste(paste(migr$site_name[i], sep=""), "to",
                           paste(migr$site_name[j], sep=""), sep="_")
        prior_dist[[trans_name]] <- c("unif", "0", "1")
      }
    }
  }
  for (i in 1:I) {
    if (estimate_sojourns[i]){
      prior_dist[[paste("soj", i, sep="_")]] <- c("unif",
        as.character(min_sojourn_domain[i]), as.character(max_sojourn_domain[i]))
    }
  }
  return(prior_dist);
}

####
## computes statistics vector from an observation matrix of birds
## (as generated by generate_observedcounts)
## It computes statistics vector in ABC from the specified migration
###
abc_get_stat_full<- function(migr, obs)
{
  I <- length(migr$site_name)
  notna_obs <- !is.na(c(migr$observation[1:I, 2:ncol(migr$observation)]))
  return (c(obs[1:I, 2:ncol(obs)])[notna_obs]);
}


######
# Problem statement and adhoc dirichlet as a solution
#
# Let :
#
# Total number of sites  : I (constant)
# the starting site i < I (constant)
# the death probability of site i : di (constant)
# the transition probability between i and j : pij
#
# Then we must have a multinomial probability distribution that respects :
#
# pi1 + ... + piI + di = 1
#
# But this is graph without cycle and without self transition, so we have :
#
# pi1 = ... = pii = 0
# pi(i+1) + ... + piI + di = 1
#
# We only have to provide pi(i+1), ..., pi(I-1) since piI can be deduced
# from all the other values in order to sum to 1.
# So we need to provide only I-i-1 values.
#
# If we use a Uniform[0,1]^(I-i-1) to draw directly pi(i+1), ...,pi(I-1)
# then the sum pi1 +...+ piI can be different from 1-di.
#
# And preprocessing the sequence pi(i+1),..., pi(I-1) by normalizing it in
# order to ensure that all sum to 1 would lead to undecidable
# algorithm since for any k the sequence k*(pi(i+1),..., pi(I-1))
# would lead to the same preprocessed multinomial ditribution probability.
#
# So let define, for j st. i < j < I:
#   qij = pij / (1-di - (pi1+...+pi(j-1)) and equivalently
#   pij = qij * (1-di - (pi1+...+pi(j-1))
#
# Note that pij can be computed only incrementaly on j from qij.
#
# This way if we draw qi(i+1), ..., qi(I-1) in a Uniform^[0,1]^(I-i-1)
# it leads to a sequence  pi1, ..., piI that sum to 1-di.
# below two functions that convert q to p and inversely p to q
#
#######
## converts q to p,
## @param I : total number of sites
## @param i : current site
## @param di : death probability on site i
## @param qvec : uniform drawning from [0,1]^(I-i-1)
## @return pvec : multinomial probability distributionof length I
##                (must sum to 1-di)
###
abc_q_to_p <- function(I, i,  di, qvec)
{
  pvec <- c(rep(0, i), rep(NA, I-i))
  sum_pik <- 0;
  for (j in (i+1):(I-1)) {
    pvec[j] <- qvec[j-i]*(1-di-sum_pik)
    sum_pik <- sum_pik+pvec[j]
  }
  pvec[I] <- 1-di-sum_pik
  return (pvec)
}

####
## converts p to q,
## @param I : total number of sites
## @param i : current site
## @param di : death probability on site i
## @param pvec : multinomial probability distribution of length I
##               (must sum to 1-di)
## @param qvec : in [0,1]^(I-i-1)
###
abc_p_to_q <- function(I, i,  di, pvec)
{
  qvec<-rep(NA, I-i-1)
  for (j in (i+1):(I-1)) {
    qvec[j-i] <- pvec[j]/(1-sum(pvec[1:(j-1)])-di)
  }
  return (qvec)
}



