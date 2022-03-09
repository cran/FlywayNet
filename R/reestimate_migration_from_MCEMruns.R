#' Estimates migration parameters from multiple MCEM runs
#' 
#' @description Estimates migration parameters using multiple runs of MCEM. 
#' Since MCEM is a local optimization process, different initialisation of 
#' the parameters can lead to different estimates. Based on log likelihood, 
#' this function searches a posteriori for the convergence of each MCEM run and 
#' returns the results of the best MCEM run.
#' Convergence of a MCEM run is considered reached when difference in log likelihood 
#' is below \emph{loglikelihood_threshold} during  \emph{nb_iterations_threshold}  
#' consecutive iterations.
#' @param mcem_runs List of mcem runs as returned by estimate_migration_MCEM
#' @param loglikelihood_threshold Threshold on loglikelihood difference between 
#'  two consecutive iterations
#' @param nb_iterations_threshold Number of consecutive iterations with a loww
#'  difference in loglikelihood. 
#' @return The migration structure given with a new attribute 
#' 
#' \emph{estimation_method} which is a structure with 3 attributes:
#' 
#'               . \emph{name} = "MCEM_combined",
#'
#'               . \emph{settings}, a structure with attributes:
#'                   loglikelihood_threshold,
#'                   nb_iterations_threshold.
#'
#'               . \emph{output}, a structure with attributes:
#'               
#'                   - transition_law_param: estimated transition parameters, 
#'                   
#'                   - sojourn_law_param: estimated sojourn parameters,
#'                    
#'                   - index_run_max: maximum number of iteration for convergence,
#'                    
#'                   - nb_simulations_total: total number of simulations,
#'                    
#'                  - nb_simulations_at_convergence: number of simulations useful at convergence,
#'                    
#'                  - summary: a summary.
#' @export
#'
#' @examples
#' migr1 <- generate_toy_migration()
#' estimated_migr1 <- estimate_migration_MCEM(migr1, MC_algo="MH",   
#'                                           log_transitions = TRUE,
#'                                           log_sojourns = TRUE,
#'                                           log_loglikelihood = TRUE) 
#' migr2 <- generate_modified_migration(migr1, 
#'   sojourn_mode=c(rep("unif", 4), "no"),
#'   sojourn_domain=c(1,5),
#'   transition_mode="unif")
#' estimated_migr2 <- estimate_migration_MCEM(migr2, MC_algo="MH",
#'                                           log_transitions = TRUE,
#'                                           log_sojourns = TRUE,
#'                                           log_loglikelihood = TRUE) 
#' estimated_migr = reestimate_migration_from_MCEMruns(list(estimated_migr1, estimated_migr2))
#' print( estimated_migr$estimation_method$output$transition_law_param )


reestimate_migration_from_MCEMruns <- function(mcem_runs, 
                                               loglikelihood_threshold = 10, 
                                               nb_iterations_threshold = 5)
{
  summary = NULL;
  for (mcem_i in 1:length(mcem_runs)){
    mcem_r = mcem_runs[[mcem_i]]
    seq_loglike = unlist(mcem_r$estimation_method$output$log_loglikelihood)
    itermax = length(seq_loglike)
    inf_threshold = (seq_loglike[2:itermax] - 
                       seq_loglike[1:(itermax-1)]) < 
      loglikelihood_threshold
    nb_iterations = 0;
    iter_conv = NULL;
    loglikelihood_conv = NULL;
    for (i in 1:length(inf_threshold)) {
      if (inf_threshold[i]){
        nb_iterations = nb_iterations+1
      } else {
        nb_iterations = 0
      }
      if ((nb_iterations == nb_iterations_threshold)  & is.null(iter_conv)){
        iter_conv = i+1
        loglikelihood_conv = seq_loglike[i+1]
      }
    }
    if (is.null(iter_conv)) {
      iter_conv = itermax
      loglikelihood_conv = seq_loglike[itermax]
    }
    summary = rbind(summary, c(
      mcem_run=mcem_i, iter_conv=iter_conv, 
      loglikelihood_conv=loglikelihood_conv,
      nb_simu_tot = mcem_r$estimation_method$settings$itermax*
        mcem_r$estimation_method$settings$nb_particles*
        mcem_r$estimation_method$settings$MH_transition_length,
      nb_simu_at_conv = iter_conv*
        mcem_r$estimation_method$settings$nb_particles*
        mcem_r$estimation_method$settings$MH_transition_length))
  }
  summary=as.data.frame(summary)
  index_run_max = summary[which.max(summary$loglikelihood_conv), "mcem_run"]
  iter_conv_best = summary[which.max(summary$loglikelihood_conv), "iter_conv"]
  mcem_best = mcem_runs[[index_run_max]]
  transition_law_param = 
    mcem_best$estimation_method$output$log_transitions[[iter_conv_best]]
  sojourns_law_param = 
    mcem_best$estimation_method$output$log_sojourns[[iter_conv_best]]
  
  mcem_ret = mcem_runs[[1]]
  mcem_ret$estimation_method = list(
    name = "MCEM_combined",
    settings = list(nb_mcem_runs = length(mcem_runs),
                    loglikelihood_threshold = loglikelihood_threshold, 
                    nb_iterations_threshold = nb_iterations_threshold),
    output = list(
      transition_law_param=transition_law_param, 
      sojourn_law_param=sojourns_law_param,
      index_run_max=index_run_max,
      nb_simulations_total = sum(summary$nb_simu_tot),
      nb_simulations_at_convergence = sum(summary$nb_simu_at_conv),
      summary=summary))
  return(mcem_ret)
}
