% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reestimate_migration_from_MCEMruns.R
\name{reestimate_migration_from_MCEMruns}
\alias{reestimate_migration_from_MCEMruns}
\title{Estimates migration parameters from multiple MCEM runs}
\usage{
reestimate_migration_from_MCEMruns(
  mcem_runs,
  loglikelihood_threshold = 10,
  nb_iterations_threshold = 5
)
}
\arguments{
\item{mcem_runs}{List of mcem runs as returned by estimate_migration_MCEM}

\item{loglikelihood_threshold}{Threshold on loglikelihood difference between 
two consecutive iterations}

\item{nb_iterations_threshold}{Number of consecutive iterations with a loww
difference in loglikelihood.}
}
\value{
The migration structure given with a new attribute 

\emph{estimation_method} which is a structure with 3 attributes:

              . \emph{name} = "MCEM_combined",

              . \emph{settings}, a structure with attributes:
                  loglikelihood_threshold,
                  nb_iterations_threshold.

              . \emph{output}, a structure with attributes:
              
                  - transition_law_param: estimated transition parameters, 
                  
                  - sojourn_law_param: estimated sojourn parameters,
                   
                  - index_run_max: maximum number of iteration for convergence,
                   
                  - nb_simulations_total: total number of simulations,
                   
                 - nb_simulations_at_convergence: number of simulations useful at convergence,
                   
                 - summary: a summary.
}
\description{
Estimates migration parameters using multiple runs of MCEM. 
Since MCEM is a local optimization process, different initialisation of 
the parameters can lead to different estimates. Based on log likelihood, 
this function searches a posteriori for the convergence of each MCEM run and 
returns the results of the best MCEM run.
Convergence of a MCEM run is considered reached when difference in log likelihood 
is below \emph{loglikelihood_threshold} during  \emph{nb_iterations_threshold}  
consecutive iterations.
}
\examples{
migr1 <- generate_toy_migration()
estimated_migr1 <- estimate_migration_MCEM(migr1, MC_algo="MH",   
                                          log_transitions = TRUE,
                                          log_sojourns = TRUE,
                                          log_loglikelihood = TRUE) 
migr2 <- generate_modified_migration(migr1, 
  sojourn_mode=c(rep("unif", 4), "no"),
  sojourn_domain=c(1,5),
  transition_mode="unif")
estimated_migr2 <- estimate_migration_MCEM(migr2, MC_algo="MH",
                                          log_transitions = TRUE,
                                          log_sojourns = TRUE,
                                          log_loglikelihood = TRUE) 
estimated_migr = reestimate_migration_from_MCEMruns(list(estimated_migr1, estimated_migr2))
print( estimated_migr$estimation_method$output$transition_law_param )
}
