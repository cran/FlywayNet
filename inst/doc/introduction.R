## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(FlywayNet)

## -----------------------------------------------------------------------------
migr <- generate_toy_migration()
print(migr)

## -----------------------------------------------------------------------------
validate_migration( migr )

## ---- out.width="50%"---------------------------------------------------------
plot( migr )

## ---- out.width="50%"---------------------------------------------------------
plot_observedcounts( migr, migr$observation )

## -----------------------------------------------------------------------------
set.seed(123)  # just to be able to reproduce results
# We set verbose to FALSE because in this vignette the display
# is much more verbose than normal execution.
migr_ABC <- estimate_migration_ABC( migr, verbose=FALSE )  # takes several seconds, 32 steps required
print(migr_ABC$estimation_method$output$transition_law_param)
print(migr_ABC$estimation_method$output$sojourn_law_param)

## -----------------------------------------------------------------------------
set.seed(123)  # just to be able to reproduce results
migr_MCEM1 <- estimate_migration_MCEM( migr,
    log_transitions = TRUE, log_sojourns = TRUE, log_loglikelihood = TRUE) 
migr2 <- generate_modified_migration(migr, 
   sojourn_mode=c(rep("unif", 4), "no"), sojourn_domain=c(1,5), transition_mode="unif")
migr_MCEM2 <- estimate_migration_MCEM(migr2, MC_algo="MH",
    log_transitions = TRUE, log_sojourns = TRUE, log_loglikelihood = TRUE) 

## -----------------------------------------------------------------------------
migr_MCEM <- reestimate_migration_from_MCEMruns(list(migr_MCEM1, migr_MCEM2))  
print(migr_MCEM$estimation_method$output$transition_law_param)
print(migr_MCEM$estimation_method$output$sojourn_law_param)

## -----------------------------------------------------------------------------
set.seed(123)  # just to be able to reproduce results
traj <- generate_trajectories( migr )
migr_traj <- estimate_migration_from_trajectories(migr, traj)
print(migr_traj$estimation_method$output$transition_law_param)
print(migr_traj$estimation_method$output$sojourn_law_param)

## -----------------------------------------------------------------------------
set.seed(123)
L_ABC <- get_paramlikelihood( migr_ABC, migr$observation, n=10000)
print( L_ABC )
L_MCEM <- get_paramlikelihood( migr_MCEM, migr$observation, n=10000)
print( L_MCEM )
L_traj <- get_paramlikelihood( migr_traj, migr$observation, n=10000)
print( L_traj )

## ---- out.width="50%"---------------------------------------------------------
migr_MCEM$transition_law_param <- migr_MCEM$estimation_method$output$transition_law_param
migr_MCEM$sojourn_law_param<- migr_MCEM$estimation_method$output$sojourn_law_param
set.seed(123)  # just to be able to reproduce results
traj <- generate_trajectories( migr_MCEM )
plot_trajectories ( migr_MCEM, traj )

## -----------------------------------------------------------------------------
counts <- get_counts( migr_ABC, traj )

## -----------------------------------------------------------------------------
# write_migration (migr_ABC, 'migr_MCEM.txt')
# migr_ABC <- read_migration ('migr_MCEM.txt')

