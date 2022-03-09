context("Check reestimate_migration_from_MCEMruns")

set.seed(123)
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
estim = reestimate_migration_from_MCEMruns(list(estimated_migr1, estimated_migr2))

expected_transition <- matrix(0, 5, 5)
expected_transition[1,] <- c(0,  0.9, 0.00000, 0.00000,  0.0)
expected_transition[2,] <- c(0,  0.0, 0.51542, 0.28458,  0.0)
expected_transition[3,] <- c(0,  0.0, 0.00000, 0.00000,  0.8)
expected_transition[4,] <- c(0,  0.0, 0.00000, 0.00000,  0.9)
expected_transition[5,] <- c(0,  0.0, 0.00000, 0.00000,  0.0)
expected_sojourn <- c(1.76090, 2.5716, 0.93505, 3.02180, 5.0000)

test_that("Estimated transitions parameters valid", {
  expect_equal(identical(signif(estim$estimation_method$output$transition_law_param, 5), expected_transition), TRUE)
})
test_that("Estimated sojourn durations parameters valid", {
  expect_equal(identical(signif(estim$estimation_method$output$sojourn_law_param, 5), expected_sojourn), TRUE)
})
