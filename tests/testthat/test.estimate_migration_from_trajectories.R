context("Check estimate_migration_from_trajectories")

set.seed(123)
migr <- generate_random_migration( nb_sites = 3 )
set.seed(123)
traj <- generate_trajectories( migr )
set.seed(123)
estim <- estimate_migration_from_trajectories( migr, 
                                               trajectories = traj,
                                               estimate_transitions = TRUE, 
                                               estimate_sojourns = TRUE,
                                               sojourn_domain = NULL,
                                               min_prob=0)

expected_transition <- matrix(0, 3, 3)
expected_transition[1,] <- c(0, 0.49, 0.48)
expected_transition[2,] <- c(0, 0.00, 0.97)
expected_transition[3,] <- c(0, 0.00, 0.00)
expected_sojourn <- c(5.28, 6.3673,   11)

test_that("Estimated transitions parameters valid", {
  expect_equal(identical(signif(estim$estimation_method$output$transition_law_param, 5), expected_transition), TRUE)
})
test_that("Estimated sojourn durations parameters valid", {
  expect_equal(all(signif(estim$estimation_method$output$sojourn_law_param, 5) == expected_sojourn), TRUE)
})


migr <- generate_toy_migration() ####################################################################
set.seed(123)
traj <- generate_trajectories( migr )
set.seed(123)
estim <- estimate_migration_from_trajectories( migr, 
                                               trajectories = traj,
                                               estimate_transitions = TRUE, 
                                               estimate_sojourns = TRUE,
                                               sojourn_domain = NULL,
                                               min_prob=0) 

expected_transition <- matrix(0, 5, 5)
expected_transition[1,] <- c(0,  0.9, 0.00000, 0.00000,  0.0)
expected_transition[2,] <- c(0,  0.0, 0.48205, 0.31795,  0.0)
expected_transition[3,] <- c(0,  0.0, 0.00000, 0.00000,  0.8)
expected_transition[4,] <- c(0,  0.0, 0.00000, 0.00000,  0.9)
expected_transition[5,] <- c(0,  0.0, 0.00000, 0.00000,  0.0)
expected_sojourn <- c(2.9100, 4.3368, 1.9362, 3.7097, 5.0000)

test_that("Estimated transitions parameters valid", {
  expect_equal(identical(signif(estim$estimation_method$output$transition_law_param, 5), expected_transition), TRUE)
})
test_that("Estimated sojourn durations parameters valid", {
  expect_equal(identical(signif(estim$estimation_method$output$sojourn_law_param, 5), expected_sojourn), TRUE)
})
