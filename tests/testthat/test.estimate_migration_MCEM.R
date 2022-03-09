context("Check estimate_migration_MCEM")

set.seed(123)
migr <- generate_random_migration( nb_sites = 3 )
set.seed(123)
estim_SIS <- estimate_migration_MCEM( migr, 
                                      estimate_transitions = TRUE,
                                      estimate_sojourns = TRUE,
                                      itermax = 10, 
                                      MC_algo = "SIS",
                                      log_transitions = FALSE,
                                      log_sojourns = FALSE,
                                      log_sel_particles = FALSE,
                                      log_acceptance_rate = FALSE,
                                      verbose = FALSE)
set.seed(123)
estim_MH <- estimate_migration_MCEM( migr, 
                                     estimate_transitions = TRUE,
                                     estimate_sojourns = TRUE,
                                     sojourn_domain = list(c(1,31), c(1,31), c(-9,-9)),
                                     itermax = 10,
                                     MC_algo = "MH",
                                     nb_particles = 10,
                                     MH_neighborhood = 0.1,
                                     MH_transition_length = 1,
                                     log_transitions = FALSE,
                                     log_sojourns = FALSE,
                                     log_sel_particles = FALSE,
                                     log_acceptance_rate = FALSE,
                                     verbose = FALSE)

expected_transition_SIS <- matrix(0, 3, 3)
expected_transition_SIS[1,] <- c(0, 0.53, 0.44)
expected_transition_SIS[2,] <- c(0, 0.00, 0.97)
expected_transition_SIS[3,] <- c(0, 0.00, 0.00)
expected_sojourn_SIS <- c(5.45, 5.9611,   11)
expected_transition_MH <- matrix(0, 3, 3)
expected_transition_MH[1,] <- c(0, 0.55065, 0.41935)
expected_transition_MH[2,] <- c(0, 0.0000, 0.97000)
expected_transition_MH[3,] <- c(0, 0.0000, 0.00000)
expected_sojourn_MH <- c(5.858, 7.61, 11 )

test_that("Estimated SIS transitions parameters valid", {
  expect_equal(identical(signif(estim_SIS$estimation_method$output$transition_law_param, 5), expected_transition_SIS), TRUE)
})
test_that("Estimated SIS sojourn durations parameters valid", {
  expect_equal(all(signif(estim_SIS$estimation_method$output$sojourn_law_param, 5) == expected_sojourn_SIS), TRUE)
})

test_that("Estimated MH transitions parameters valid", {
  expect_equal(identical(signif(estim_MH$estimation_method$output$transition_law_param, 5), expected_transition_MH), TRUE)
})
test_that("Estimated MH sojourn durations parameters valid", {
  expect_equal(all(signif(estim_MH$estimation_method$output$sojourn_law_param, 5) == expected_sojourn_MH), TRUE)
})


migr <- generate_toy_migration() ####################################################################
set.seed(123)
estim_SIS <- estimate_migration_MCEM( migr, 
                                              estimate_transitions = TRUE,
                                              estimate_sojourns = TRUE,
                                              itermax = 10,
                                              MC_algo = "SIS",
                                              log_transitions = FALSE,
                                              log_sojourns = FALSE,
                                              log_sel_particles = FALSE,
                                              log_acceptance_rate = FALSE,
                                              verbose = FALSE)
set.seed(123)
estim_MH <- estimate_migration_MCEM( migr, 
                                             estimate_transitions = TRUE,
                                             estimate_sojourns = TRUE,
                                             itermax = 10,
                                             MC_algo = "MH",
                                             nb_particles = 10,
                                             MH_neighborhood = 0.1,
                                             MH_transition_length = 1,
                                             log_transitions = FALSE,
                                             log_sojourns = FALSE,
                                             log_sel_particles = FALSE,
                                             log_acceptance_rate = FALSE,
                                             verbose = FALSE)

expected_transition_SIS <- matrix(0, 5, 5)
expected_transition_SIS[1,] <- c(0,  0.9, 0.00000, 0.00000,  0.0)
expected_transition_SIS[2,] <- c(0,  0.0, 0.67778, 0.12222,  0.0)
expected_transition_SIS[3,] <- c(0,  0.0, 0.00000, 0.00000,  0.8)
expected_transition_SIS[4,] <- c(0,  0.0, 0.00000, 0.00000,  0.9)
expected_transition_SIS[5,] <- c(0,  0.0, 0.00000, 0.00000,  0.0)
expected_sojourn_SIS <- c(1.82000, 2.26090, 0.29508, 5.90910, 5.0000)
expected_transition_MH <- matrix(0, 5, 5)
expected_transition_MH[1,] <- c(0,  0.9, 0.00000, 0.00000,  0.0)
expected_transition_MH[2,] <- c(0,  0.0, 0.43164, 0.36836,  0.0)
expected_transition_MH[3,] <- c(0,  0.0, 0.00000, 0.00000,  0.8)
expected_transition_MH[4,] <- c(0,  0.0, 0.00000, 0.00000,  0.9)
expected_transition_MH[5,] <- c(0,  0.0, 0.00000, 0.00000,  0.0)
expected_sojourn_MH <- c(2.1400, 3.3070, 1.7094, 3.2362, 5.0000)

test_that("Estimated SIS transitions parameters valid", {
  expect_equal(identical(signif(estim_SIS$estimation_method$output$transition_law_param, 5), expected_transition_SIS), TRUE)
})
test_that("Estimated SIS sojourn durations parameters valid", {
  expect_equal(identical(signif(estim_SIS$estimation_method$output$sojourn_law_param, 5), expected_sojourn_SIS), TRUE)
})

test_that("Estimated MH transitions parameters valid", {
  expect_equal(identical(signif(estim_MH$estimation_method$output$transition_law_param, 5), expected_transition_MH), TRUE)
})
test_that("Estimated MH sojourn durations parameters valid", {
  expect_equal(identical(signif(estim_MH$estimation_method$output$sojourn_law_param, 5), expected_sojourn_MH), TRUE)
})
