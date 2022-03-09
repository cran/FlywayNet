context("Check estimate_migration_ABC")

set.seed(123)
migr <- generate_random_migration( nb_sites = 3 )
set.seed(123)
estim <- estimate_migration_ABC( migr,
                                 estimate_transitions = TRUE,
                                 estimate_sojourns = TRUE,
                                 sojourn_domain = c(1, migr$horizon),
                                 nb_simul = 40,
                                 p_acc_min = 0.05,
                                 choice_method = "mean",
                                 n_cluster = 1,
                                 verbose = FALSE)
expected_transition <- matrix(0, 3, 3)
expected_transition[1,] <- c(0, 0.51634, 0.45366)
expected_transition[2,] <- c(0, 0.00000, 0.97000)
expected_transition[3,] <- c(0, 0.00000, 0.00000)

expected_sojourn <- c(5.5588, 6.633,   11)

test_that("Estimated transitions parameters valid", {
  expect_equal(identical(signif(estim$estimation_method$output$transition_law_param, 5), expected_transition), TRUE)
})

# For sojourn here identical does not work !?
test_that("Estimated sojourn durations parameters valid", {
  expect_equal(all(signif(estim$estimation_method$output$sojourn_law_param, 5) == expected_sojourn), TRUE)
})


migr <- generate_toy_migration()
set.seed(123)
estim <- estimate_migration_ABC( migr,
                                 estimate_transitions = TRUE,
                                 estimate_sojourns = TRUE,
                                 sojourn_domain = c(1, migr$horizon),
                                 nb_simul = 40,
                                 p_acc_min = 0.05,
                                 choice_method = "mean",
                                 n_cluster = 1,
                                 verbose = FALSE)
expected_transition <- matrix(0, 5, 5)
expected_transition[1,] <- c(0,  0.9, 0.00000, 0.00000,  0.0)
expected_transition[2,] <- c(0,  0.0, 0.55805, 0.24195,  0.0)
expected_transition[3,] <- c(0,  0.0, 0.00000, 0.00000,  0.8)
expected_transition[4,] <- c(0,  0.0, 0.00000, 0.00000,  0.9)
expected_transition[5,] <- c(0,  0.0, 0.00000, 0.00000,  0.0)

expected_sojourn <- c(1.9232, 1.5149, 2.3284, 3.2682, 5.0000)

test_that("Estimated transitions parameters valid", {
  expect_equal(identical(signif(estim$estimation_method$output$transition_law_param, 5), expected_transition), TRUE)
})

test_that("Estimated sojourn durations parameters valid", {
  expect_equal(identical(signif(estim$estimation_method$output$sojourn_law_param, 5), expected_sojourn), TRUE)
})

set.seed(123)
estim <- estimate_migration_ABC( migr,
                                 estimate_transitions = TRUE,
                                 estimate_sojourns = TRUE,
                                 sojourn_domain = list(c(2,3), c(2,3), c(2,3), c(2,3), c(2,3)),
                                 nb_simul = 40,
                                 p_acc_min = 0.05,
                                 choice_method = "mean",
                                 n_cluster = 1,
                                 verbose = FALSE)

expected_sojourn <- c(2.3246, 2.3987, 2.3605, 2.4569, 5.0000)

test_that("Estimated sojourn durations parameters valid", {
  expect_equal(identical(signif(estim$estimation_method$output$sojourn_law_param, 5), expected_sojourn), TRUE)
})


migr <- generate_toy_migration()
set.seed(123)
estim <- estimate_migration_ABC( migr,
                                 estimate_transitions = TRUE,
                                 estimate_sojourns = TRUE,
                                 sojourn_domain = c(1, migr$horizon),
                                 nb_simul = 40,
                                 p_acc_min = 0.05,
                                 choice_method = "median",
                                 n_cluster = 1,
                                 verbose = FALSE)
expected_transition <- matrix(0, 5, 5)
expected_transition[1,] <- c(0,  0.9, 0.00000, 0.00000,  0.0)
expected_transition[2,] <- c(0,  0.0, 0.53645, 0.26355,  0.0)
expected_transition[3,] <- c(0,  0.0, 0.00000, 0.00000,  0.8)
expected_transition[4,] <- c(0,  0.0, 0.00000, 0.00000,  0.9)
expected_transition[5,] <- c(0,  0.0, 0.00000, 0.00000,  0.0)

expected_sojourn <- c(1.9778, 1.5477, 2.3648, 3.1804, 5.0000)

test_that("Estimated transitions parameters valid", {
  expect_equal(identical(signif(estim$estimation_method$output$transition_law_param, 5), expected_transition), TRUE)
})

test_that("Estimated sojourn durations parameters valid", {
  expect_equal(identical(signif(estim$estimation_method$output$sojourn_law_param, 5), expected_sojourn), TRUE)
})


migr <- generate_toy_migration()
set.seed(123)
estim <- estimate_migration_ABC( migr,
                                 estimate_transitions = TRUE,
                                 estimate_sojourns = TRUE,
                                 sojourn_domain = c(1, migr$horizon),
                                 nb_simul = 40,
                                 p_acc_min = 0.05,
                                 choice_method = "density",
                                 n_cluster = 1,
                                 verbose = FALSE)
expected_transition <- matrix(0, 5, 5)
expected_transition[1,] <- c(0,  0.9, 0.00000, 0.00000,  0.0)
expected_transition[2,] <- c(0,  0.0, 0.50707, 0.29293,  0.0)
expected_transition[3,] <- c(0,  0.0, 0.00000, 0.00000,  0.8)
expected_transition[4,] <- c(0,  0.0, 0.00000, 0.00000,  0.9)
expected_transition[5,] <- c(0,  0.0, 0.00000, 0.00000,  0.0)

expected_sojourn <- c(1.9791, 1.6775, 2.4095, 2.4292, 5.0000)

test_that("Estimated transitions parameters valid", {
  expect_equal(identical(signif(estim$estimation_method$output$transition_law_param, 5), expected_transition), TRUE)
})

test_that("Estimated sojourn durations parameters valid", {
  expect_equal(identical(signif(estim$estimation_method$output$sojourn_law_param, 5), expected_sojourn), TRUE)
})


migr <- generate_toy_migration()
set.seed(123)
estim <- estimate_migration_ABC( migr,
                                 estimate_transitions = TRUE,
                                 estimate_sojourns = TRUE,
                                 sojourn_domain = c(1, migr$horizon),
                                 nb_simul = 40,
                                 p_acc_min = 0.05,
                                 choice_method = "mode",
                                 n_cluster = 1,
                                 verbose = FALSE)
expected_transition <- matrix(0, 5, 5)
expected_transition[1,] <- c(0,  0.9, 0.00000, 0.00000,  0.0)
expected_transition[2,] <- c(0,  0.0, 0.61172, 0.18828,  0.0)
expected_transition[3,] <- c(0,  0.0, 0.00000, 0.00000,  0.8)
expected_transition[4,] <- c(0,  0.0, 0.00000, 0.00000,  0.9)
expected_transition[5,] <- c(0,  0.0, 0.00000, 0.00000,  0.0)

expected_sojourn <- c(2.0538, 1.6801, 2.3605, 2.3081, 5.0000)

test_that("Estimated transitions parameters valid", {
  expect_equal(identical(signif(estim$estimation_method$output$transition_law_param, 5), expected_transition), TRUE)
})

test_that("Estimated sojourn durations parameters valid", {
  expect_equal(identical(signif(estim$estimation_method$output$sojourn_law_param, 5), expected_sojourn), TRUE)
})


