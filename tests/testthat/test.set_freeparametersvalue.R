context("Check assign parameters")

migr <- generate_toy_migration()
para <- c(0.1, 10, 11, 12, 13)
migr <- set_freeparametersvalue(migr, para)

expected_transition_law_param <- matrix(0, 5, 5)
expected_transition_law_param[1,] <- c(0,  0.9,  0.0,  0.0,  0.0)
expected_transition_law_param[2,] <- c(0,  0.0,  0.1,  0.7,  0.0)
expected_transition_law_param[3,] <- c(0,  0.0,  0.0,  0.0,  0.8)
expected_transition_law_param[4,] <- c(0,  0.0,  0.0,  0.0,  0.9)
expected_transition_law_param[5,] <- c(0,  0.0,  0.0,  0.0,  0.0)
expected_sojourn_law_param <- c(10, 11, 12, 13, 5)


test_that("extract_parameters transitions valid", {
  expect_equal(all(abs(migr$transition_law_param - expected_transition_law_param) < 0.00001), TRUE)
})

test_that("extract_parameters sojourn valid", {
  expect_equal(all(migr$sojourn_law_param == expected_sojourn_law_param), TRUE)
})

