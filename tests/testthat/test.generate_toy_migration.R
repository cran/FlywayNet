context("Check generate toy migration")
# This test is important because the function is used in other tests

migr <- generate_toy_migration()

expected_site_name <- c("departure", "site1", "site2", "site3", "arrival")  

expected_link_knowledge <- matrix(FALSE, 5, 5)
expected_link_knowledge[1,] <- c(FALSE,  TRUE, FALSE, FALSE, FALSE)
expected_link_knowledge[2,] <- c(FALSE, FALSE,  TRUE,  TRUE, FALSE)
expected_link_knowledge[3,] <- c(FALSE, FALSE, FALSE, FALSE,  TRUE)
expected_link_knowledge[4,] <- c(FALSE, FALSE, FALSE, FALSE,  TRUE)
expected_link_knowledge[5,] <- c(FALSE, FALSE, FALSE, FALSE, FALSE)

expected_flight_duration <- matrix(0, 5, 5)
expected_flight_duration[1,]    <- c(0,    1,    0,    0,    0)
expected_flight_duration[2,]    <- c(0,    0,    1,    2,    0)
expected_flight_duration[3,]    <- c(0,    0,    0,    0,    2)
expected_flight_duration[4,]    <- c(0,    0,    0,    0,    1)
expected_flight_duration[5,]    <- c(0,    0,    0,    0,    0)

expected_initial_state <- c( 100,   0,   0,   0,   0)

expected_horizon <- 20

expected_death_probability <- c( 0.1, 0.2, 0.2, 0.1, 1.0)

expected_transition_law_type <- "multinomial"

expected_transition_law_param <- matrix(0, 5, 5)
expected_transition_law_param[1,]    <- c(0,  0.9, 0.00, 0.00,  0.0)
expected_transition_law_param[2,]    <- c(0,  0.0, 0.55, 0.25,  0.0)
expected_transition_law_param[3,]    <- c(0,  0.0, 0.00, 0.00,  0.8)
expected_transition_law_param[4,]    <- c(0,  0.0, 0.00, 0.00,  0.9)
expected_transition_law_param[5,]    <- c(0,  0.0, 0.00, 0.00,  0.0)

expected_sojourn_law_type <- "Poisson"

expected_sojourn_law_param <- c( 3, 4, 2, 4, 5 )

expected_observation_law_type <- "Poisson"

expected_observation_law_param <- NULL

expected_observation <- matrix(NA, 7, 21)
expected_observation[1,] <-  c(NA,   91,   58,   30,   11,    6,    2,    2,    0,     0,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,   NA)
expected_observation[2,] <-  c(NA,    3,   18,   49,   51,   45,   46,   33,   20,    12,    11,     5,     7,     6,     3,     0,     0,    NA,    NA,    NA,   NA)
expected_observation[3,] <-  c(NA,    0,    0,    0,    3,   12,   20,    5,    8,    22,     7,     4,     2,     5,     2,     0,     0,     1,     2,     1,   0)
expected_observation[4,] <-  c(NA,    0,    0,    0,    0,    1,    1,    0,   11,     9,     4,     5,     7,     7,    10,     8,     4,     1,     2,     0,   0)
expected_observation[5,] <-  c(NA,    0,    0,    0,    0,    0,    0,    0,    0,    16,    25,    15,    37,    39,    44,    49,    41,    49,    47,    58,   52)

expected_class <- "migration"


test_that("generated toy migration valid", {
  expect_equal(all(migr$site_name == expected_site_name), TRUE)
  expect_equal(all(migr$link_knowledge == expected_link_knowledge), TRUE)
  expect_equal(all(migr$flight_duration == expected_flight_duration), TRUE)
  expect_equal(all(migr$initial_state == expected_initial_state), TRUE)
  expect_equal(all(migr$horizon == expected_horizon), TRUE)
  expect_equal(all(migr$death_probability == expected_death_probability), TRUE)
  expect_equal(all(migr$transition_law_type == expected_transition_law_type), TRUE)
  expect_equal(all(migr$transition_law_param == expected_transition_law_param), TRUE)
  expect_equal(all(migr$sojourn_law_type == expected_sojourn_law_type), TRUE)
  expect_equal(all(migr$sojourn_law_param == expected_sojourn_law_param), TRUE)
  expect_equal(all(migr$observation_law_type == expected_observation_law_type), TRUE)
  expect_equal(all(migr$observation_law_param == expected_observation_law_param), TRUE)
  mask <- !is.na(expected_observation)
  expect_equal(all(migr$observation[mask] == expected_observation[mask]), TRUE)
})

