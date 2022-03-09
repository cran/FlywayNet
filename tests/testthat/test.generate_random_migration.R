context("Check generate random migration")

set.seed(123)
migr <- generate_random_migration()

expected_site_name <- c("site1", "site2", "site3", "site4")  

expected_link_knowledge <- matrix(FALSE, 4, 4)
expected_link_knowledge[1,] <- c(FALSE,  TRUE, TRUE, FALSE)
expected_link_knowledge[2,] <- c(FALSE, FALSE,  TRUE,  TRUE)
expected_link_knowledge[3,] <- c(FALSE, FALSE, FALSE,  TRUE)
expected_link_knowledge[4,] <- c(FALSE, FALSE, FALSE, FALSE)

expected_flight_duration <- matrix(0, 4, 4)
expected_flight_duration[1,]    <- c(-1,     3,     3,    -1)
expected_flight_duration[2,]    <- c(-1,    -1,     3,     3)
expected_flight_duration[3,]    <- c(-1,    -1,    -1,     3)
expected_flight_duration[4,]    <- c(-1,    -1,    -1,    -1)
 
expected_initial_state <- c( 100,   0,   0,   0)

expected_horizon <- 36

expected_death_probability <- c( 0.03, 0.03, 0.03, 1.00)

expected_transition_law_type <- "multinomial"

expected_transition_law_param <- matrix(0, 4, 4)
expected_transition_law_param[1,]    <- c(0,  0.485, 0.485, 0.00)
expected_transition_law_param[2,]    <- c(0,  0.0,   0.485, 0.485)
expected_transition_law_param[3,]    <- c(0,  0.0,   0.00,  0.97)
expected_transition_law_param[4,]    <- c(0,  0.0,   0.00,  0.00)

expected_sojourn_law_type <- "Poisson"

expected_sojourn_law_param <- c( 5.5,  5.5,  5.5, 16.5 )

expected_observation_law_type <- "Poisson"

expected_observation_law_param <- NULL

expected_observation <- matrix(NA, 6, 37)
expected_observation[1,] <-  c(NA,  123,   80,   92,   69,   57,   49,   21,   22,    19,    8,     5,     5,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0)
expected_observation[2,] <-  c(NA,    0,    0,    0,    0,    0,    3,    6,   14,    28,    30,    40,    48,    34,    32,    28,    24,    16,    15,    12,    5,     1,     2,     2,    1,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0)
expected_observation[3,] <-  c(NA,    0,    0,    0,    0,    0,    5,    7,   17,    17,    32,    30,    29,    25,    18,    25,    24,    23,    18,    18,    11,    14,    8,    13,     6,     6,    10,    12,    13,    11,     2,     2,     0,     0,     0,     0,     0)
expected_observation[4,] <-  c(NA,    0,    0,    0,    0,    0,    0,    0,    0,     0,     0,     0,     0,     0,     8,     4,    15,    29,    31,    45,    54,    63,   78,    57,    71,    62,    77,    78,    77,    86,    64,    68,    68,    80,    61,    56,    62)

expected_class <- "migration"


test_that("generated random migration valid", {
  expect_equal(all(migr$site_name == expected_site_name), TRUE)
  expect_equal(all(migr$link_knowledge == expected_link_knowledge), TRUE)
  expect_equal(all(migr$flight_duration == expected_flight_duration), TRUE)
  expect_equal(all(migr$initial_state == expected_initial_state), TRUE)
  expect_equal(all(migr$horizon == expected_horizon), TRUE)
  expect_equal(all(signif(migr$death_probability,3) == expected_death_probability), TRUE)
  expect_equal(all(migr$transition_law_type == expected_transition_law_type), TRUE)
  expect_equal(all(migr$transition_law_param == expected_transition_law_param), TRUE)
  expect_equal(all(migr$sojourn_law_type == expected_sojourn_law_type), TRUE)
  expect_equal(all(migr$sojourn_law_param == expected_sojourn_law_param), TRUE)
  expect_equal(all(migr$observation_law_type == expected_observation_law_type), TRUE)
  expect_equal(all(migr$observation_law_param == expected_observation_law_param), TRUE)
  mask <- !is.na(expected_observation)
  expect_equal(all(migr$observation[mask] == expected_observation[mask]), TRUE)
})

