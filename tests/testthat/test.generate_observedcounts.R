context("Check generate observed counts")

set.seed(123)
migr <- generate_random_migration( nb_sites = 3, flight_duration_domain = c(1, 1) )
migr$horizon <- 5

set.seed(456)
traj <- generate_trajectories( migr )

set.seed(789)
obs <- generate_observedcounts( migr, traj )
expected_obs <- matrix(NA, nrow = 5, ncol = 6)
expected_obs[1, ] <-   c(NA,    105,    75,    69,    79,    68)
expected_obs[2, ] <-   c(NA,    0,    0,    1,    6,    8)
expected_obs[3, ] <-   c(NA,    0,    0,    0,    3,    2)

maskNA <- !is.na(expected_obs)
test_that("generated observed counts valid", {
  expect_equal(all(obs[maskNA] == expected_obs[maskNA]), TRUE)
})


mask <- matrix(TRUE, nrow = 3, ncol = 5)
mask[1, 5] <- FALSE
mask[3, 1:3] <- FALSE
set.seed(456)
obs_mask <- generate_observedcounts( migr, traj, mask )
expected_obs_mask <- matrix(NA, nrow = 5, ncol = 6)
expected_obs_mask[1, ] <-   c(NA,    86,   108,    84,    80,   NA)
expected_obs_mask[2, ] <-   c(NA,    0,    0,    1,    10,    15)
expected_obs_mask[3, ] <-   c(NA,   NA,   NA,   NA,    5,    6)

maskNA <- !is.na(expected_obs)
test_that("generated observed counts valid", {
  expect_equal(all(obs[maskNA] == expected_obs[maskNA]), TRUE)
})

