context("Check generate trajectories R")

set.seed(123)
migr <- generate_random_migration( nb_sites = 3, flight_duration_domain = c(1, 1), nb_birds = 5 )
migr$horizon <- 20
set.seed(123)
traj <- generate_trajectories( migr )
expected_traj <- matrix(0, nrow = 5, ncol = 21)
expected_traj[1, ] <-   c(1,    1,    1,    1,    1,    0,    3,    3,    3,     3,     3,     3,     3,     3,     3,     3,     3,    -1,    -1,    -1,    -1)
expected_traj[2, ] <-   c(1,    1,    1,    0,    3,    3,    3,    3,    3,     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,    -1)
expected_traj[3, ] <-   c(1,    1,    1,    1,    1,    1,    1,    1,    1,     1,     1,     0,     2,     2,     2,     2,     2,     2,     2,     0,     3)
expected_traj[4, ] <-   c(1,    1,    1,    1,    0,    3,    3,    3,    3,     3,     3,     3,     3,     3,    -1,    -1,    -1,    -1,    -1,    -1,    -1)
expected_traj[5, ] <-   c(1,    1,    1,    1,    1,    1,    1,    1,    1,     0,     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,     3)

test_that("generated trajectories valid", {
  expect_equal(identical(traj, expected_traj), TRUE)
})

