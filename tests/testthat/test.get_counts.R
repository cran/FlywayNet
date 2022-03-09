context("Check generate counts")

set.seed(123)
migr <- generate_random_migration( nb_sites = 3, flight_duration_domain = c(1, 1), nb_birds = 10 )
migr$horizon <- 15
set.seed(123)
traj <- generate_trajectories( migr )
count <- get_counts( migr, traj )

expected_count <- matrix(0, nrow = 5, ncol = 16)
expected_count[1, ] <-   c(10,   10,   10,    9,    8,    7,    5,    5,    4,     2,     2,     0,     0,     0,     0,     0)
expected_count[2, ] <-   c( 0,    0,    0,    0,    0,    0,    0,    1,    1,     1,     2,     2,     3,     2,     2,     2)
expected_count[3, ] <-   c( 0,    0,    0,    0,    1,    2,    3,    4,    4,     5,     6,     6,     7,     7,     7,     7)
expected_count[4, ] <-   c( 0,    0,    0,    0,    0,    0,    0,    0,    0,     0,     0,     0,     0,     0,     1,     1)
expected_count[5, ] <-   c( 0,    0,    0,    1,    1,    1,    2,    0,    1,     2,     0,     2,     0,     1,     0,     0)

test_that("generated count valid", {
  expect_equal(all(count == expected_count), TRUE)
})

