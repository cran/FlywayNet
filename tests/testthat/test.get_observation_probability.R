context("Check get observation probability")

migr <- generate_toy_migration()
set.seed(123)
traj <- generate_trajectories( migr )
p <- get_observation_probability( migr$observation, get_counts( migr, traj ) )
expected_p <- -742.8667 
test_that("generated observation probability valid", {
  expect_equal(signif(p,7), expected_p)
})


set.seed(789)
traj <- generate_trajectories( migr )
p <- get_observation_probability( migr$observation, get_counts( migr, traj ) )
expected_p <- -660.6548

test_that("generated observation probability  valid", {
  expect_equal(signif(p,7), expected_p)
})

