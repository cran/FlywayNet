context("Check get parameters likelihood")

migr <- generate_toy_migration()

set.seed(123)
L <- get_paramlikelihood( migr, migr$observation)
expected_L <- -621.9155
test_that("get parameters likelihood valid", {
  expect_equal(signif(L,7), expected_L)
})


migr_obs <- migr
set.seed(123)
L <- get_paramlikelihood( migr, generate_observedcounts(migr, generate_trajectories(migr)))
expected_L <- -193.4887

test_that("get parameters likelihood valid", {
  expect_equal(signif(L,7), expected_L)
})

