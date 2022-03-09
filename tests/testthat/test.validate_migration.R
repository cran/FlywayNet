context("Validate migration structure")

test_that("generated migrations valid", {
  expect_equal(validate_migration(generate_toy_migration()), list())
  expect_equal(
    validate_migration(new_migration(
      site_name = c("s1", "s2", "s3"),
      flight_duration = matrix(c(0, 1, 2, 0, 0, 1, 0, 0, 0), ncol = 3, byrow = TRUE),
      initial_state = c(10, 0, 0),
      horizon = 4,
      death_probability = c(0.05, 0.1, 1),
      observation = matrix(c(NA, 9, 0, 0, 0,
                             NA, 8, 0, 0, 2,
                             NA, 1, 0, 0, 7,
                             NA, NA, NA, NA, NA,
                             NA, NA, NA, NA, NA), ncol = 5, byrow = TRUE)
    )),
    list())
})

test_that("Detect invalid migrations", {
  expect_equal(
    validate_migration(new_migration(
      site_name = c("s1", "s2", "s3"),
      link_knowledge = matrix(as.logical(c(0, 1, 1, 0, 0, 1, 1, 0, 0)), nrow = 3, ncol = 3, byrow = TRUE),
      flight_duration = matrix(c(0, 1, 2, 0, 0, 1, 0, 0, 0), ncol = 3, byrow = TRUE),
      initial_state = c(10, 0, 0),
      horizon = 4,
      death_probability = c(0.05, 0.1, 1),
      observation = matrix(c(NA, 9, 0, 0, 0,
                             NA, 8, 0, 0, 2,
                             NA, 1, 0, 0, 7,
                             NA, NA, NA, NA, NA,
                             NA, NA, NA, NA, NA), ncol = 5, byrow = TRUE)
    ))[[1]],
    "link_knowledge attribute not consistent with site_name order declaration.")
  expect_equal(
    validate_migration(new_migration(
      site_name = c("s1", "s2", "s3"),
      flight_duration = matrix(c(0, 1, 2, 0, 0, 1, 0, 0, 0), ncol = 3, byrow = TRUE),
      initial_state = c(10, 0, 0),
      horizon = 4,
      death_probability = c(0.05, 0.1, 1),
      observation = matrix(c(NA, 9, 0, 0, 0,
                             NA, 8, 0, 0, 2,
                             NA, 1, 0, 0, 7,
                             NA, NA, NA, NA, NA,
                             NA, NA, NA, NA, NA), ncol = 5, byrow = TRUE),
      transition_law_type = "multinomial",
      transition_law_param = 2
    ))[[1]],
    "transition_law_param attribute contains at least a value not in [0 1].")
  expect_equal(
    validate_migration(new_migration(
      site_name = c("s1", "s2", "s3"),
      flight_duration = matrix(c(0, 1, 2, 0, 0, 1, 0, 0, 0), ncol = 3, byrow = TRUE),
      initial_state = c(10, 0, 0),
      horizon = 4,
      death_probability = c(0.05, 0.1, 1),
      observation = matrix(c(NA, 9, 0, 0, 0,
                             NA, 8, 0, 0, 2,
                             NA, 1, 0, 0, 7,
                             NA, NA, NA, NA, NA,
                             NA, NA, NA, NA, NA), ncol = 5, byrow = TRUE),
      observation_law_type = "Poisson",
      observation_law_param = c(0.1, 0.3)
    ))[[1]],
    "observation_law_param attribute not consistent with a Poisson law.")
  expect_equal(
    validate_migration(new_migration(
      site_name = c("s1", "s2", "s3"),
      flight_duration = matrix(c(0, 1, 2, 0, 0, 1, 0, 0, 0), ncol = 3, byrow = TRUE),
      initial_state = c(10, 0, 0),
      horizon = 4,
      death_probability = c(0.05, 0.1, 1),
      observation = matrix(c(NA, 9, 0, 0, 0,
                             NA, 8, 0, 0, 2,
                             NA, 1, 0, 0, 7,
                             NA, NA, NA, NA, NA,
                             NA, NA, NA, NA, NA), ncol = 5, byrow = TRUE),
      sojourn_law_param = c(0.1, -1)
    ))[[1]],
    "sojourn_law_param attribute length problem.")
})
