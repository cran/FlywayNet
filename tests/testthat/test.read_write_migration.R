context("Check migration structure")

set.seed(123)
migr1 <- generate_random_migration( nb_sites = 5)
set.seed(123)
file_name <- "test_read_write_migration.txt"
write_migration(migr1, file_name = file_name)
migr2  <- read_migration("test_read_write_migration.txt")

test_that("generated migrations valid", {
  expect_equal(all(migr1$site_name == migr2$site_name), TRUE)
  expect_equal(all(migr1$site_link == migr2$site_link), TRUE)
  expect_equal(all(migr1$flight_duration == migr2$flight_duration), TRUE)
  expect_equal(all(migr1$initial_state == migr2$initial_state), TRUE)
  expect_equal(all(signif(migr1$death_probability,3) == migr2$death_probability), TRUE)
  expect_equal(migr1$transition_law_type == migr2$transition_law_type, TRUE)
  expect_equal(all(migr1$transition_law_param == migr2$transition_law_param), TRUE)
  expect_equal(migr1$sojourn_law_type == migr2$sojourn_law_type, TRUE)
  expect_equal(all(migr1$sojourn_law_param == migr2$sojourn_law_param), TRUE)
  expect_equal(migr1$observation_law_type == migr2$observation_law_type,TRUE)
  expect_equal(all(migr1$observation_law_param == migr2$observation_law_param), TRUE)
  expect_equal(all(migr1$observation == migr2$observation, na.rm = TRUE), TRUE)
})

file.remove(file_name)
