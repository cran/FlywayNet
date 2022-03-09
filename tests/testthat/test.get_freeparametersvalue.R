context("Check extract parameters")

migr <- generate_toy_migration()
para <- get_freeparametersvalue(migr)

expected_para <- c(0.55, 3.00, 4.00, 2.00, 4.00)

test_that("extract_parameters valid", {
  expect_equal(all(para == expected_para), TRUE)
})

