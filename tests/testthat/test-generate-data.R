test_that("no negative gas values", {
  data <- generate_greenfeed_data(n_rows = 100, type = "preliminary")
  expect_true(all(data$CH4GramsPerDay >= 0))
  expect_true(all(data$CO2GramsPerDay >= 0))
  expect_true(all(data$O2GramsPerDay >= 0))
  expect_true(all(data$H2GramsPerDay >= 0))
  expect_true(all(data$H2SGramsPerDay >= 0))
  expect_true(all(data$AirflowLitersPerSec >= 0))
  expect_true(all(data$AirflowCf >= 0))
  expect_true(all(data$WindSpeedMetersPerSec >= 0))
})

test_that("WindDirDeg values between 0 and 360", {
  data <- generate_greenfeed_data(n_rows = 100, type = "preliminary")
  expect_true(all(data$WindDirDeg >= 0))
  expect_true(all(data$WindDirDeg <= 360))
})

test_that("eid length is exactly 15", {
  eid_numbers <- generate_eid(100)
  eid_length <- eid_length(eid_numbers)
  expect_true(all(eid_length == 15))
})

test_that("all eid numbers are unique within a set", {
  eid_numbers <- generate_eid(100)
  expect_true(length(unique(eid_numbers)) == length(eid_numbers))
})

test_that("generate_forage_data generates correct number of rows and columns", {
  set.seed(123) # Set seed for reproducibility

  data <- generate_forage_data(n_rows = 10)

  # Check that the data frame has the correct number of rows
  expect_equal(nrow(data), 10)

  # Check that the data frame has 5 columns
  expect_equal(ncol(data), 5)
})

test_that("generate_forage_data generates non-negative values", {
  set.seed(123) # Set seed for reproducibility

  data <- generate_forage_data(n_rows = 10)

  # Check that all values are non-negative
  expect_true(all(data >= 0))
})

test_that("generate_forage_data generates different values with different seeds", { # nolint: line_length_linter
  set.seed(123) # Set seed for reproducibility

  data1 <- generate_forage_data(n_rows = 10)

  set.seed(456) # Set a different seed for the second call
  data2 <- generate_forage_data(n_rows = 10)

  # Check that the generated data frames are not identical
  expect_false(identical(data1, data2))
})

test_that("generate_forage_data generates the same values with the same seed", {
  set.seed(123) # Set seed for reproducibility

  data1 <- generate_forage_data(n_rows = 10)

  set.seed(123) # Set a different seed for the second call
  data2 <- generate_forage_data(n_rows = 10)

  # Check that the generated data frames are not identical
  expect_true(identical(data1, data2))
})