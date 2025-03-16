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
