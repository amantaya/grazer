test_that("no negative gas values", {
  data <- generate_greenfeed_data(10000, type = "preliminary")
  expect_true(all(data$CH4GramsPerDay >= 0))
  expect_true(all(data$CO2GramsPerDay >= 0))
  expect_true(all(data$O2GramsPerDay >= 0))
  expect_true(all(data$H2GramsPerDay >= 0))
  expect_true(all(data$H2SGramsPerDay >= 0))
})
