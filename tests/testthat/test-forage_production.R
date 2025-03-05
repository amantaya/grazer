test_that("both dimensions must be positive", {
  expect_error(
    calc_plot_area(
      dim_x = -30,
      dim_y = 30,
      units = "cm"
      ),
    "dim_x and dim_y must be positive."
    )
})

test_that("plot_size units must be valid", {
  expect_error(
    calc_plot_area(
      dim_x = 30,
      dim_y = 30,
      units = "unknown unit"
      ),
    "units must be either 'cm', 'm', 'in', or 'ft'."
    )
})

test_that("both dimensions must be provided", {
  expect_error(
    calc_plot_area(
      dim_x = 30,
      units = "cm"
      ),
    "You must provide both dim_x and dim_y."
    )
})

test_that("plot_area is calculated correctly for centimeters", {
  result <- calc_plot_area(
    dim_x = 30,
    dim_y = 30,
    units = "cm"
  )
  expected <- units::set_units(900, "cm^2")
  expect_equal(result, expected)
})

test_that("plot_area is calculated correctly for meters", {
  result <- calc_plot_area(
    dim_x = 0.5,
    dim_y = 0.5,
    units = "m"
  )
  expected <- units::set_units(0.25, "m^2")
  expect_equal(result, expected)
})

test_that("plot_area is calculated correctly for inches", {
  result <- calc_plot_area(
    dim_x = 12,
    dim_y = 12,
    units = "in"
  )
  expected <- units::set_units(144, "in^2")
  expect_equal(result, expected)
})

test_that("plot_area is calculated correctly for feet", {
  result <- calc_plot_area(
    dim_x = 1,
    dim_y = 1,
    units = "ft"
  )
  expected <- units::set_units(1, "ft^2")
  expect_equal(result, expected)
})
