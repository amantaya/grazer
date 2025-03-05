test_that("both dimensions must be positive", {
  expect_error(
    get_plot_area(
      dim_x = -30,
      dim_y = 30,
      units = "cm"
      ),
    "dim_x and dim_y must be positive."
    )
})

test_that("plot_size units must be valid", {
  expect_error(
    get_plot_area(
      dim_x = 30,
      dim_y = 30,
      units = "unknown unit"
      ),
    "units must be either 'cm', 'm', 'in', or 'ft'."
    )
})

test_that("both dimensions must be provided", {
  expect_error(
    get_plot_area(
      dim_x = 30,
      units = "cm"
      ),
    "You must provide both dim_x and dim_y."
    )
})

test_that("plot_area is calculated correctly", {
  expect_equal(
    get_plot_area(
      dim_x = 30,
      dim_y = 30,
      units = "m"
      ),
    900
    )
})
