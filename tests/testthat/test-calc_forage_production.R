test_that("both dimensions must be positive", {
  expect_error(
    get_plot_area(
      dim_x = -30,
      dim_y = 30,
      plot_units = "cm"
      ),
    "dim_x and dim_y must be positive."
    )
})

test_that("plot_size units must be valid", {
  expect_error(
    get_plot_area(
      dim_x = 30,
      dim_y = 30,
      plot_units = "unknown unit"
      ),
    "plot_units must be either 'cm', 'm', 'in', or 'ft'."
    )
})
