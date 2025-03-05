test_that("both dimensions must be positive", {
  expect_error(
    calc_forage_production(
      dim_x = -30,
      dim_y = 30,
      plot_units = "cm",
      output_units = "kg"
      ),
    "dim_x and dim_y must be positive."
    )
})

test_that("plot_size units must be valid", {
  expect_error(
    calc_forage_production(
      dim_x = 30,
      dim_y = 30,
      plot_units = "unknown unit",
      output_units = "kg"
      ),
    "plot_units must be either 'cm', 'm', 'in', or 'ft'."
    )
})
