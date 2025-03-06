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

test_that("plot_area is calculated correctly for rectangular plots", {
  result <- calc_plot_area(
    dim_x = 30,
    dim_y = 50,
    units = "cm"
  )
  expected <- units::set_units(1500, "cm^2")
  expect_equal(result, expected)
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

test_that("forage production is calculated correctly for grams", {
  sample_area <- calc_plot_area(
    dim_x = 50,
    dim_y = 50,
    units = "cm"
  )
  result <- calc_forage_prod(
    sample_wt = 100,
    sample_units = "g",
    sample_area = sample_area,
    output_units = "kg/ha"
  )
  expected <- units::set_units(4000, "kg/hectare")
  expect_equal(result, expected)
})

test_that("forage production is calculated correctly for lbs", {
  sample_area <- calc_plot_area(
    dim_x = 50,
    dim_y = 50,
    units = "cm"
  )
  result <- calc_forage_prod(
    sample_wt = 100,
    sample_units = "g",
    sample_area = sample_area,
    output_units = "lb/acre"
  )
  expected <- units::set_units(3568.73076, "lb/acre")
  expect_equal(result, expected)
})

test_that("output_units must be valid", {
  sample_area <- calc_plot_area(
    dim_x = 50,
    dim_y = 50,
    units = "cm"
  )
  expect_error(
    calc_forage_prod(
      sample_wt = 100,
      sample_units = "g",
      sample_area = sample_area,
      output_units = "unknown unit"
    ),
    "output_units must be either 'kg/ha' or 'lb/acre'."
  )
})

test_that("forage production is calculated correctly when sample_units are ounces", {
  sample_area <- calc_plot_area(
    dim_x = 50,
    dim_y = 50,
    units = "cm"
  )
  result <- calc_forage_prod(
    sample_wt = 3.5274,
    sample_units = "avoirdupois_ounce",
    sample_area = sample_area,
    output_units = "kg/ha"
  )
  expected <- units::set_units(4000, "kg/hectare")
  expect_equal(result, expected, tolerance = 0.01)
})

test_that("forage production is calculated correctly when sample_units are pounds", {
  sample_area <- calc_plot_area(
    dim_x = 50,
    dim_y = 50,
    units = "cm"
  )
  result <- calc_forage_prod(
    sample_wt = 0.220462,
    sample_units = "avoirdupois_pound",
    sample_area = sample_area,
    output_units = "kg/ha"
  )
  expected <- units::set_units(4000, "kg/hectare")
  expect_equal(result, expected, tolerance = 0.01)
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

test_that("generate_forage_data generates different values on subsequent calls with different seeds", {
  set.seed(123) # Set seed for reproducibility

  data1 <- generate_forage_data(n_rows = 10)

  set.seed(456) # Set a different seed for the second call
  data2 <- generate_forage_data(n_rows = 10)

  # Check that the generated data frames are not identical
  expect_false(identical(data1, data2))
})

test_that("generate_forage_data generates the same values on subsequent calls with the same seed", {
  set.seed(123) # Set seed for reproducibility

  data1 <- generate_forage_data(n_rows = 10)

  set.seed(123) # Set a different seed for the second call
  data2 <- generate_forage_data(n_rows = 10)

  # Check that the generated data frames are not identical
  expect_true(identical(data1, data2))
})
