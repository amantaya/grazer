test_that("create_greenfeed_col_spec returns correct column specifications", {
  expected_col_spec <- readr::cols(
    FeederID = readr::col_double(),
    AnimalName = readr::col_character(),
    RFID = readr::col_character(),
    StartTime = readr::col_datetime(format = "%Y-%m-%d %H:%M:%S"),
    EndTime = readr::col_datetime(format = "%Y-%m-%d %H:%M:%S"),
    GoodDataDuration = readr::col_time(),
    CO2GramsPerDay = readr::col_double(),
    CH4GramsPerDay = readr::col_double(),
    O2GramsPerDay = readr::col_double(),
    H2GramsPerDay = readr::col_double(),
    H2SGramsPerDay = readr::col_double(),
    AirflowLitersPerSec = readr::col_double(),
    AirflowCf = readr::col_double(),
    WindSpeedMetersPerSec = readr::col_double(),
    WindDirDeg = readr::col_double(),
    WindCf = readr::col_double(),
    WasInterrupted = readr::col_logical(),
    InterruptingTags = readr::col_character(),
    TempPipeDegreesCelsius = readr::col_double(),
    IsPreliminary = readr::col_logical(),
    RunTime = readr::col_datetime(format = "%Y-%m-%d %H:%M:%S")
  )
  actual_col_spec <- create_greenfeed_col_spec()
  expect_equal(actual_col_spec, expected_col_spec)
})

test_that("create_greenfeed_col_spec throws error for invalid type", {
  expect_error(
    create_greenfeed_col_spec("invalid"),
    "Invalid type argument. Must be either 'preliminary' or 'verified'."
  )
})

test_that("write_greenfeed_col_spec writes correct column specifications", {
  temp_file <- tempfile(fileext = ".rds")
  write_greenfeed_col_spec(temp_file)
  actual_col_spec <- readr::read_rds(temp_file)
  expected_col_spec <- create_greenfeed_col_spec()
  expect_equal(actual_col_spec, expected_col_spec)
})

test_that("write_greenfeed_col_spec throws error for invalid type", {
  expect_error(
    write_greenfeed_col_spec("temp.rds", type = "invalid"),
    "Invalid type argument. Must be either 'preliminary' or 'verified'."
  )
})

test_that("get_greenfeed_col_spec is the same as create_greenfeed_col_spec", {
  actual <- get_greenfeed_col_spec()
  expected <- create_greenfeed_col_spec()
  expect_identical(actual, expected)
})

test_that("get_greenfeed_schema returns a tibble with the correct col spec", {
  path <- testthat::test_path(
    "../../inst/extdata/preliminary-greenfeed-data.csv"
  )
  actual <- readr::read_csv(
    path,
    col_types = create_greenfeed_col_spec()
  )
  expected <- tibble::tibble(
    FeederID = NA_real_,
    AnimalName = NA_character_,
    RFID = NA_character_,
    StartTime = NA_character_,
    EndTime = NA_character_,
    GoodDataDuration = NA_character_,
    CO2GramsPerDay = NA_real_,
    CH4GramsPerDay = NA_real_,
    O2GramsPerDay = NA_real_,
    H2GramsPerDay = NA_real_,
    H2SGramsPerDay = NA_real_,
    AirflowLitersPerSec = NA_real_,
    AirflowCf = NA_real_,
    WindSpeedMetersPerSec = NA_real_,
    WindDirDeg = NA_real_,
    WindCf = NA_real_,
    WasInterrupted = NA,
    InterruptingTags = NA_character_,
    TempPipeDegreesCelsius = NA_real_,
    IsPreliminary = NA,
    RunTime = NA_character_
  )
  expect_equal(actual, expected)
})
