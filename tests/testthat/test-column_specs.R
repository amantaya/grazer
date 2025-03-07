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

test_that("get_greenfeed_schema reads preliminary data correctly", {

  path <- system.file("extdata",
    "preliminary-greenfeed-data.csv",
    package = "grazer"
  )
  actual_data <- get_greenfeed_schema(type = "preliminary")

  expected_data <- readr::read_csv(
    path,
    col_types = create_greenfeed_col_spec("preliminary")
  )
  expect_equal(actual_data, expected_data)
})

test_that("get_greenfeed_schema reads verified data correctly", {

  path <- system.file("extdata",
    "verified-greenfeed-data.csv",
    package = "grazer"
  )
  actual_data <- get_greenfeed_schema(type = "verified")
  actual_data <- get_greenfeed_schema(type = "verified")

  expected_data <- readr::read_csv(
    path,
    col_types = create_greenfeed_col_spec("verified")
  )
  expect_equal(actual_data, expected_data)
})

test_that("get_greenfeed_schema throws error for invalid type", {
  expect_error(
    get_greenfeed_schema(type = "invalid"),
    "Invalid type argument. Must be either 'preliminary' or 'verified'."
  )
})
