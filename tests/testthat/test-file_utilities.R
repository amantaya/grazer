test_that("md5 hash of preliminary greenfeed data schema is correct", {
  path <- testthat::test_path(
    "../../inst/extdata/preliminary-greenfeed-data.csv"
  )
  actual_md5hash <- generate_md5_hash(path)
  actual_md5hash <- as.character(actual_md5hash)
  expected_md5hash <- "4710cdc12cd6f2fc62e2567dc27fe392"
  expect_equal(
    actual_md5hash,
    expected_md5hash
  )
})

test_that("sha256 hash of preliminary greenfeed data schema is correct", {
  path <- testthat::test_path(
    "../../inst/extdata/preliminary-greenfeed-data.csv"
  )
  actual_sha256hash <- generate_sha256_hash(path)
  actual_sha256hash <- as.character(actual_sha256hash)
  expected_sha256hash <- "454f3eea52d6a6e935825f2e0a6614825b02d454c6038e29206d6845dbf44c36" # nolint: line_length_linter
  expect_equal(
    actual_sha256hash,
    expected_sha256hash
  )
})

# test_that("strict_read_csv correctly parses data", {
#     expected_col_spec <- get_prelim_col_spec()
#     # get the preliminary greenfeed data schema
#     parsed_data <- strict_read_csv(
#         testthat::test_path(
#             "inst",
#             "data",
#             "preliminary-greenfeed-data.csv"
#         ) # TODO - create a test data file
#     )
#     # Define the expected data frame
#     expected_data <- tibble::tibble(
#         column1 = c("value1", "value2"),
#         column2 = c(1.0, 2.0),
#         column3 = as.Date(c("2021-01-01", "2021-01-02"))
#     ) # TODO - finish defining the expected data frame

#     # check if the preliminary greenfeed data schema is correct
#     expect_equal(
#         parsed_data,
#         expected_data
#         )
# })

test_that("when old csv file and new csv file are equal return the same hash", {
  old_file <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(
      column1 = c("value1", "value2"),
      column2 = c(1.0, 2.0),
      column3 = as.Date(c("2021-01-01", "2021-01-02"))
    ),
    old_file
  )
  new_file <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(
      column1 = c("value1", "value2"),
      column2 = c(1.0, 2.0),
      column3 = as.Date(c("2021-01-01", "2021-01-02"))
    ),
    new_file
  )
  expect_equal(compare_file_hashes(old_file, new_file), TRUE)
})

test_that("when old csv file and new csv file are not equal return different hashes", { # nolint: line_length_linter
  old_file <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(
      column1 = c("value1", "value2"),
      column2 = c(1.0, 2.0),
      column3 = as.Date(c("2021-01-01", "2021-01-02"))
    ),
    old_file
  )
  new_file <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(
      column1 = c("value1", "value3"),
      column2 = c(1.0, 2.0),
      column3 = as.Date(c("2021-01-01", "2021-01-02"))
    ),
    new_file
  )
  expect_warning(
    compare_file_hashes(old_file, new_file), "The file hashes do not match."
  )
  expect_equal(suppressWarnings(compare_file_hashes(old_file, new_file)), FALSE)
})

test_that("old text file and new text file are equal", {
  old_file <- tempfile(fileext = ".txt")
  writeLines("Hello, world!", old_file)
  new_file <- tempfile(fileext = ".txt")
  writeLines("Hello, world!", new_file)
  expect_equal(compare_file_hashes(old_file, new_file), TRUE)
})

test_that("old text file and new text file are not equal", {
  old_file <- tempfile(fileext = ".txt")
  writeLines("Hello, world!", old_file)
  new_file <- tempfile(fileext = ".txt")
  writeLines("Goodbye, world!", new_file)
  expect_warning(
    compare_file_hashes(old_file, new_file), "The file hashes do not match."
  )
  expect_equal(suppressWarnings(compare_file_hashes(old_file, new_file)), FALSE)
})
