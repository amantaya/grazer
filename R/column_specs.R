#' Create a `readr` column specification for GreenFeed data.
#'
#' `readr` column specifications are used to specify the data types of each
#' column in a data frame when reading in data from a file. This function
#' creates a column specification for GreenFeed data.
#'
#' Column specifications are useful for ensuring that data is read in
#' correctly and that columns are parsed according to the correct data type.
#' For example, a column that contains dates should be parsed as a date
#' when reading in the data.
#'
#' @param type Specifies the type of the greenfeed data to create a
#' column specification for. Options are `preliminary` and `verified`.
#'
#' @return A `readr` column specification for the preliminary greenfeed data.
#'
#' @examples
#' \dontrun{
#' create_col_spec()
#' }
#' @export
create_greenfeed_col_spec <- function(type = "preliminary") {
  if (type == "preliminary" || type == "verified") {
    readr::cols(
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
  } else {
    stop("Invalid type argument. Must be either 'preliminary' or 'verified'.")
  }
}

#' Write GreenFeed column specification to an RDS file.
#'
#' @param file The name of the RDS file to write the column specification to.
#'
#' @param type Specifies the type of the greenfeed data to write a
#' column specification for. Options are `preliminary` and `verified`.
#'
#' @return An RDS file containing the column specification for the
#' preliminary greenfeed data.
#'
#' @examples
#' @export
#' @importFrom readr write_rds
write_greenfeed_col_spec <- function(file, type = "preliminary") {
  if (type == "preliminary" || type == "verified") {
    greenfeed_col_spec <- create_greenfeed_col_spec(type)
    readr::write_rds(
      greenfeed_col_spec,
      file = file
    )
  } else {
    stop("Invalid type argument. Must be either 'preliminary' or 'verified'.")
  }
}

#' Get the preliminary greenfeed data schema from a CSV file
#'
#' @param none A placeholder for the function to work with the
#' get_prelim_greenfeed_schema function in the roxygen2 documentation.
#'
#' @return A tibble with the standardized column names
#' for preliminary greenfeed data that has been read
#' in and parsed according to the
#' preliminary greenfeed data column specification.
#'
#' @examples
#' \dontrun{
#' get_prelim_greenfeed_schema()
#' }
#'
#' @export
#'
# TODO - refactor the function name to be get_greenfeed_schema()
# nd have a type argument
# that specifies the type of greenfeed data schema to get
get_prelim_greenfeed_schema <- function(none) {
  prelim_gf_schema <- readr::read_csv(
    system.file(
      "extdata",
      "preliminary-greenfeed-data-schema.csv",
      package = "grazer"
    ),
    col_types = create_greenfeed_col_spec()
  )
  prelim_gf_schema
}
