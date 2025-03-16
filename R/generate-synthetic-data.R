#' Generate Electronic Identification (EID) numbers.
#'
#' @param n A numeric value corresponding to the number of
#' EID (Electronic Identifier) numbers to generate.
#'
#' @return A vector of EID numbers. Each EID number is a 15-digit integer.
#' The EID numbers are generated randomly and are unique within the set.
#'
#' @examples
#' generate_eid(n = 10)
#'
#' @export
generate_eid <- function(n) {
  # TODO - add a argument to generate EID numbers with a specific prefix
  eid_number <- sample(111111111111111:999999999999999, n, replace = FALSE)
  eid_number
}

#' Generate RFID numbers (alias for generate_eid).
#'
#' @param n A numeric value corresponding to the number of
#' RFID (Radio Frequency Identifier) numbers to generate.
#'
#' @return A vector of RFID numbers. Each RFID number is a 15-digit integer.
#' The RFID numbers are generated randomly and are unique within the set.
#'
#' @examples
#' generate_rfid(n = 10)
#'
#' @export
generate_rfid <- function(n) {
  generate_eid(n)
}

#' Generate synthetic GreenFeed data for testing or simulation.
#'
#' @param n_rows The number of rows of synthetic data to generate.
#' @param type The type of greenfeed data to generate.
#' The default is "preliminary". The other option is "verified".
#'
#' @return A CSV file containing synthetic GreenFeed data.
#' The data includes the following columns:
#' - FeederID: A numeric value corresponding to the feeder ID.
#' - AnimalName: A character value corresponding to the animal name.
#' - RFID: A character value corresponding to the RFID.
#' - StartTime: A datetime value corresponding to the start time.
#' - EndTime: A datetime value corresponding to the end time.
#' - GoodDataDuration: A time value corresponding to the length
#' of the observation in HH:MM:SS.
#' - CO2GramsPerDay: A numeric value corresponding to the carbon dioxide
#' in grams per day.
#' - CH4GramsPerDay: A numeric value corresponding to the methane
#' in grams per day.
#' - O2GramsPerDay: A numeric value corresponding to the oxygen
#' in grams per day.
#' - H2GramsPerDay: A numeric value corresponding to the hydrogen
#' in grams per day.
#' - H2SGramsPerDay: A numeric value corresponding to the hydrogen sulfide
#' in grams per day.
#' - AirflowLitersPerSec: A numeric value corresponding to airflow in
#' liters per second.
#' - AirflowCf: A numeric value corresponding to correction factor
#' used to adjust gas measurements.
#' - WindSpeedMetersPerSec: A numeric value corresponding to the wind speed
#' in meters per second.
#' - WindDirDeg: A numeric value corresponding to the wind direction in degrees.
#' - WindCf: A numeric value corresponding to the wind in cubic feet.
#' - WasInterrupted: A logical value indicating whether the data was
#' interrupted by another animal. If the data was interrupted,
#' the value is TRUE; otherwise, it is FALSE.
#' - InterruptingTags: A character value corresponding to the interrupting
#' RFID tags from the animals that interrupted the observation.
#' - TempPipeDegreesCelsius: A numeric value corresponding to the
#' temperature of the pipe in degrees Celsius.
#' - IsPreliminary: A logical value indicating whether the data is preliminary.
#' If the data is preliminary, the value is TRUE; otherwise, it is FALSE.
#' - RunTime: A datetime value corresponding to when the observation
#' was processed by C-Lock's pre-processor
#'
#' @importFrom stats runif
#' @importFrom stats rnorm
#' @importFrom utils write.csv
#' @importFrom lubridate ymd_hms
#' @importFrom hms as_hms
#'
#' @examples
#' \dontrun{
#' generate_greenfeed_data(n_rows = 1000, type = "preliminary")
#' }
#' @export
generate_greenfeed_data <- function(n_rows, type = "preliminary") {
  sample_data <- data.frame(
    FeederID = sample(100:800, n_rows, replace = TRUE),
    # NOTE: the 000000000 prefix is specific to the C-Lock System
    AnimalName = paste0("000000000", generate_eid(n = n_rows)),
    RFID = paste0("000000000", generate_eid(n = n_rows)),
    StartTime = sample(
      seq(
        lubridate::ymd_hms("2023-01-01 00:00:00"),
        lubridate::ymd_hms("2023-12-31 23:59:59"),
        by = "hour"
      ),
      n_rows,
      replace = TRUE
    ),
    EndTime = NA,
    GoodDataDuration = NA,
    CO2GramsPerDay = round(pmax(rnorm(n_rows, mean = 1000, sd = 100), 0), 2), # TODO adjust mean and SD
    CH4GramsPerDay = round(pmax(rnorm(n_rows, mean = 200, sd = 50), 0), 2), # TODO adjust mean and SD
    O2GramsPerDay = round(pmax(rnorm(n_rows, mean = 200, sd = 50), 0), 2), # TODO adjust mean and SD
    H2GramsPerDay = round(pmax(rnorm(n_rows, mean = 200, sd = 50), 0), 2), # TODO adjust mean and SD
    H2SGramsPerDay = round(pmax(rnorm(n_rows, mean = 200, sd = 50), 0), 2), # TODO adjust mean and SD
    AirflowLitersPerSec = round(runif(n_rows, 10, 30), 2),
    AirflowCf = round(runif(n_rows, 0, 2), 2),
    WindSpeedMetersPerSec = round(runif(n_rows, 0, 10), 2),
    WindDirDeg = round(runif(n_rows, 0, 360), 2),
    WindCf = round(runif(n_rows, 0, 2), 2),
    WasInterrupted = sample(c(TRUE, FALSE), n_rows, replace = TRUE),
    InterruptingTags = NA_character_,
    TempPipeDegreesCelsius = round(runif(n_rows, 0, 50), 2),
    IsPreliminary = ifelse(type == "preliminary", 1, 0),
    RunTime = sample(
      seq(
        lubridate::ymd_hms("2023-01-01 00:00:00"),
        lubridate::ymd_hms("2023-12-31 23:59:59"),
        by = "hour"
      ),
      n_rows,
      replace = TRUE
    )
  )

  # the EndTime is the StartTime plus a random duration
  # between 2 minutes and 20 minutes
  # this is in seconds
  sample_data$EndTime <- sample_data$StartTime + runif(n_rows, 120, 1200)

  sample_data$GoodDataDuration <- difftime(
    sample_data$EndTime,
    sample_data$StartTime,
    units = "secs"
  )
  sample_data$GoodDataDuration <- round(
    sample_data$GoodDataDuration,
    digits = 0
  )

  sample_data$GoodDataDuration <- hms::as_hms(sample_data$GoodDataDuration)

  sample_data <- sample_data[order(sample_data$StartTime), ]

  sample_data
}
