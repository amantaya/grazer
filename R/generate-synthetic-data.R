#' Title
#'
#' @param seed An optional seed for reproducibility.
#'
#' @return A numeric value corresponding to the RFID.
#' The first 9 digits are all zeros. The last 15 digits are randomly generated.
#'
#' @examples
#' generate_eid(seed = 123)
#'
#' @export
# TODO - create an alias for this function called `generate_rfid`
generate_eid <- function(seed = NULL) {
  # if seed is not NULL, set the supplied seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  } else {
    # TODO - remove this line. Seeds should be set by the user.
    set.seed(123)
  }
  # Generate the last 15 digits as a random number
  last_15_digits <- sample(0:999999999999999, 1)
  # Concatenate the first 9 digits (all zeros) with the last 15 digits
  # to create a 24-digit RFID
  # this converts the number to a string
  # TODO - the leading zeros are specific to C-Lock and we should have an option to remove them
  rfid <- paste0("000000000", last_15_digits)
  # convert the string to a number
  as.numeric(rfid)
  return(rfid)
}

#' Generate synthetic GreenFeed data for testing or simulation.
#'
#' @param path A path to where the synthetic data should be saved.
#' @param n The number of rows of synthetic data to generate.
#' @param seed An optional seed for reproducibility.
#'
#' @return A CSV file containing synthetic GreenFeed data.
#' The data includes the following columns:
#' - FeederID: A numeric value corresponding to the feeder ID.
#' - AnimalName: A character value corresponding to the animal name.
#' - RFID: A character value corresponding to the RFID.
#' - StartTime: A datetime value corresponding to the start time.
#' - EndTime: A datetime value corresponding to the end time.
#' - GoodDataDuration: A time value corresponding to the good data duration.
#' - CO2GramsPerDay: A numeric value corresponding to the CO2 grams per day.
#' - CH4GramsPerDay: A numeric value corresponding to the CH4 grams per day.
#' - O2GramsPerDay: A numeric value corresponding to the O2 grams per day.
#' - H2GramsPerDay: A numeric value corresponding to the H2 grams per day.
#' - H2SGramsPerDay: A numeric value corresponding to the H2S grams per day.
#' - AirflowLitersPerSec: A numeric value corresponding to the airflow in liters per second.
#' - AirflowCf: A numeric value corresponding to the airflow in cubic feet.
#' - WindSpeedMetersPerSec: A numeric value corresponding to the wind speed in meters per second.
#' - WindDirDeg: A numeric value corresponding to the wind direction in degrees.
#' - WindCf: A numeric value corresponding to the wind in cubic feet.
#' - WasInterrupted: A logical value indicating whether the data was interrupted.
#' - InterruptingTags: A character value corresponding to the interrupting tags.
#' - TempPipeDegreesCelsius: A numeric value corresponding to the temperature of the pipe in degrees Celsius.
#' - IsPreliminary: A logical value indicating whether the data is preliminary.
#' - RunTime: A datetime value corresponding to the run time.
#'
#' @importFrom stats runif
#' @importFrom utils write.csv
#' @importFrom lubridate ymd_hms
#' @importFrom hms as_hms
#'
#' @examples
#' \dontrun{
#' generate_greenfeed_data("data/synthetic_prelim_greenfeed_data.csv", 1000)
#' }
#' @export
generate_greenfeed_data <- function(n, type = "preliminary") {
  sample_data <- data.frame(
    FeederID = sample(100:800, n, replace = TRUE),
    AnimalName = as.character(replicate(n, generate_eid())),
    RFID = as.character(replicate(n, generate_eid())),
    StartTime = sample(
      seq(
        lubridate::ymd_hms("2023-01-01 00:00:00"),
        lubridate::ymd_hms("2023-12-31 23:59:59"),
        by = "hour"
      ),
      n,
      replace = TRUE
    ),
    EndTime = NA,
    GoodDataDuration = NA,
    CO2GramsPerDay = round(pmax(rnorm(n, mean = 1000, sd = 100), 0), 2), # TODO adjust mean and SD
    CH4GramsPerDay = round(pmax(rnorm(n, mean = 200, sd = 50), 0), 2), # TODO adjust mean and SD
    O2GramsPerDay = round(pmax(rnorm(n, mean = 200, sd = 50), 0), 2), # TODO adjust mean and SD
    H2GramsPerDay = round(pmax(rnorm(n, mean = 200, sd = 50), 0), 2), # TODO adjust mean and SD
    H2SGramsPerDay = round(pmax(rnorm(n, mean = 200, sd = 50), 0), 2), # TODO adjust mean and SD
    AirflowLitersPerSec = round(runif(n, 10, 30), 2),
    AirflowCf = round(runif(n, 0, 2), 2),
    WindSpeedMetersPerSec = round(runif(n, 0, 10), 2),
    WindDirDeg = round(runif(n, 0, 360), 2),
    WindCf = round(runif(n, 0, 2), 2),
    WasInterrupted = sample(c(TRUE, FALSE), n, replace = TRUE),
    InterruptingTags = NA_character_,
    TempPipeDegreesCelsius = round(runif(n, 0, 50), 2),
    IsPreliminary = ifelse(type == "preliminary", 1, 0),
    RunTime = sample(
      seq(
        lubridate::ymd_hms("2023-01-01 00:00:00"),
        lubridate::ymd_hms("2023-12-31 23:59:59"),
        by = "hour"
      ),
      n,
      replace = TRUE
    )
  )

  # the EndTime is the StartTime plus a random duration
  # between 2 minutes and 20 minutes
  # this is in seconds
  sample_data$EndTime <- sample_data$StartTime + runif(n, 120, 1200)

  sample_data$GoodDataDuration <- difftime(
    sample_data$EndTime,
    sample_data$StartTime,
    units = "secs"
  )
  sample_data$GoodDataDuration <- round(sample_data$GoodDataDuration, digits = 0)

  sample_data$GoodDataDuration <- hms::as_hms(sample_data$GoodDataDuration)

  sample_data <- sample_data[order(sample_data$StartTime), ]

  sample_data
}
