#' @title Validate EID Length
#'
#' @param eid A character string representing
#' the electronic identification number (EID) of an animal.
#'
#' @return A logical value indicating whether the EID is of the correct length.
#'
#' @examples
#' valid_eid <- "123456789012345"
#' validate_eid_length(valid_eid) # returns TRUE
#' invalid_eid <- "12345678901234"
#' validate_eid_length(invalid_eid) # returns FALSE
#'
#' @export
validate_eid_length <- function(eid) {
  if (is.na(eid)) {
    return(FALSE)
  }
  valid_eid <- (nchar(eid) == 15)
  if (!valid_eid) {
    message("EID is not 15 characters in length.")
  }
  valid_eid
}

#' @title Validate EID Prefix
#'
#' @param eid A character string representing
#' the electronic identification number (EID) of an animal.
#'
#' @return A logical value indicating whether the EID has a valid prefix.
#'
#' @examples
#' valid_eid <- "982012345678901"
#' validate_eid_prefix(valid_eid) # returns TRUE
#' invalid_eid <- "123456789012345"
#' validate_eid_prefix(invalid_eid) # returns FALSE
#' @export
validate_eid_prefix <- function(eid) {
  if (is.na(eid)) {
    return(FALSE)
  }
  prefix <- substr(eid, 1, 3)
  # NOTE: this is not a complete list of acceptable prefixes
  acceptable_prefixes <- c(
    "840", # USA Country Code - USDA AIN Complaint
    "982", # Allflex
    "985", # Destron Fearing
    "942", # Zee Tags
    "949" # Y-Tex
  )
  valid_eid_prefix <- prefix %in% acceptable_prefixes
  if (!valid_eid_prefix) {
    message("EID does not have a valid prefix.")
  }
  valid_eid_prefix
}

#' @title Get Length of EID
#'
#' @param eid A character string representing
#' the electronic identification number (EID) of an animal.
#'
#' @return An integer representing the length of the EID.
#'
#' @examples
#' eid_length("123456789012345") # returns 15
#'
#' @export
#' @importFrom stringr str_length
eid_length <- function(eid) {
  eid_length <- stringr::str_length(eid)
  eid_length
}

#' @title Get the Manufacturer or Type of an EID
#'
#' @param eid A character string representing
#' the electronic identification number (EID) of an animal.
#'
#' @return A character string representing the manufacturer or type of the EID.
#'
#' @examples
#' eid_type("982012345678901") # returns "Allflex"
#' eid_type("985012345678901") # returns "Destron Fearing"
#' eid_type("840012345678901") # returns "USDA Animal Identification Number"
#' eid_type("123456789012345") # returns NA_character_
#' eid_type(NA) # returns NA_character_
#' @export
eid_type <- function(eid) {
  valid_eid_length <- validate_eid_length(eid)
  valid_eid_prefix <- validate_eid_prefix(eid)
  if (is.na(eid)) {
    return(NA_character_)
  }
if (valid_eid_prefix == FALSE) {
    return(NA_character_)
  }
  if (valid_eid_length && valid_eid_prefix) {
    prefix <- eid_prefix(eid)
    # NOTE: this is not a complete list of manufacturers
    if (prefix == "840") {
      "USDA Animal Identification Number"
    } else if (prefix == "982") {
      "Allflex"
    } else if (prefix == "985") {
      "Destron Fearing"
    } else if (prefix == "942") {
      "Zee Tags"
    } else if (prefix == "949") {
      "Y-Tex"
    } else {
      NA_character_
    }
  }
}

#' Get the Prefix of an EID
#'
#' @param eid A character string representing
#' the electronic identification number (EID) of an animal.
#'
#' @return A character string with the first three digits of the EID.
#'
#' @examples
#' eid_prefix("982012345678901") # returns "982"
#' eid_prefix("985012345678901") # returns "985"
#'
#' @export
eid_prefix <- function(eid) {
  if (is.na(eid)) {
    NA_character_
  } else {
    substr(eid, 1, 3)
  }
}

# TODO - function to check for invalid dates in a column
