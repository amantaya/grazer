#' Generate sha256 hash for a file
#'
#' @param file_path A path to a file.
#'
#' @return A sha256 hash for the file.
#'
#' @examples
#' temp_file <- tempfile(fileext = ".txt")
#' writeLines("Hello, world!", temp_file)
#' sha256_hash <- generate_sha256_hash(temp_file)
#'
#' @export
generate_sha256_hash <- function(file_path) {
  con <- file(file_path, "rb")
  file_hash <- openssl::sha256(con)
  close(con)
  file_hash <- as.character(file_hash)
  file_hash
}

#' Generate md5 hash for a file
#'
#' @param file_path A path to a file.
#'
#' @return A md5 hash for the file.
#'
#' @examples
#' temp_file <- tempfile(fileext = ".txt")
#' writeLines("Hello, world!", temp_file)
#' md5_hash <- generate_md5_hash(temp_file)
#'
#' @export
# function to open the file and create a md5 hash
generate_md5_hash <- function(file_path) {
  con <- file(file_path, "rb")
  file_hash <- openssl::md5(con)
  file_hash <- as.character(file_hash)
  close(con)
  file_hash
}

#' @title compare_file_hashes
#'
#' @param old_file Path to the old file.
#' @param new_file Path to the new file.
#'
#' @return A Boolean value (TRUE/FALSE)
#' indicating whether the file hashes match.
#'
#' @examples
#' old_file <- tempfile(fileext = ".txt")
#' new_file <- tempfile(fileext = ".txt")
#' writeLines("Hello, world!", old_file)
#' writeLines("Hello, world!", new_file)
#' compare_file_hashes(old_file, new_file) # returns TRUE
#'
#' old_file <- tempfile(fileext = ".txt")
#' new_file <- tempfile(fileext = ".txt")
#' writeLines("Hello, world!", old_file)
#' writeLines("Goodbye, world!", new_file)
#' compare_file_hashes(old_file, new_file) # returns FALSE
#'
#' @export
compare_file_hashes <- function(old_file, new_file) {
  old_file_hash <- generate_sha256_hash(old_file)
  new_file_hash <- generate_sha256_hash(new_file)

  if (old_file_hash != new_file_hash) {
    warning("The file hashes do not match.")
  }
  old_file_hash == new_file_hash
}
