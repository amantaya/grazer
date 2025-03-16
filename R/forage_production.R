#' Calculate the Area of a Sampling Frame
#'
#' @param dim_x The length of the quadrat in the x-dimension
#' @param dim_y The length of the quadrat in the y-dimension
#' @param units The units of the quadrat dimensions
#'
#' @return The area of the sampling frame in the units provided.
#'
#' @examples
#' calc_plot_area(dim_x = 30, dim_y = 30, units = "cm")
#'
#' @export
#' @importFrom units set_units
calc_plot_area <- function(dim_x, dim_y, units) {
  if (missing(dim_x) || missing(dim_y)) {
    stop("You must provide both dim_x and dim_y.")
  }

  if (dim_x <= 0 || dim_y <= 0) {
    stop("dim_x and dim_y must be positive.")
  }

  if (units != "cm" && units != "m" && units != "in" && units != "ft") {
    stop("units must be either 'cm', 'm', 'in', or 'ft'.")
  }

  # length of quadrat dimension X
  dim_x_units <- units::set_units(dim_x, units, mode = "standard")

  # length of quadrat dimension Y
  dim_y_units <- units::set_units(dim_y, units, mode = "standard")

  # calculate the area of the sampling frame
  sample_area <- dim_x_units * dim_y_units

  sample_area
}

#' Calculate Forage Production
#'
#' @param sample_wt The weight of the forage sample.
#' @param sample_units The units of the forage sample weight.
#' @param sample_area The area of the sampling frame.
#' Most likely calculated using \code{\link{calc_plot_area}}.
#' @param output_units The units to return the forage production in.
#' Either 'kg/ha' or 'lb/acre'.
#'
#' @return The forage production in the units provided.
#' Either 'kg/ha' or 'lb/acre'.
#'
#' @examples
#' sample_area <- calc_plot_area(
#'   dim_x = 50,
#'   dim_y = 50,
#'   units = "cm"
#' )
#' calc_forage_prod(
#'   sample_wt = 100,
#'   sample_units = "g",
#'   sample_area = sample_area,
#'   output_units = "kg/ha"
#' )
#'
#' @export
#' @importFrom units set_units
calc_forage_prod <- function(
    sample_wt,
    sample_units,
    sample_area,
    output_units) {
  sample_wt <- units::set_units(sample_wt, sample_units, mode = "standard")

  sample_area_m2 <- units::set_units(sample_area, "m^2")

  sample_wt_per_m2 <- sample_wt / sample_area_m2

  if (output_units == "kg/ha") {
    forage_production <- units::set_units(sample_wt_per_m2, "kg/hectare")
  } else if (output_units == "lb/acre") {
    forage_production <- units::set_units(sample_wt_per_m2, "lb/acre")
  } else {
    stop("output_units must be either 'kg/ha' or 'lb/acre'.")
  }
  forage_production
}
