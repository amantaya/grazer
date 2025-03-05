#' Title
#'
#' @param x
#'
#' @return
#'
#' @examples
#'
#' @export
#' @importFrom units set_units
calc_plot_area <- function(dim_x, dim_y, units) {

    if (missing(dim_x) | missing(dim_y)) {
        stop("You must provide both dim_x and dim_y.")
    }

    if (dim_x <= 0 | dim_y <= 0) {
        stop("dim_x and dim_y must be positive.")
    }

    if (units != "cm" & units != "m" & units != "in" & units != "ft") {
        stop("units must be either 'cm', 'm', 'in', or 'ft'.")
    }

    # length of quadrat dimension X
    dim_x_units <- units::set_units(dim_x, units, mode = "standard")

    # length of quadrat dimension Y
    dim_y_units <- units::set_units(dim_y, units, mode = "standard")

    # calculate the area of the sampling frame
    sample_area <- dim_x_units*dim_y_units

    return(sample_area)
}

#' Title
#'
#' @param x
#'
#' @return
#'
#' @examples
#'
#' @export
#' @importFrom units set_units
calc_forage_prod <- function(sample_wt, sample_units, sample_area, output_units) {

    sample_wt <- units::set_units(sample_wt, sample_units, mode = "standard")

    sample_area_m2 <- units::set_units(sample_area, "m^2")

    sample_wt_per_m2 <- sample_wt/sample_area_m2

    if (output_units == "kg/ha") {
        forage_production <- units::set_units(sample_wt_per_m2, "kg/hectare")
    } else if (output_units == "lb/acre") {
        forage_production <- units::set_units(sample_wt_per_m2, "lb/acre")
    } else {
        stop("output_units must be either 'kg/ha' or 'lb/acre'.")
    }

    return(forage_production)

}
