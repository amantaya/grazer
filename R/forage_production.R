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
get_plot_area <- function(dim_x, dim_y, units) {

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
    dim_x <- units::set_units(dim_x, units)

    # length of quadrat dimension Y
    dim_y <- units::set_units(dim_y, units)

    # calculate the area of the sampling frame
    sample_area <- dim_x*dim_y

    return(sample_area)
}