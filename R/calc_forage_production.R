#' Title
#'
#' @param x
#'
#' @return
#'
#' @examples
#'
#' @export
#' @import units
calc_forage_production <- function(dim_x, dim_y, plot_units, output_units) {

    if (missing(dim_x) | missing(dim_y)) {
        stop("You must provide both dim_x and dim_y.")
    }

    if (dim_x <= 0 | dim_y <= 0) {
        stop("dim_x and dim_y must be positive.")
    }

    if (plot_units != "cm" | plot_units != "m" | plot_units != "in" | plot_units != "ft") {
        stop("plot_units must be either 'cm', 'm', 'in', or 'ft'.")
    }

    # length of quadrat dimension X
    dim_x <- units::set_units(dim_x, plot_units)

    # length of quadrat dimension Y
    dim_y <- units::set_units(dim_y, plot_units)

    # calculate the area of the sampling frame
    sample_area <- dim_x*dim_y

    if (missing(plot_size_cm) && missing(plot_size_in)) {
        stop("You must provide either plot_size_cm or plot_size_in.")
    }

    if (!missing(plot_size_cm) && !missing(plot_size_in)) {
        stop("You must provide either plot_size_cm or plot_size_in, not both.")
    }
    if (!missing(plot_size_cm)) {
        plot_size_in <- as_units(plot_size_cm, "cm") %>% set_units("in")
    }

    if (output_units == "kg") {
        return(plot_size_in %>% set_units("m^2") %>% set_units("ha") %>% set_units("kg"))
    } else if (output_units == "lb") {
        return(plot_size_in %>% set_units("m^2") %>% set_units("ha") %>% set_units("lb"))
    } else {
        stop("output_units must be either 'kg' or 'lb'.")
    }
}