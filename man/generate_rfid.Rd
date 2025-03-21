% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate-data.R
\name{generate_rfid}
\alias{generate_rfid}
\title{Generate RFID numbers (alias for generate_eid).}
\usage{
generate_rfid(n)
}
\arguments{
\item{n}{A numeric value corresponding to the number of
RFID (Radio Frequency Identifier) numbers to generate.}
}
\value{
A vector of RFID numbers. Each RFID number is a 15-digit integer.
The RFID numbers are generated randomly and are unique within the set.
}
\description{
Generate RFID numbers (alias for generate_eid).
}
\examples{
generate_rfid(n = 10)

}
