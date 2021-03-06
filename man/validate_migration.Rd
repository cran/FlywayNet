% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate_migration.R
\name{validate_migration}
\alias{validate_migration}
\title{Check a migration structure.}
\usage{
validate_migration(migr)
}
\arguments{
\item{migr}{A migration structure.}
}
\value{
A list of error messages, list() if no error.
}
\description{
Check a migration structure and print a description of attributes values.
}
\examples{
migr <- generate_toy_migration()
msg <- validate_migration( migr )
}
