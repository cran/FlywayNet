% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_migration.R
\name{read_migration}
\alias{read_migration}
\title{Read a migration structure in a text file.}
\usage{
read_migration(file_name)
}
\arguments{
\item{file_name}{A file name.}
}
\value{
A migration structure or NULL if file does not exist.
}
\description{
Read a migration structure in a text file.
}
\examples{
migr <- generate_toy_migration()
write_migration( migr, "toy_migration.txt")
read_migration("toy_migration.txt")
unlink("toy_migration.txt") # delete the file to pass package check
}
