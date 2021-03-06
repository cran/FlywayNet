% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/new_migration.R
\name{new_migration}
\alias{new_migration}
\title{Initialize a migration structure.}
\usage{
new_migration(
  site_name,
  link_knowledge = NA,
  flight_duration,
  initial_state,
  horizon,
  death_probability,
  transition_law_type = NA,
  transition_law_param = NA,
  sojourn_law_type = NA,
  sojourn_law_param = NA,
  observation_law_type = NA,
  observation_law_param = NA,
  observation
)
}
\arguments{
\item{site_name}{Ordered list of sites.}

\item{link_knowledge}{Possible links between sites.}

\item{flight_duration}{Flight duration between sites.}

\item{initial_state}{Initial count of birds on each site.}

\item{horizon}{Migration total duration.}

\item{death_probability}{Sites death probability.}

\item{transition_law_type}{Law of sites transition probability. Default value is 'multinomial'.}

\item{transition_law_param}{Sites transition probability. Default value computed with link_knowledge (uniform).}

\item{sojourn_law_type}{Law of site sojourn duration distribution. Default value is 'Poisson'.}

\item{sojourn_law_param}{Parameters of the law of site sojourn duration distribution. Default value is 1.}

\item{observation_law_type}{Law of observing birds for each site. Default value is 'Poisson'.}

\item{observation_law_param}{Parameters of the law of observing birds for each site. Default value is NULL.}

\item{observation}{Observed counts of birds.}
}
\value{
A migration structure.
}
\description{
Initialize a migration structure.
}
\examples{
migr <- new_migration(
  site_name = c("s1", "s2", "s3"),
  flight_duration = matrix(c(0, 1, 2, 0, 0, 1, 0, 0, 0), ncol = 3, byrow = TRUE),
  initial_state = c(10, 0, 0),
  horizon = 5,
  death_probability = c(0.05, 0.1, 1),
  observation = matrix(c(9, 0, 0, 8, 0, 0, 2, 4, 1, 0, 0, 7), ncol = 3, byrow = TRUE)
)
}
