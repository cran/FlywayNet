#' Initialize a migration structure.
#'
#' @description  Initialize a migration structure.
#' 
#' @param site_name Ordered list of sites.
#' @param flight_duration Flight duration between sites.
#' @param link_knowledge Possible links between sites.
#' @param initial_state Initial count of birds on each site.
#' @param horizon Migration total duration.
#' @param death_probability Sites death probability.
#' @param transition_law_type Law of sites transition probability. Default value is 'multinomial'.
#' @param transition_law_param Sites transition probability. Default value computed with link_knowledge (uniform).
#' @param sojourn_law_type Law of site sojourn duration distribution. Default value is 'Poisson'.
#' @param sojourn_law_param Parameters of the law of site sojourn duration distribution. Default value is 1.
#' @param observation_law_type Law of observing birds for each site. Default value is 'Poisson'.
#' @param observation_law_param Parameters of the law of observing birds for each site. Default value is NULL.
#' @param observation Observed counts of birds.
#' 
#' @return A migration structure.
#' 
#' @export
#'
#' @examples
#' migr <- new_migration(
#'   site_name = c("s1", "s2", "s3"),
#'   flight_duration = matrix(c(0, 1, 2, 0, 0, 1, 0, 0, 0), ncol = 3, byrow = TRUE),
#'   initial_state = c(10, 0, 0),
#'   horizon = 5,
#'   death_probability = c(0.05, 0.1, 1),
#'   observation = matrix(c(9, 0, 0, 8, 0, 0, 2, 4, 1, 0, 0, 7), ncol = 3, byrow = TRUE)
#' )

 
new_migration <- function(site_name, link_knowledge = NA,
                           flight_duration, initial_state, horizon, death_probability,
                           transition_law_type = NA, transition_law_param = NA,
                           sojourn_law_type = NA, sojourn_law_param = NA,
                           observation_law_type = NA, observation_law_param = NA,
                           observation) {
  site_nb <- length(site_name)

  if (missing(link_knowledge)) {
    link_knowledge <- matrix(FALSE, nrow = site_nb, ncol = site_nb)
    link_knowledge[upper.tri(link_knowledge)] <- TRUE
  }

  if (missing(transition_law_type)) {
    transition_law_type <- "multinomial"
  }

  if (missing(transition_law_param)) {
    p <- link_knowledge
    p[p == FALSE] <- 0
    p[p == TRUE] <- 1
    p[site_nb, site_nb] <- 1
    nb <- rowSums(p)
    p <- p / rowSums(p)
    p <- p - death_probability/nb
    p[p<0] <- 0
    transition_law_param <- p[1:site_nb, 1:site_nb]
  }

  if (missing(sojourn_law_type)) {
    sojourn_law_type <- "Poisson"
  }

  if (missing(sojourn_law_param)) {
    sojourn_law_param <- matrix(1, nrow = 1, ncol = site_nb)
  }

  if (missing(observation_law_type)) {
    observation_law_type <- "Poisson"
  }

  if (missing(observation_law_param)) {
    observation_law_param <- NULL
  }

  migr <- list(
    site_name = site_name,
    link_knowledge = link_knowledge,
    flight_duration = flight_duration,
    initial_state = initial_state,
    horizon = horizon,
    death_probability = death_probability,

    transition_law_type = transition_law_type,
    transition_law_param = transition_law_param,
    sojourn_law_type = sojourn_law_type,
    sojourn_law_param = sojourn_law_param,
    observation_law_type = observation_law_type,
    observation_law_param = observation_law_param,

    observation = observation
  )
  class(migr) <- "migration"

  return(migr)
}
