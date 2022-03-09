#' Generate a toy migration structure with 5 sites.
#'
#' @description  Generate a toy migration structure with 5 sites, for inference or simulation. 
#'
#' @return A migration structure.
#' 
#' @export
#'
#' @examples
#' toy_migration <- generate_toy_migration()

generate_toy_migration <- function() {

  horizon <- 20
  time_step_nb <- horizon + 1
  site_nb <- 5
  bird_position_nb <- site_nb + 2 # sites, death, flying

  toy_migration <- list(
    site_name = c("departure", "site1", "site2", "site3", "arrival"),
    link_knowledge = matrix(data = as.logical(c(
      0, 1, 0, 0, 0,
      0, 0, 1, 1, 0,
      0, 0, 0, 0, 1,
      0, 0, 0, 0, 1,
      0, 0, 0, 0, 0
    )), nrow = site_nb, ncol = site_nb, byrow = TRUE),
    flight_duration = matrix(data = c(
      0, 1, 0, 0, 0,
      0, 0, 1, 2, 0,
      0, 0, 0, 0, 2,
      0, 0, 0, 0, 1,
      0, 0, 0, 0, 0
    ), nrow = site_nb, ncol = site_nb, byrow = TRUE),
    initial_state = c(100, 0, 0, 0, 0),
    horizon = 20,
    death_probability = c(0.1, 0.2, 0.2, 0.1, 1),
    transition_law_type = "multinomial",
    transition_law_param = matrix(data = c(
      0, 0.9, 0, 0, 0,
      0, 0, 0.55, 0.25, 0,
      0, 0, 0, 0, 0.8,
      0, 0, 0, 0, 0.9,
      0, 0, 0, 0, 0
    ), nrow = site_nb, ncol = site_nb, byrow = TRUE),
    sojourn_law_type = "Poisson",
    sojourn_law_param = c(3, 4, 2, 4, 5),
    observation_law_type = "Poisson",
    observation_law_param = NULL,
    observation = matrix(c(
      NA,   91,   58,   30,   11,    6,    2,    2,    0,     0,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,
      NA,    3,   18,   49,   51,   45,   46,   33,   20,    12,    11,     5,     7,     6,     3,     0,     0,    NA,    NA,    NA,    NA,
      NA,    0,    0,    0,    3,   12,   20,    5,    8,    22,     7,     4,     2,     5,     2,     0,     0,     1,     2,     1,     0,
      NA,    0,    0,    0,    0,    1,    1,    0,   11,     9,     4,     5,     7,     7,    10,     8,     4,     1,     2,     0,     0,
      NA,    0,    0,    0,    0,    0,    0,    0,    0,    16,    25,    15,    37,    39,    44,    49,    41,    49,    47,    58,    52,
      NA,   NA,   NA,   NA,   NA,   NA,   NA,   NA,   NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,
      NA,   NA,   NA,   NA,   NA,   NA,   NA,   NA,   NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA
    ), ncol = time_step_nb, byrow = TRUE)
  )
  class(toy_migration) <- "migration"

  toy_migration
}
