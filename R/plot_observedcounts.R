#' Barplot observed counts at different sites and time steps.
#'
#' @description  Barplot observed counts at different sites and time steps.
#' 
#' @param migr A migration structure.
#' @param obs An observation matrix (S+2) x (H +1).
#'
#' @return No return value.
#' 
#' @export
#'
#' @examples
#' migr <- generate_toy_migration()
#' traj <- generate_trajectories( migr )
#' obs <- generate_observedcounts( migr, traj )
#' plot_observedcounts( migr,  obs )



plot_observedcounts <- function(migr, obs) {

  site_nb <-length(migr$site_name)
  bird_position_nb <- site_nb + 2
  horizon <- migr$horizon
  time_step_nb <- horizon + 1

  stopifnot( class(migr) == "migration" )
  stopifnot( all(dim(obs) == c(bird_position_nb, time_step_nb)) )

  obs <- obs[c(1:site_nb), 2:time_step_nb]

  legend_text <- migr$site_name
  name_text <- matrix("", 1, horizon)
  for (i in 1:horizon) {name_text[i] <- as.character(i)}

  graphics::barplot(obs,
                    main = "Observation of birds",
                    width = 0.7,
                    xlab = "time",
                    names.arg = name_text,
                    cex.names  = 0.5,
                    xlim = c(0, migr$horizon),
                    ylab = "number of bird present on sites",
                    ylim = c(0, sum(migr$initial_state)),
                    col = grDevices::rainbow(site_nb),
                    legend = legend_text,
                    args.legend = list( bty = "n", title = "States"))
}


