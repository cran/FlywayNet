#' Barplot of birds counts par site and per time step, given bird trajectories.
#'
#' @description  Barplot of birds counts par site and per time step, given bird trajectories.
#'
#' @param migr A migration structure.
#' @param traj A trajectory matrix with a trajectory for each bird, matrix N x (horizon+1).
#' 
#' @return Barplot of birds counts and return a count matrix (S+2) x (horizon+1)
#' with, at each time step (0,...,horizon), specify the number of bird on each site (rows 1,...,S),
#' flying (row S+1) and dead (row S+2).
#' 
#' @export
#'
#' @examples
#' migr <- generate_toy_migration()
#' traj <- generate_trajectories( migr )
#' counts <- plot_trajectories( migr, traj )


plot_trajectories <- function(migr, traj) {

  stopifnot( class(migr) == "migration" )
  stopifnot( all(dim(traj) == c(sum(migr$initial_state),
                                          migr$horizon + 1)) )

  site_nb <-length(migr$site_name)+1 # add virtual death site
  horizon <- migr$horizon

  # count birds at each time step
  counts <- get_counts(migr, traj)

  legend_text <- migr$site_name
  legend_text[site_nb] <- "dead"
  legend_text[site_nb+1] <- "flying"
  name_text <- matrix("", 1, horizon+1)
  for (i in 1:(horizon+1)) {name_text[i] <- as.character(i)}
  
  graphics::barplot(counts,
                    main = "Evolution of counts given trajectories of birds",
                    width = 0.7,
                    xlab = "time step",
                    names.arg = name_text,
                    cex.names  = 0.5,
                    xlim = c(0, horizon+1),
                    ylab = "number of bird present on sites",
                    ylim = c(0, sum(migr$initial_state)),
                    col = c(grDevices::rainbow(site_nb-1),"black","gray"),
                    legend = legend_text, 
                    args.legend = list(bty = "n", title = "States"))

  return(counts)
}
