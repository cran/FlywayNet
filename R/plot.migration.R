#' Plot a migration network from transition probabilities and mean sojourn time parameters.
#'
#' @description  Plot a migration network from transition probabilities 
#'               and mean sojourn time parameters. 
#'               Currently just plot transition_law_param as edge width
#'              and sejourn_law_param as node size of the network
#'
#' @param x a migration structure
#' @param ... others default arguments
#'
#' @return An igraph structure.
#'
#' @export
#'
#' @examples
#' migr <- generate_toy_migration()
#' plot( migr )



plot.migration <- function(x, ...) {

  G <- x$link_knowledge
  rownames(G) <- x$site_name
  colnames(G) <- x$site_name

  g <- igraph::graph_from_adjacency_matrix(G,
                                           mode = "directed",
                                           weighted = TRUE,
                                           diag = FALSE,
                                           add.colnames = NULL,
                                           add.rownames = TRUE)

  transition_edge <- as.vector(t(x$transition_law_param))
  transition_edge <- transition_edge[ transition_edge != 0 ]

  graphics::plot(g,
                 main = "Migration network",
                 sub = "Site transition probability aside edge.",
                 vertex.size = x$sojourn_law_param * 10,
                 vertex.label.dist = 0,
                 vertex.color = grDevices::rainbow( length(x$site_name) ),
                 edge.arrow.size = 0.8,
                 edge.width = transition_edge * 5,
                 edge.label = transition_edge,
                 layout = igraph::layout_as_tree
  )

  return(g)
}
