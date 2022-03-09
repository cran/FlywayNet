#' Assign free parameter values to a migration structure.
#'
#' @description Set values to free parameters. Indeed taking into account sites order,
#' known impossible sites links, sites dead probability, not all the sites transition probabilities
#' have to be estimated. Others parameters values may be evaluated from free parameters.
#' 
#' @param migr A migration structure.
#' @param values Parameter values to set to the migration structure.
#'   these are the transition probabilities and sojourn mean time.
#' @param update_transitions If TRUE, transitions are unknown and the values 
#'   should be given in the vector 'values'.
#' @param update_sojourns Vector of boolean, that identifies which sojourn times
#'   should be given. 
#' @param use_adhoc_dirichlet If TRUE, the transition probabilities are 
#'  considered as the ad hoc dirichlet (\eqn{q_ij} values)
#'  explained in the documentation of the extract_parameters function.
#'  
#' @return A migration structure with \emph{migr} attributes except\emph{transition_law_param} and \emph{sojourn_law_param}
#' that are changed.
#' 
#' @export
#'
#' @examples
#'  migr <- generate_toy_migration()
#'  param <- get_freeparametersvalue(migr) # just for example purpose, should be different values
#'  set_freeparametersvalue(migr, param)


set_freeparametersvalue = function(migr, 
                                   values, 
                                   update_transitions = TRUE, 
                                   update_sojourns = NULL,
                                   use_adhoc_dirichlet = FALSE) 
{
  stopifnot( class(migr) == "migration" )
  stopifnot( is.numeric(values))
  I = length(migr$site_name)
  if (is.null(update_sojourns)) {
    update_sojourns = c(rep(TRUE, I-1), FALSE)
  }
  
  #Ut the number of unknown transition probabilities
  Ut = 0;
  if (update_transitions) {
    Ut = sum(apply(migr$link_knowledge, MARGIN=1, 
                   function(x){max(0, sum(x==TRUE)-1)}))
  }
  #Us the number of unknown sojourn mean time
  Us = sum(update_sojourns == TRUE)
  if (Us+Ut != length(values)) {
    stop (paste(' length of values must be ', Us+Ut))
  }
  ## now assign parameter values
  index_param = 1;
  ### fill the transition matrix
  if (update_transitions){
    for (i in 1:I) {
      ## p_ij = q_ij * (1- d_i - sum_k=1^k=j-1 p_ik)
      d_i = migr$death_probability[i]
      sum_k = 0
      for (j in 1:I) {
        is_a_link = migr$link_knowledge[i,j]
        if (is_a_link){
          is_last_link  = sum(migr$link_knowledge[i,j:I]) == 1
          if (is_last_link) {
            q_ij = 1.0;
            p_ij = q_ij * (1- d_i - sum_k)
            sum_k = sum_k+p_ij;
            migr$transition_law_param[i, j] = p_ij
            #just check
            stopifnot(abs(sum_k + d_i - 1) < 1e-5)
          } else {
            p_ij = values[index_param];
            if (use_adhoc_dirichlet) {
              q_ij = values[index_param]
              p_ij = q_ij * (1- d_i - sum_k)
            }
            sum_k = sum_k+p_ij;
            index_param = index_param+1;
            migr$transition_law_param[i, j] = p_ij
          }
        }
      }
    }
  }
  ### fill the sojourn mean time
  for (i in 1:I) {
    if (update_sojourns[i]){
      toassign = values[index_param]
      index_param = index_param+1;
      migr$sojourn_law_param[i] = toassign;
    }
  }
  validate_migration(migr)
  return(migr)
}