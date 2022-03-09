#' Extract free parameter values from a migration structure.
#'
#' @description Get values of free parameters. Indeed taking into account sites order,
#' known impossible sites links, sites dead probability, not all the sites transition probabilities
#' have to be estimated. Others parameters values may be evaluated from free parameters.
#'
#' @param migr A migration structure.
#' @param update_transitions If TRUE, transitions are considered 
#'   unknown and their values should be returned. 
#' @param update_sojourns Vector of boolean, that identifies which sojourn times
#'   should be returned.
#' @param use_adhoc_dirichlet If TRUE, the transition probabilities are 
#'  given in the ad hoc Dirichlet distribution (\eqn{q_ij} values). See details.
#'  
#' @return return named vector of default values of parameters to estimate.
#'   
#' @details 
#' 
#' Let N be number of sites, \eqn{d_i} the death probability at a given site i, 
#' \eqn{p_ij} the transition probability from site i to site j.
#' Then we must have a multinomial probability distribution that respects :
#' \eqn{p_i1 + ... + p_iN + di = 1}. Drawing \eqn{p_i1}, ..., \eqn{p_iN} 
#' so that this set of values forms a multinomial distribution can be done with 
#' the multivariate Dirichlet distribution which is difficult to handle in 
#' estimation processes (especially ABC). The adhoc dirichlet proposed here 
#' is a way to provide a bijection between a multinomial distribution and a 
#' a set of uniform distribution in [0:1]. This way, from N values 
#' \eqn{q_ij}  drawn uniformely in [0:1], we can compute 
#' \eqn{p_ij} with the constraint of the multinomial 
#' distribution. And inversely, if we have the  multinomial distribution then 
#' we can compute uniquely the   \eqn{q_ij} values. A way to do it is to define 
#' recursively the \eqn{p_ij} values as 
#' \deqn{p_ij = q_ij * (1- d_i - p_i1 - ... - p_i(j-1))}
#'
#' Also, the \eqn{d_i} values are expected to be known and the last 
#' probability transition from one site i (eg. \eqn{p_iN}) can be computed 
#' from others  \eqn{p_ij} and \eqn{d_i}. Finally, the 'link_knowledge' 
#' identifies the transitions that are not possible (with probability 0)
#' which are also removed from the extraction.
#' 
#' @export
#'
#' @examples
#'  migr <- generate_toy_migration()
#'  get_freeparametersvalue(migr) 
 

get_freeparametersvalue = function(migr, 
                                   update_transitions = TRUE, 
                                   update_sojourns = NULL,
                                   use_adhoc_dirichlet = FALSE)
{
  def_values = NULL
  def_names = NULL
  
  I = length(migr$site_name)

  if (is.null(update_sojourns)) {
    update_sojourns = c(rep(TRUE, I-1), FALSE)
  }
  
  #fill transition parameters
  if (update_transitions) {
    for (i in 1:I) {
      ## q_ij = p_ij /(1 - d_i - sum_k=1^k=j-1 p_ik)
      d_i = migr$death_probability[i]
      sum_k = 0
      for (j in 1:I) {
        is_a_link = migr$link_knowledge[i,j]
        if (is_a_link) {
          is_last_link  = sum(migr$link_knowledge[i,j:I]) == 1
          
          p_ij = migr$transition_law_param[i,j]
          q_ij = p_ij /(1 - d_i - sum_k);
          sum_k = sum_k+p_ij
          if (is_last_link) {
            #just check
            stopifnot(abs(q_ij - 1) < 1e-5)
          } else {
            trans_name = paste(paste(migr$site_name[i], sep=""), "to",
                               paste(migr$site_name[j], sep=""), sep="_")
            if (use_adhoc_dirichlet) { 
              def_values = c(def_values, q_ij)
            } else {
              def_values = c(def_values, p_ij)
            }
            def_names = c(def_names, trans_name)
          }
        }
      }
    }
  }
  ## fill sojourns transitions
  for (i in 1:I) {
    if (update_sojourns[i]){
      def_values = c(def_values, migr$sojourn_law_param[i])
      def_names = c(def_names, paste(sep="_", "soj", migr$site_name[i]))
    }
  }
  names(def_values) <- def_names
  return (def_values);
}