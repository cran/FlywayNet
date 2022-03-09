#' Generate a migration structure with a random structure.
#'
#' @description Generate a migration structure with a random structure.
#'
#' @param nb_sites, required number of site. Default is 4.
#' @param type_structure, Either "forward" or "two-directions". If "forward"
#'  the transitions go forward only. If "two-directions", forward and backwards 
#'  transitions  are possible. Default is "forward".
#' @param nb_max_neigh, the number of destinations of any site, unless structural 
#'  constraints prevent it. Default is 2. 
#' @param nb_birds, the number of birds on the initial sites. Birds are 
#'  distributed over all sources sites. Default is 100.
#' @param death_probability_mode Either "mean" or "unif" 
#'  (or a vector of such values of length the number of sites).  
#'  If "mean" death probability are set to the mean value of 
#'  the death_probability_domain. If "unif", the death probabilities are drawn 
#'  uniformly into the range of the death_probability_domain. Default is "mean".
#' @param death_probability_domain Min and max of death probabilities. 
#'  Default is c(0.01,0.05)
#' @param sojourn_mode Either "mean" or "unif" (or a vector of such values 
#'  of length the number of sites). If "mean" sojourns mean time is set to 
#'  the mean value of the sojourn_domain. If "unif", the sojourn mean time is 
#'  drawn uniformly into the range of the sojourn_domain. Default is "mean".
#' @param sojourn_domain Min and max of sojourn mean time. Default is c(1,10)
#' @param flight_duration_mode, Either "mean" or "unif". If "mean" flight 
#'  durations are set to the closest integer of the mean value of the 
#'  flight_duration_domain. If "unif", the flight durations are drawn uniformly
#'  into the range of the flight_duration_domain. Default is "mean". 
#' @param flight_duration_domain, Min and max integers of flight durations. 
#'  Default is c(1,5)
#' @param transition_mode Either "equi" or "unif". If "equi" transition 
#'  are equiprobable taking into account the link_knowledge. If "unif" 
#'  transitions are drawn uniformly. Default is "equi".
#' @param last_site_as_sink Boolean. If type_structure is set to "forward", 
#'  last site is the only sink of the network. If TRUE, the last site is 
#'  considered  as a specific sink where death probability is set to 1 and the 
#'  sojourn mean time is set to the sum of sojourn mean time of other sites.
#'  Default is TRUE.
#' @param observation_hide probability of hidding a single observation. 
#'  Default is 0.
#'
#' @return A new generated migration structure
#' @export
#'
#' @examples
#'  migr = generate_random_migration()


generate_random_migration = function(
  nb_sites=4, type_structure="forward", nb_max_neigh = 2, nb_birds=100,
  death_probability_mode="mean", death_probability_domain=c(0.01,0.05),
  sojourn_mode="mean", sojourn_domain=c(1,10), flight_duration_mode="mean",
  flight_duration_domain=c(1,5), transition_mode="equi", 
  last_site_as_sink=TRUE, observation_hide=0)
{
  #build link_knowledge
  I = nb_sites;
  link_knowledge = matrix(FALSE, ncol=I, nrow=I);
  if (type_structure == "forward") {
    for (i in (I-1):1) {
      if (i >= (I-nb_max_neigh)) {
        link_knowledge[i, (i+1):I] = TRUE;
      } else {
        sel = sample((i+1):I, size=nb_max_neigh, 
                     replace = FALSE, 
                     prob=rep(1/length((i+1):I), length((i+1):I)))
        link_knowledge[i, sel] = TRUE
      }
    }
  } else if (type_structure == "two-directions") {
    for (i in 1:I) {
      possible = setdiff(1:I, i)
      sel = sample(possible, size=nb_max_neigh, 
                   replace = FALSE, 
                   prob=rep(1/length(possible), length(possible)))
      link_knowledge[i, sel] = TRUE
    }
  }
  
  sink_sites = which(apply(link_knowledge, MARGIN=1, sum) == 0)
  source_sites = which(apply(link_knowledge, MARGIN=2, sum) == 0)
  
  #build initial state
  initial_state = rep(0, I)
  if (length(source_sites) > 0) {
    for (i in source_sites) {
      initial_state[i] = floor(nb_birds/length(source_sites))
    }
    initial_state[source_sites[1]] =  initial_state[source_sites[1]] + 
                                      nb_birds - sum(initial_state)
  } else {
    for (i in 1:I) {
      initial_state[i] = floor(nb_birds/I)
    }
    initial_state[1] = initial_state[1] + nb_birds - sum(initial_state)
  }
  migr = new_migration(site_name = paste(sep="", "site", 1:nb_sites),
                       link_knowledge=link_knowledge,
                       flight_duration=matrix(-1, nrow=I, ncol=I),
                       initial_state=initial_state,
                       horizon = NA,
                       death_probability=NA,
                       observation=NA)
  
  migr = generate_modified_migration(migr, death_probability_mode=death_probability_mode,
                                     death_probability_domain=death_probability_domain,
                                     sojourn_mode=sojourn_mode, sojourn_domain=sojourn_domain,
                                     flight_duration_mode=flight_duration_mode,
                                     flight_duration_domain=flight_duration_domain,
                                     transition_mode=transition_mode)
  if ((type_structure == "forward") & last_site_as_sink) {
    migr$sojourn_law_param[I] = sum(migr$sojourn_law_param[1:(I-1)])
    migr$death_probability[I] = 1
  }
  migr = generate_modified_migration(migr, observation_mode="simulation")
  if (observation_hide > 0) {
    migr = generate_modified_migration(migr, observation_mode="hide", 
                                       observation_hide=observation_hide)
  }
 
    
  
  return(migr);

}
