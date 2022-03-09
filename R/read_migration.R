#' Read a migration structure in a text file.
#'
#' @description Read a migration structure in a text file.
#'
#' @param file_name A file name.
#'
#' @return A migration structure or NULL if file does not exist.
#' 
#' @export
#'
#' @examples
#' migr <- generate_toy_migration()
#' write_migration( migr, "toy_migration.txt")
#' read_migration("toy_migration.txt")
#' unlink("toy_migration.txt") # delete the file to pass package check

read_migration <- function(file_name){
  stopifnot(file.exists(file_name))

  text <- utils::read.delim(file = file_name, sep = "[",header=FALSE)
  text <- text$V1

  line <- 2
  site_name <- unlist(strsplit(as.character(text[line]), " "))
  line <- line+2

  site_nb <- length(site_name)

  link_knowledge <- matrix(as.logical(unlist(utils::type.convert(strsplit(as.character(text[line]), " ")[[1]], as.is = TRUE))),
                                      nrow = site_nb, ncol = site_nb)
  line <- line+2
  flight_duration <- matrix(unlist(utils::type.convert(strsplit(as.character(text[line]), " ")[[1]], as.is = TRUE)),
                            nrow = site_nb, ncol = site_nb)
  line <- line+2
  initial_state <- as.numeric(unlist(utils::type.convert(strsplit(as.character(text[line]), " ")[[1]], as.is = TRUE)),
                                           nrow = 1, ncol = site_nb)
  line <- line+2
  horizon <- as.numeric(unlist(utils::type.convert(strsplit(as.character(text[line]), " ")[[1]], as.is = TRUE)),
                         nrow = 1, ncol = 1)
  line <- line+2
  death_probability <- as.numeric(unlist(utils::type.convert(strsplit(as.character(text[line]), " ")[[1]], as.is = TRUE)),
                                  nrow = 1, ncol = site_nb)
  line <- line+2
  transition_law_type <- as.character(text[line])
  line <- line+2
  transition_law_param <- matrix(unlist(utils::type.convert(strsplit(as.character(text[line]), " ")[[1]], as.is = TRUE)),
                                 nrow = site_nb, ncol = site_nb)
  line <- line+2
  sojourn_law_type <- as.character(text[line])
  line <- line+2
  sojourn_law_param <- matrix(unlist(utils::type.convert(strsplit(as.character(text[line]), " ")[[1]], as.is = TRUE)),
                              nrow = 1, ncol = site_nb)
  line <- line+2
  observation_law_type <- as.character(text[line])
  line <- line+2
  if (text[line] == "") {
    observation_law_param <- NULL
  } else {
    observation_law_param <- matrix(unlist(utils::type.convert(strsplit(as.character(text[line]), " ")[[1]], as.is = TRUE)),
                                                                      nrow = 1, ncol = site_nb)
  }
  line <- line+1

  obs <- unlist(utils::type.convert(strsplit(as.character(text[line]), " ")[[1]], as.is = TRUE))
  if (length(obs) > 1) {
    observation <- matrix(obs, ncol = horizon + 1)
  } else {
    observation <- NULL
  }


  migr <- new_migration(
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

  return( migr)
}

