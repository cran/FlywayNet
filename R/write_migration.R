#' Write a migration structure in a text file.
#'
#' @description Write a migration structure in a text file.
#' If file exists, just print a warning and erase file.
#'
#' @param migr A migration structure.
#' @param file_name A file name.
#'
#' @return No returned value.
#' 
#' @export
#'
#' @examples
#' migr <- generate_toy_migration()
#' write_migration( migr, "toy_migration.txt" )
#' unlink("toy_migration.txt") # delete the file to pass package check

write_migration <- function(migr, file_name) {

  stopifnot( class(migr) == "migration" )

  if (file.exists(file_name)) {
    warning("A file allready exists and is overwritten.")
  }

  cat("[site names]\n", file = file_name)
  cat(migr$site_name, file = file_name, append = TRUE, sep = " ")
  cat("\n[site relations]\n", file = file_name, append = TRUE)
  cat(migr$link_knowledge, file = file_name, append = TRUE, sep = " ")
  cat("\n[flight duration between site]\n", file = file_name, append = TRUE)
  cat(migr$flight_duration, file = file_name, append = TRUE, sep = " ")
  cat("\n[initial state]\n", file = file_name, append = TRUE)
  cat(as.character(migr$initial_state), file = file_name, append = TRUE, sep = " ")
  cat("\n[horizon]\n", file = file_name, append = TRUE)
  cat(as.character(migr$horizon), file = file_name, append = TRUE, sep = " ")
  cat("\n[death probability]\n", file = file_name, append = TRUE)
  cat(as.character(migr$death_probability), file = file_name, append = TRUE, sep = " ")

  cat("\n[site transition law type]\n", file = file_name, append = TRUE)
  cat(as.character(migr$transition_law_type), file = file_name, append = TRUE, sep = " ")
  cat("\n[site transition law parameters]\n", file = file_name, append = TRUE)
  cat(as.character(migr$transition_law_param), file = file_name, append = TRUE, sep = " ")
  cat("\n[site sojourn time law type]\n", file = file_name, append = TRUE)
  cat(as.character(migr$sojourn_law_type), file = file_name, append = TRUE, sep = " ")
  cat("\n[site sojourn time law parameters]\n", file = file_name, append = TRUE)
  cat(as.character(migr$sojourn_law_param), file = file_name, append = TRUE, sep = " ")
  cat("\n[site observation law type]\n", file = file_name, append = TRUE)
  cat(as.character(migr$observation_law_type), file = file_name, append = TRUE, sep = " ")
  cat("\n[site observation law parameters]\n", file = file_name, append = TRUE)
  cat(as.character(migr$observation_law_param), file = file_name, append = TRUE, sep = " ")

  cat("\n[observation]\n", file = file_name, append = TRUE)
  cat(as.character(migr$observation), file = file_name, append = TRUE, sep = " ")
  cat("\n", file = file_name, append = TRUE)
}
