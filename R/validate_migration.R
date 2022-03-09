#' Check a migration structure.
#'
#' @description  Check a migration structure and print a description of attributes values.
#' 
#' @param migr A migration structure.
#'
#' @return A list of error messages, list() if no error.
#' 
#' @export
#'
#' @examples
#' migr <- generate_toy_migration()
#' msg <- validate_migration( migr )

validate_migration <- function(migr) {

  stopifnot( class(migr) == "migration" )

  msg <- list()
  i_msg <- 1
  site_nb <- 0

  if (class(migr)!="migration") {
    msg[[i_msg]] <- "Not defined as class migration."
    i_msg <- i_msg + 1
  }

  if (!is.element("site_name", names(migr))) {
    msg[[i_msg]] <- "No site_name attribute."
    i_msg <- i_msg + 1
  } else {
    site_nb <- length(migr$site_name)
    if (site_nb < 3) {
      msg[[i_msg]] <- "Less than 3 site names (minimum size problem)."
      i_msg <- i_msg + 1
    }
  }

  if (is.element("link_knowledge", names(migr))) {
    if (any(c(site_nb, site_nb) != dim(migr$link_knowledge))) {
      msg[[i_msg]] <- "link_knowledge attribute dimension problem."
      i_msg <- i_msg + 1
    } else {
      if ( any( migr$link_knowledge[lower.tri(migr$link_knowledge, diag = TRUE)] ) ) {
        msg[[i_msg]] <- "link_knowledge attribute not consistent with site_name order declaration."
        i_msg <- i_msg + 1
      } else {
        if (!is.logical(migr$link_knowledge)) {
          msg[[i_msg]] <- "link_knowledge attribute not logical."
          i_msg <- i_msg + 1
        }
      }
    }
  }

  if (!is.element("flight_duration", names(migr))) {
    msg[[i_msg]] <- "No flight_duration attribute."
    i_msg <- i_msg + 1
  } else {
    if (any(c(site_nb, site_nb) != dim(migr$flight_duration))) {
      msg[[i_msg]] <- "flight_duration attribute dimension problem."
      i_msg <- i_msg + 1
    } else {
      if ( any(!is.numeric(migr$flight_duration))  |
           any(floor(migr$flight_duration)!=migr$flight_duration ) ) {
          msg[[i_msg]] <- "flight_duration attribute non numeric."
          i_msg <- i_msg + 1
        }
    }
  }

  if (!is.element("initial_state", names(migr))) {
    msg[[i_msg]] <- "No initial_state attribute."
    i_msg <- i_msg + 1
  } else {
    if (site_nb != length(migr$initial_state)) {
      msg[[i_msg]] <- "initial_state attribute length problem."
      i_msg <- i_msg + 1
    } else {
      if ( any(!is.numeric(migr$initial_state)) |
           any(floor(migr$flight_duration)!=migr$flight_duration) ) {
        msg[[i_msg]] <- "initial_state attribute non integer."
        i_msg <- i_msg + 1
      } else {
        if (any(migr$initial_state < 0)) {
          msg[[i_msg]] <- "initial_state attribute contains at least a negative value."
          i_msg <- i_msg + 1
        }
      }
    }
  }


  if (!is.element("horizon", names(migr))) {
    msg[[i_msg]] <- "No horizon (migration duration) attribute."
    i_msg <- i_msg + 1
  } else {
    if (1 != length(migr$horizon)) {
      msg[[i_msg]] <- "horizon attribute has more than one value."
      i_msg <- i_msg + 1
    } else {
      if ( !all(is.numeric(migr$horizon)) |
           any(floor(migr$horizon) != migr$horizon) ) {
        msg[[i_msg]] <- "horizon attribute non integer."
        i_msg <- i_msg + 1
      } else {
        if ( migr$horizon < 3 )  {
          msg[[i_msg]] <- "horizon attribute lower than 3]."
          i_msg <- i_msg + 1
        }
      }
    }
  }

  if (!is.element("death_probability", names(migr))) {
    msg[[i_msg]] <- "No death_probability attribute."
    i_msg <- i_msg + 1
  } else {
    if (site_nb != length(migr$death_probability)) {
      msg[[i_msg]] <- "death_probability attribute length problem."
      i_msg <- i_msg + 1
    } else {
      if (any(!is.numeric(migr$death_probability))) {
        msg[[i_msg]] <- "death_probability attribute non numeric."
        i_msg <- i_msg + 1
      } else {
        if ( any(migr$death_probability < 0) | any(migr$death_probability > 1) ) {
          msg[[i_msg]] <- "death_probability attribute contains at least a value not in [0, 1]."
          i_msg <- i_msg + 1
        }  else {
          if (migr$death_probability[site_nb] != 1){
            msg[[i_msg]] <- "death_probability of arrival site must be 1"
            i_msg <- i_msg + 1
          }
        }
      }
    }
  }


  if (!is.element("transition_law_type", names(migr))) {
    msg[[i_msg]] <- "No transition_law_type attribute."
    i_msg <- i_msg + 1
  } else {
    if ( migr$transition_law_type != "multinomial" ) {
      msg[[i_msg]] <- "Type of law not managed in transition_law_type attribute."
      i_msg <- i_msg + 1
    }
  }
  if (!is.element("transition_law_param", names(migr))) {
    msg[[i_msg]] <- "No transition_law_param attribute."
    i_msg <- i_msg + 1
  } else {
    if ( any( c(site_nb, site_nb) != dim(migr$transition_law_param ) ) ) {
      msg[[i_msg]] <- "transition_law_param attribute dimension problem."
      i_msg <- i_msg + 1
    } else {
      if (!all(is.numeric(migr$transition_law_param))) {
        msg[[i_msg]] <- "transition_law_param attribute non numeric."
        i_msg <- i_msg + 1
      } else {
        if ( any(migr$transition_law_param < 0) | any(migr$transition_law_param > 1) ) {
          msg[[i_msg]] <- "transition_law_param attribute contains at least a value not in [0 1]."
          i_msg <- i_msg + 1
        } else {
          if (any(abs(rowSums(migr$transition_law_param)+migr$death_probability - 1) > 0.0001)) {
            msg[[i_msg]] <- "transition_law_param attribute not a transition matrix (taking into account death_probability attribute)."
            i_msg <- i_msg + 1
          }
        }
      }
    }
  }


  if (!is.element("sojourn_law_type", names(migr))) {
    msg[[i_msg]] <- "No sojourn_law_type attribute."
    i_msg <- i_msg + 1
  } else {
    if ( migr$sojourn_law_type != "Poisson" ) {
      msg[[i_msg]] <- "Type of law not managed in sojourn_law_type attribute."
      i_msg <- i_msg + 1
    }
  }
  if (!is.element("sojourn_law_param", names(migr))) {
    msg[[i_msg]] <- "No sojourn_law_param attribute."
    i_msg <- i_msg + 1
  } else {
    if (length(migr$sojourn_law_param)!= site_nb) {
      msg[[i_msg]] <- "sojourn_law_param attribute length problem."
      i_msg <- i_msg + 1
    } else {
      p <- migr$sojourn_law_param
      if (!all(is.numeric(p)) | !all(p >= 0)) {
        msg[[i_msg]] <- "sojourn_law_param attribute not in [0, 1]."
        i_msg <- i_msg + 1
      }
    }
  }

  if (!is.element("observation_law_type", names(migr))) {
    msg[[i_msg]] <- "No observation_law_type attribute."
    i_msg <- i_msg + 1
  } else {
    if ( migr$observation_law_type != "Poisson") {
      msg[[i_msg]] <- "Type of law not managed in sojourn_law_type attribute."
      i_msg <- i_msg + 1
    }
  }
  if (!is.element("observation_law_param", names(migr))) {
    msg[[i_msg]] <- "No observation_law_param attribute."
    i_msg <- i_msg + 1
  } else {
    if ( (migr$observation_law_type == "Poisson") &
         !is.null(migr$observation_law_param) ) {
      msg[[i_msg]] <- "observation_law_param attribute not consistent with a Poisson law."
      i_msg <- i_msg + 1
    }
  }

  if (!is.element("observation", names(migr))) {
    msg[[i_msg]] <- "No observation attribute."
    i_msg <- i_msg + 1
  } else {
    bird_position_nb <- site_nb + 2 # Add position dead and flying
    time_step_nb <- migr$horizon + 1 # Add time step t0
    if ( !all( c(bird_position_nb, time_step_nb) == dim(migr$observation) ) ) {
      msg[[i_msg]] <- "observation attribute dimension problem, must be (#site+2) x (horizon+1)."
      i_msg <- i_msg + 1
    } else {
      x <- migr$observation
      x[ is.na (x)]=0
      if (!is.numeric(x)) {
        msg[[i_msg]] <- "observation attribute non numeric or NA."
        i_msg <- i_msg + 1
      } else {
        if ( !(all(x >= 0) & all(floor(x)==x)) ) {
          msg[[i_msg]] <- "observation attribute contains at least a negative or a non integer count."
          i_msg <- i_msg + 1
        }
      }
    }
  }


  if ( is.null(migr$transition_law_param) |
       is.null(migr$sojourn_law_param) |
       (is.null(migr$observation_law_param) & migr$observation_law_type != "Poisson") ) {
    ## Not all model parameters defined
    if ( is.null(migr$observation) ) {
      description <- "Incomplete object with no observation to estimate model parameters."
    } else {
      description <- "Not all model parameters defined."
    }
  } else {
    ## All model parameters defined
    if ( is.element("estimation_algorithm", names(migr)) ) {
      # Estimated model parameters
      if ( is.null(migr$observation) ) {
        description <- "Incoherent object with estimated approach and no observation."
      } else {
        description <- "Model parameters estimated given observations."
      }
    } else {
      # Provided model parameters (not computed by an estimated approach)
      if ( is.null(migr$observation) ) {
        description <- "Model parameters specified without estimated approach and observation."
      } else {
        description <- "Model parameters specified without estimated approach."
      }
    }
  }
  #print(description)

  msg
}
