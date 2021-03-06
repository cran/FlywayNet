% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_freparametersvalue.R
\name{get_freeparametersvalue}
\alias{get_freeparametersvalue}
\title{Extract free parameter values from a migration structure.}
\usage{
get_freeparametersvalue(
  migr,
  update_transitions = TRUE,
  update_sojourns = NULL,
  use_adhoc_dirichlet = FALSE
)
}
\arguments{
\item{migr}{A migration structure.}

\item{update_transitions}{If TRUE, transitions are considered 
unknown and their values should be returned.}

\item{update_sojourns}{Vector of boolean, that identifies which sojourn times
should be returned.}

\item{use_adhoc_dirichlet}{If TRUE, the transition probabilities are 
given in the ad hoc Dirichlet distribution (\eqn{q_ij} values). See details.}
}
\value{
return named vector of default values of parameters to estimate.
}
\description{
Get values of free parameters. Indeed taking into account sites order,
known impossible sites links, sites dead probability, not all the sites transition probabilities
have to be estimated. Others parameters values may be evaluated from free parameters.
}
\details{
Let N be number of sites, \eqn{d_i} the death probability at a given site i, 
\eqn{p_ij} the transition probability from site i to site j.
Then we must have a multinomial probability distribution that respects :
\eqn{p_i1 + ... + p_iN + di = 1}. Drawing \eqn{p_i1}, ..., \eqn{p_iN} 
so that this set of values forms a multinomial distribution can be done with 
the multivariate Dirichlet distribution which is difficult to handle in 
estimation processes (especially ABC). The adhoc dirichlet proposed here 
is a way to provide a bijection between a multinomial distribution and a 
a set of uniform distribution in [0:1]. This way, from N values 
\eqn{q_ij}  drawn uniformely in [0:1], we can compute 
\eqn{p_ij} with the constraint of the multinomial 
distribution. And inversely, if we have the  multinomial distribution then 
we can compute uniquely the   \eqn{q_ij} values. A way to do it is to define 
recursively the \eqn{p_ij} values as 
\deqn{p_ij = q_ij * (1- d_i - p_i1 - ... - p_i(j-1))}

Also, the \eqn{d_i} values are expected to be known and the last 
probability transition from one site i (eg. \eqn{p_iN}) can be computed 
from others  \eqn{p_ij} and \eqn{d_i}. Finally, the 'link_knowledge' 
identifies the transitions that are not possible (with probability 0)
which are also removed from the extraction.
}
\examples{
 migr <- generate_toy_migration()
 get_freeparametersvalue(migr) 
}
