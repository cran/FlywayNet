% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_counts.R
\name{get_counts}
\alias{get_counts}
\title{Count birds on each site at each time step from given set of trajectories.}
\usage{
get_counts(migr, traj)
}
\arguments{
\item{migr}{A migration structure (attributes horizon, initial_state are used).}

\item{traj}{A trajectory matrix N x (horizon+1) with a trajectory for each bird.
For each bird, at each time step (from 0 to horizon), state of the bird is specified:
site number, 0 for flying, -1 for dead.}
}
\value{
Counts of birds on sites, matrix (S+2) x (horizon+1).
 At each time step (from 1 to horizon), count of birds in each site.
 The first column (time step 0) and the two last rows (state flying and dead)
 are set to NA.
}
\description{
Count birds on each site at each time step from given set of trajectories. 
              The number of flying and dead birds are also computed.
}
\examples{
migr <- generate_toy_migration()
traj <- generate_trajectories( migr )
get_counts( migr, traj )
}
