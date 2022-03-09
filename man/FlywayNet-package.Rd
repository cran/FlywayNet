\name{FlywayNet-package}
\alias{FlywayNet-package}
\docType{package}

\title{\pkg{FlywayNet} package: infer the structure of migratory bird flyway networks given counts}

\description{
  The \code{R} package \pkg{FlywayNet} implements the inference of 
  the structure of migratory bird flyway networks given counts.

  It relies on a HSMM discrete time model.
  
  Underlying hypothesis:
  \itemize{
     \item sites are ordered, birds can only fly forward, no loop possible,
     \item initial state (ie number of birds at each site) at time t0 known,
     \item death probabilities at each site known,
     \item a multinomial law for site transition probabilities,
     \item a Poisson law for site sojourn time distribution,
     \item a Poisson law for observed counts.
   }

  Parameters to infer:
  \itemize{
     \item sites transition probabilities,
     \item sites sojourn time law parameters.
  }

Given S site.
For sites transition probabilities, due to the order of sites, the parameters to infer are in a upper triangular matrix. As the death probability is known, the number of
free parameters to infer is ½ S . (S+1). When site links are known to be impossible, the number of transition parameters to infer is reduced. 
For mean sites sojourn time, there is 1 parameter per site to infer.

  Two approaches: ABC and MCEM are available to infer parameters.

  The package is organized around a central class (structure) called migration.
  This structure may be use to infer parameters, to simulate trajectories
  and even to generate observations (counts) depending on the information store in the structure.
}

\section{migration structure}{
  Attributes of the migration class representing a migration network fo S sites:\itemize{
\item \emph{site_name}: list of ordered (a site can only be related to a successor site in the list) sites names [list of S caracter set]. 
\item \emph{link_knowledge}: known possible relation between sites [matrix S x S of logical]. 
        TRUE when link is possible, FALSE if not.
        Beware, this attribute must be consistent with the considering of site_name attribut. 
\item \emph{flight_duration}: times to fly between two linked sites [matrix S x S, integer], -1 stands for unknown.
\item \emph{initial_state}: initial state (number of birds per site at t=0) [vector 1 x S, integer].
\item \emph{horizon}: migration duration [integer]. t = 0, 1,…, horizon.
\item \emph{death_probability}: death probability at each site [vector 1 x S of double].

\item \emph{transition_law_type}: name of site transition law  [caracter set, “multinomial”].
\item \emph{transition_law_param}: parameters of site transition law [NULL or matrix S x S of double].
        Beware, row sums may not sum to 1 because of death probability on each site.
\item \emph{sojourn_law_type}: name of site sojourn time law [caracter set, “Poisson”]
\item \emph{sojourn_law_param}: parameters of site sojourn time distribution law [NULL or vector 1 x S of double].
\item \emph{observation_law_type}:  name of site observation probability law, [caracter set, “Poisson”]
\item \emph{observation_law_param}: parameters  [NULL or vector 1 x S of double].

\item \emph{observation}: observed counts  [NULL or matrix (S+2) x (horizon+1), integers in \{1,…, S, NA\}. 
          The first column is for time step zero. The two last rows are filled with NA.

\item \emph{estimation_method}:\itemize{
	           \item \emph{name}: name [caracter set in {”ABC”, ”MCEM”} ], 
             \item \emph{settings}: information provided to the method [list of pair (parameter, value) ], 
             \item \emph{output}: \itemize{
	                       \item \emph{transition_law_param}: estimated probabilities of transition,
	                       \item \emph{sojour_law_param}: estimated sojourn time,
             	           \item additional output (trace…) [free format related to each method].} } }


Required attributes of a migration structure for inference: \emph{site_name}, \emph{link_knowledge},
\emph{flight_duration}, \emph{initial_state}, \emph{horizon}, \emph{death_probability}, 
\emph{transition_law_type}, \emph{sojourn_law_type}.

For simulation, two more attributes are required: \emph{transition_law_param}, \emph{sojourn_law_param}.
}


\section{Observed counts format}{             
Counts of birds are expressed as a matrix (S+2) x (horizon+1) of integer with S being the number of sites. 
Then a row i (i in 1,...,S) of the matrix is the observed counts of bird at each time step from 0 to \emph{horizon}.
Birds are not observed at time step 0, so the first column is filled of NA. 
The two last rows are filled with NA.\cr
This format is adapted also for counts of birds from a set of bird trajectories. In this case, the two last rows are also computed: states flying and dead.
}


\section{Trajectories format}{             
Trajectories of birds are expressed as a matrix N x (horizon+1) of integer with N being the number of birds in initial state (time step 0).
Then a row i of the matrix is the trajectory of the bird i at each time step indicating the state of the bird. Possible states of birds are a site number (1,…,S), flying (0), or death (-1).  
}


\section{Package functions}{

\emph{Model creation} \cr
\bold{new_migration}                Generate a specific migration structure usable for inference or simulation 
                                    depending of given attributs. \cr
\bold{generate_toy_migration}       Generate a toy problem, usable for inference or simulation. \cr
\bold{generate_random_migration}    Generate a random migration structure, usable for inference or simulation. \cr
\bold{generate_modified_migration}  Generate a modified migration structure, usable for inference or simulation. \cr

\emph{Parameters estimation} \cr
\bold{estimate_migration_ABC}   Infer parameters using ABC (Approximate Bayesian Computation) approach. \cr
\bold{estimate_migration_MCEM}  Infer parameters using MCEM (Monte Carlo Expectation Maximization) approach. \cr
\bold{reestimate_migration_from_MCEMruns}  Infer parameters using logs of several runs of MCEM. \cr
\bold{estimate_migration_from_trajectories}  Infer parameters just using a set of trajectories.\cr\cr

\emph{Simulation} \cr
\bold{generate_trajectories}      Generate a trajectory for each bird. \cr
\bold{generate_observedcounts}  Generate a set of observed counts.\cr\cr

\emph{Utilities} \cr
\bold{validate_migration}       Check the validity of a migration structure.\cr
\bold{read_migration}           Read a file describing a migration. \cr
\bold{write_migration}          Write a file describing a migration. \cr
\bold{plot}                     Plot the migration graph of a given migration. \cr
\bold{plot_observedcounts}      Plot a set of observed counts as a flow. \cr
\bold{plot_trajectories}        Plot a set of trajectories as a flow. \cr
\bold{get_counts}               Compute the count of birds in each state (site number, flying, dead) at each time step, 
                                given a set of birds trajectories. \cr
\bold{set_freeparametersvalue}  Assign free parameter values to a migration structure. \cr
\bold{get_freeparametersvalue}  Extract free parameter values from a migration structure. \cr
\bold{get_paramlikelihood}      Computes, by simulation, the log-likelihood of parameters. \cr
\bold{get_observation_probability}  Computes the probability of observations given the birds count from trajectories. \cr\cr


}



