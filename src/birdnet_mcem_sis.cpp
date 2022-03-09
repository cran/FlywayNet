#include "generate_trajectories_C.h"
#include "get_counts_C.h"
#include "get_observation_probability_C.h"
#include "birdnet_mcem_utils.h"
#include "birdnet_mcem_sis.h"


/**
 * Compute obs. probabilities of particles until time t ie:
 * something proportionnal to P(O | Pi_[0:t]^m). A particle is a set of birds 
 * trajectories. It's actually an in-place incremental version of 
 * the observation probability. 
 * 
 * @param [IN/OUT] obsProbs, Previous particles probabilities. It must be 
 *   computed until time t-1. It's a matrix of size nb_particles*(horizon+1)
 * @param [IN] use_log, if true, probabilitis are computed using log
 * @param [IN] migration, the migration structure.
 * @param [IN] particles, simulated particles.
 * @param [IN] t, current time
 * @param [IN] nb_birds, number of birds in a particle.
 * @param [IN] nb_particles, the number of particles
 */
void compute_observation_probabilities(Rcpp::NumericMatrix obsProbs, 
                         bool use_log,
                         Rcpp::List migration,
                         Rcpp::NumericMatrix particles, 
                         int t, int nb_birds, int nb_particles)
{
  Rcpp::NumericMatrix obs = migration["observation"];
  Rcpp::NumericVector obs_t = obs.column(t);
  //Rcpp::Rcout << " dbg comput_weight obs_t=" << obs_t << "\n";
  for (int m=0; m<nb_particles; m++) {
    Rcpp::NumericMatrix part_i = particles(
      Rcpp::Range(m*nb_birds,(m+1)*nb_birds-1), Rcpp::Range(t,t));
    
    Rcpp::NumericVector hidden_count_t = 
      get_counts_t(migration, part_i.column(0));
    if (use_log) {
      obsProbs(m,t) = obsProbs(m, t-1)+
        get_observation_probability_t(obs_t, hidden_count_t,true);
    } else {
      obsProbs(m,t) = obsProbs(m, t-1) * 
        get_observation_probability_t(obs_t, hidden_count_t,false);
    }
  }
}


/**
 * Initializes the observation proabilities structure
 *
 * @param [IN/OUT] obsProbs, It's a matrix of size nb_particles*(horizon+1)
 * @param [IN] use_log, should be the log computed
 *
 */
void init_observation_probabilities(Rcpp::NumericMatrix obsProbs, bool use_log)
{
  std::fill(obsProbs.begin(), obsProbs.end(), 
            Rcpp::NumericMatrix::get_na()) ;
  for (int p=0; p<obsProbs.rows(); p++){
    if (use_log) {
      obsProbs(p,0) = 0;
    } else {
      obsProbs(p,0) = 1;
    }
  }
}



/***
 * Initialize the particles structure. A particle is a set of birds 
 * trajectories. The birds are allocated for each particle in their initial 
 * state.
 * 
 * @param [IN/OUT] particles, Empty set of particles. At first time_step,
 *   birds are allocated to site according the number of birds per site in
 *   the initial_state specified in the migration structure.
 *   It's a matrix of size : (nb_birds*nb_particles) * (horizon+1)
 * @param [IN] migration, the migration structure.
 * @param [IN] nb_particles, the number of particles in the SIS resampling 
 *   algorithm.
 * @param [IN] t, time until which the particles must be filled (usually 0, 
 *   after that the simulation is required)
 */
void init_particles(Rcpp::NumericMatrix particles,
                    const Rcpp::List migration, 
                    int nb_particles,
                    int t)
{
  std::fill(particles.begin(), particles.end(), Rcpp::NumericMatrix::get_na()) ;
  if (t == -1) return;
  
  const Rcpp::StringVector site_name = migration["site_name"];
  const Rcpp::NumericVector initial_state = migration["initial_state"];//
  int site_nb = site_name.length();//
  int bird_nb = sum(initial_state);//
  int bird_ind = 0;
  for (int s=0; s<site_nb; s++) {
    int init_s = initial_state[s];
    for (int sb=0;sb<init_s;sb++) {
      for (int p=0; p<nb_particles; p++) {
        particles(bird_ind+p*bird_nb, 0) = s+1;
      }
      bird_ind ++;  
    }
  }
}


/***
 * Resampling particles, resamples particles at current time. 
 * Used only in MCEM SIS.
 * 
 * @param [IN/OUT] resampling, vector of size nb_particles. It is the 
 *  resampling of the particles according the observation probabilities.
 * @param [IN] obsProbs, resampled particles that must be simulated 
 *  until horizon time.
 * @param [IN] use_log, if true, observation probabilities are computed using log
 * @param [IN] t, current time
 * @param [IN] nb_particles, number of particles
 * @return a resampled vector of particles identifiers.
 */
Rcpp::IntegerVector resampling_particles(
    Rcpp::NumericMatrix obsProbs,
    bool log, int t, int nb_particles)
{
  Rcpp::NumericVector obsProbs_t = obsProbs.column(t);
  Rcpp::NumericVector probs = Rcpp::clone(obsProbs_t);
  
  if (log){
    //normalise log_obs_probs to get a multinomial distribution on particles
    maxlog_softmax(probs);
  } else {
    //normalise obs. probabilities to get a multinomial distribution on particles
    double sumObsProbs =  Rcpp::sum(probs);
    if (sumObsProbs == 0) {
      Rcpp::stop("erreur sumObsProbs = 0\n");
    } 
    for (int p =0; p<probs.size(); p++) {
      probs[p] = probs[p]/sumObsProbs;
    }
  }
  //sample particles
  Rcpp::IntegerVector particles_id = Rcpp::seq(0, nb_particles-1);
  Rcpp::IntegerVector res = Rcpp::sample(particles_id, nb_particles,  
                                         true, probs);
  return res;
}



/***
 * Apply the resampling of particles. Used only in MCEM SIS.
 * 
 * @param [IN/OUT] particles_new, particles resampled according the resampling
 * @param [IN/OUT] obsProbs_new, particles obs. probabilities resampled
 * @param [IN] resampling, a resampled vector of particles identifiers.
 * @param [IN] particles, particles before the resampling
 * @param [IN] obsProbs, particles obs. probabilities before the resampling
 * @param [IN] nb_birds, the number of birds
 * @param [IN] t, current time
 * @param [IN] nb_particles, the number of particles
 */
void apply_resampling(
    Rcpp::NumericMatrix particles_new,
    Rcpp::NumericMatrix obsProbs_new,
    Rcpp::IntegerVector resampling,
    Rcpp::NumericMatrix particles,
    Rcpp::NumericMatrix obsProbs,
    int nb_birds, int t, int nb_particles)
{
  //resample particles and obsProbs
  for (int p =0; p<nb_particles; p++){
    int pnew = resampling[p];
    for (int ti =0; ti<particles.cols(); ti++){
      for (int b =0; b<nb_birds; b++){
        particles_new(p*nb_birds+b, ti) = particles(pnew*nb_birds+b, ti) ;
      }
    }
    obsProbs_new(p, t) = obsProbs(pnew, t) ;
  }
}




