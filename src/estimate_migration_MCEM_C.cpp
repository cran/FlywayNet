#include <Rcpp.h>

#include "birdnet_mcem_utils.h"
#include "birdnet_mcem_sis.h"
#include "birdnet_mcem_mh.h"
#include "generate_trajectories_C.h"
#include "get_counts_C.h"
#include "get_observation_probability_C.h"


//' Internal C function (do not use). 
//' 
//' Estimate parameters with MCEM algorithm
//' 
//' @param migration A migration (migration structure).
//' @param estimate_transitions Boolean for requiring transition parameters estimation.
//' @param estimate_sojourns Boolean for requiring sojourn parameters estimation.
//' @param sojourn_domain Sojourn parameters domains.
//' @param start_transitions Start transition.
//' @param start_sojourns Start sojourns.
//' @param nb_particles Number of particles.
//' @param itermax Maximum number of iteration.
//' @param MC_algo Monte Carlo algorithm name.
//' @param MH_neighborhood Neighborhood, only when MC_algo is "MH".
//' @param MH_transition_length Transition length, only when MC_algo is "MH".
//' @param log_transitions Log of transition parameters.
//' @param log_sojourns Log of sojourn parameters.
//' @param log_loglikelihood log of log-likelihood.
//' @param log_sel_particles log of selected particles.
//' @param log_acceptance_rate log of acceptance rate.
//' @param verbose Boolean for verbose.
//' 
//' @return migration with estimated parameters (migration structure).
//' 
//' @export
// [[Rcpp::export]]

Rcpp::List estimate_migration_MCEM_C(Rcpp::List migration,
  Rcpp::Nullable<bool> estimate_transitions = R_NilValue, 
  Rcpp::Nullable<Rcpp::LogicalVector> estimate_sojourns = R_NilValue,
  Rcpp::Nullable<Rcpp::List> sojourn_domain = R_NilValue,
  Rcpp::Nullable<Rcpp::NumericMatrix> start_transitions = R_NilValue,
  Rcpp::Nullable<Rcpp::NumericVector> start_sojourns = R_NilValue,
  Rcpp::Nullable<int> nb_particles = R_NilValue, 
  Rcpp::Nullable<int> itermax = R_NilValue,
  Rcpp::Nullable<Rcpp::String> MC_algo = R_NilValue,
  Rcpp::Nullable<double> MH_neighborhood = R_NilValue,
  Rcpp::Nullable<int> MH_transition_length = R_NilValue,
  Rcpp::Nullable<bool> log_transitions = R_NilValue,
  Rcpp::Nullable<bool> log_sojourns = R_NilValue,
  Rcpp::Nullable<bool> log_loglikelihood = R_NilValue,
  Rcpp::Nullable<bool> log_sel_particles = R_NilValue,
  Rcpp::Nullable<bool> log_acceptance_rate = R_NilValue,
  Rcpp::Nullable<bool> verbose = R_NilValue)
  {
  bool estimate_transitions_i = true;
  Rcpp::LogicalVector estimate_sojourns_i;
  Rcpp::List sojourn_domain_i = 
    Rcpp::clone(Rcpp::as<Rcpp::List>(sojourn_domain));
  int nb_particles_i = 100;
  int itermax_i = 10;
  bool log_transitions_i = false;
  bool log_sojourns_i = false;
  bool log_loglikelihood_i = false;
  bool log_sel_particles_i = false;
  bool log_acceptance_rate_i = false;
  bool MC_SIS = true;
  double MH_neighborhood_i = 0.1;
  int MH_transition_length_i = 1;
  bool verb = Rcpp::as<bool>(verbose);

  if (not estimate_transitions.isNull()) {
    estimate_transitions_i = Rcpp::as<bool>(estimate_transitions); 
  }
  if (estimate_sojourns) {
    Rcpp::StringVector site_name = migration["site_name"];
    int site_nb = site_name.size();
    estimate_sojourns_i=Rcpp::LogicalVector(site_nb);
    estimate_sojourns_i.fill(true);
    estimate_sojourns_i[site_nb-1] = false;
  } else {
    estimate_sojourns_i=
      Rcpp::clone(Rcpp::as<Rcpp::LogicalVector>(estimate_sojourns));
  }
  if (not nb_particles.isNull()) {
    nb_particles_i = Rcpp::as<int>(nb_particles); 
  }
  if (not itermax.isNull()) {
    itermax_i = Rcpp::as<int>(itermax); 
  }
  if (not MC_algo.isNull()) {
    std::string MC_algo_str = Rcpp::as<std::string>(MC_algo);
    if (MC_algo_str == "MH") {
      MC_SIS = false;
    } else if (MC_algo_str == "SIS") {
      MC_SIS = true;
    } else {
      Rcpp::stop("MCEM: bad parameter value MC_algo");
    }
  }
  if (not MH_neighborhood.isNull()) {
   // if (MC_SIS) {
   //   Rcpp::stop("MCEM cannot use MH_neighborhood with the SIS method");
   // }
    MH_neighborhood_i = Rcpp::as<double>(MH_neighborhood); 
  }
  if (not MH_transition_length.isNull()) {
   // if (MC_SIS) {
   //   Rcpp::stop("MCEM cannot use MH_transition_length with the SIS method");
   // }
    MH_transition_length_i = Rcpp::as<int>(MH_transition_length); 
  }
  if (not log_transitions.isNull()) {
    log_transitions_i = Rcpp::as<bool>(log_transitions); 
  }
  if (not log_sojourns.isNull()) {
    log_sojourns_i = Rcpp::as<bool>(log_sojourns); 
  }
  if (not log_loglikelihood.isNull()) {
    log_loglikelihood_i = Rcpp::as<bool>(log_loglikelihood); 
  }
  if (not log_sel_particles.isNull()) {
    log_sel_particles_i = Rcpp::as<bool>(log_sel_particles); 
  //  if (not MC_SIS and log_sel_particles_i) {
  //    Rcpp::stop("MCEM cannot use log_sel_particles with the MH method");
  //  }
  }
  if (not log_acceptance_rate.isNull()) {
    log_acceptance_rate_i = Rcpp::as<bool>(log_acceptance_rate); 
  //  if (MC_SIS and log_acceptance_rate_i) {
  //    Rcpp::stop("MCEM cannot use log_acceptance_rate with the SIS method");
  //  }
  }
  //migration structure
  Rcpp::NumericVector initial_state = migration["initial_state"];
  Rcpp::NumericMatrix observations = migration["observation"];
  int horizon_time = Rcpp::as<int>(migration["horizon"]) ;
  int horizon_col = horizon_time + 1 ;
  int bird_nb = Rcpp::sum(initial_state);
  Rcpp::List curr_migration = Rcpp::clone(migration);
  if (not start_transitions.isNull()) {
    curr_migration["transition_law_param"] = start_transitions;
  }
  if (not start_sojourns.isNull()) {
    curr_migration["sojourn_law_param"] = start_sojourns;
  }
  Rcpp::List logged_transitions;
  Rcpp::List logged_sojourns;
  Rcpp::List logged_loglikelihood;
  Rcpp::List logged_sel_particles;
  Rcpp::List logged_acceptance_rate;
  if (log_transitions_i) {
    Rcpp::NumericMatrix tolog = curr_migration["transition_law_param"];
    logged_transitions.push_back(Rcpp::clone(tolog));
  }
  if (log_sojourns_i) {
    Rcpp::NumericVector tolog = curr_migration["sojourn_law_param"];
    logged_sojourns.push_back(Rcpp::clone(tolog));
  }
  if (log_acceptance_rate_i) {
    logged_acceptance_rate.push_back(0.0);
  }
  
  if (MC_SIS) {//////////////////////////////////////// ////////////  SIS resampling
    bool use_log = true;
    int iter = 1;
    bool done = false;
    Rcpp::NumericMatrix particles_1(bird_nb*nb_particles_i, horizon_col);
    Rcpp::NumericMatrix particles_2(bird_nb*nb_particles_i, horizon_col);
    Rcpp::NumericMatrix obsProbs_1(nb_particles_i, horizon_col);
    Rcpp::NumericMatrix obsProbs_2(nb_particles_i, horizon_col);
    Rcpp::IntegerVector resampling(nb_particles_i);
    while (!done) {
      if (verb) {Rcpp::Rcout << "-------------- ITER EM SIS " << iter << "\n";}
  
      init_observation_probabilities(obsProbs_1, use_log);
      init_observation_probabilities(obsProbs_2, use_log);
      init_particles(particles_1, curr_migration, nb_particles_i, 0);
      if (log_sel_particles_i) {
        Rcpp::NumericVector tolog(horizon_col, 1.0);
        logged_sel_particles.push_back(Rcpp::clone(tolog));
      }
      bool use_1 = true;
      for (int t=1; t<horizon_col; t++) {
        Rcpp::NumericMatrix particles_sim;
        Rcpp::NumericMatrix particles_resampled;
        Rcpp::NumericMatrix obsProbs_sim;
        Rcpp::NumericMatrix obsProbs_resampled;
        if (use_1) {
          particles_sim = particles_1;
          particles_resampled = particles_2;
          obsProbs_sim = obsProbs_1;
          obsProbs_resampled = obsProbs_2;
        } else {
          particles_sim = particles_2;
          particles_resampled = particles_1;
          obsProbs_sim = obsProbs_2;
          obsProbs_resampled = obsProbs_1;
        }
        // Sample particles
        simulate_trajectories(particles_sim, curr_migration, t);
        // Update observation probabilities
        compute_observation_probabilities(obsProbs_sim, use_log, curr_migration, 
                            particles_sim, t, bird_nb, nb_particles_i);
        
        // Resample trajectories
        Rcpp::IntegerVector resampling = resampling_particles(
          obsProbs_sim, use_log, t, nb_particles_i);
        
        if (log_sel_particles_i) {
          Rcpp::NumericVector tolog = 
            logged_sel_particles[logged_sel_particles.length()-1];
          Rcpp::IntegerVector unique_part = Rcpp::unique(resampling);
          tolog[t] = (double) unique_part.length() / (double) nb_particles_i;
        }
        
        apply_resampling(particles_resampled, obsProbs_resampled, resampling,
                         particles_sim, obsProbs_sim, bird_nb, t, 
                         nb_particles_i);
        use_1 = not use_1;
      }
      ////////// Update parameters
      Rcpp::NumericMatrix particles_final;
      if (use_1) {
        particles_final = particles_2;
      } else {
        particles_final = particles_1;
      }
      //TODO should provide and compute the loglikelihood of parameters on
      // particle_final
      update_parameters(curr_migration, particles_final,
                        estimate_transitions_i, estimate_sojourns_i,
                        sojourn_domain_i, 0.05);
      if (log_transitions_i) {
        Rcpp::NumericMatrix tolog = curr_migration["transition_law_param"];
        logged_transitions.push_back(Rcpp::clone(tolog));
      }
      if (log_sojourns_i) {
        Rcpp::NumericVector tolog = curr_migration["sojourn_law_param"];
        logged_sojourns.push_back(Rcpp::clone(tolog));
      }
      if (iter >= itermax_i) {
        done = true;
      }
      iter ++;
    }
  } else { ///////////////////////////////////////////////////////////////////////  MH resampling
    bool use_log = true;
    int iter = 1;
    bool done = false;
    
    while (!done) {
      
      if (verb) {Rcpp::Rcout << "-------------- ITER EM MH " << iter << "\n";}
      
      // monte carlo
      Rcpp::NumericMatrix particles_chain(bird_nb*nb_particles_i, horizon_col);
      std::fill(particles_chain.begin(), particles_chain.end(), 
                Rcpp::NumericMatrix::get_na()) ;
      
      Rcpp::NumericMatrix pi_curr = init_trajectories(curr_migration);
      simulate_trajectories(pi_curr, curr_migration, horizon_time);
      double log_PO_pi_curr = get_observation_probability_C(observations, 
                                               get_counts_C(curr_migration, pi_curr), 
                                               use_log);
      double log_likelihood = log_PO_pi_curr;
                               //compute loglikelihood of parameters based on 
                               //maxlog operator
      int acceptance_rate = 0;
      for (int p=0;  p<nb_particles_i; p++) {
        Rcpp::NumericVector draws = Rcpp::runif(MH_transition_length_i);
        for (int l=0;  l<MH_transition_length_i; l++) {
          Rcpp::NumericMatrix pi_prop = Rcpp::clone(pi_curr);
          resimulate_trajectories(pi_prop, curr_migration, horizon_time, 
                                  MH_neighborhood_i);
          double log_PO_pi_prop = get_observation_probability_C(observations, 
                                             get_counts_C(curr_migration, pi_prop), 
                                             use_log);
          if (draws[l] < std::min(1.0, 
                                  std::exp(log_PO_pi_prop - log_PO_pi_curr))) {
            acceptance_rate ++;
            pi_curr = pi_prop;
            log_PO_pi_curr = log_PO_pi_prop;
          }
        }
        for (int i=0; i<bird_nb; i++) {
          for (int t=0; t<horizon_col;t++){
            particles_chain(i+p*bird_nb, t) = pi_curr(i,t);
          }
        }
        log_likelihood = maxlog(log_likelihood, log_PO_pi_curr);
        if (p == nb_particles_i-1){
          log_likelihood = log(1.0/(double)nb_particles_i)+log_likelihood;
        }
      }
      update_parameters(curr_migration, particles_chain,
                        estimate_transitions_i, estimate_sojourns_i,
                        sojourn_domain_i, 0.05);
      if (log_transitions_i) {
        Rcpp::NumericMatrix tolog = curr_migration["transition_law_param"];
        logged_transitions.push_back(Rcpp::clone(tolog));
      }
      if (log_sojourns_i) {
        Rcpp::NumericVector tolog = curr_migration["sojourn_law_param"];
        logged_sojourns.push_back(Rcpp::clone(tolog));
      }
      if (log_loglikelihood_i) {
        logged_loglikelihood.push_back((double) log_likelihood);
      }
      if (log_acceptance_rate_i) {
        logged_acceptance_rate.push_back((double) acceptance_rate / 
          ((double) nb_particles_i * (double) MH_transition_length_i) );
      }
      if (iter >= itermax_i) {
        done = true;
      }
      iter ++;
    }
  }

  Rcpp::List estimation_method;
  estimation_method.push_back("MCEM", "name");
  Rcpp::List settings;
  settings.push_back(estimate_transitions_i, "estimate_transitions");
  settings.push_back(estimate_sojourns_i, "estimate_sojourns");
  settings.push_back(sojourn_domain_i, "sojourn_domain");
  settings.push_back(start_transitions, "start_transitions");
  settings.push_back(start_sojourns, "start_sojourns");
  settings.push_back(nb_particles_i, "nb_particles");
  settings.push_back(itermax_i, "itermax");
  settings.push_back(MC_algo, "MC_algo");
  settings.push_back(MH_neighborhood_i, "MH_neighborhood");
  settings.push_back(MH_transition_length_i, "MH_transition_length");
  settings.push_back(log_transitions_i, "log_transitions");
  settings.push_back(log_sojourns_i, "log_sojourns");
  settings.push_back(log_loglikelihood_i, "log_loglikelihood");
  settings.push_back(log_sel_particles_i, "log_sel_particles");
  settings.push_back(log_acceptance_rate_i, "log_acceptance_rate");
  estimation_method.push_back(settings, "settings");
  Rcpp::List output;
  output.push_back(curr_migration["transition_law_param"], "transition_law_param");
  output.push_back(curr_migration["sojourn_law_param"], "sojourn_law_param");
  if (log_transitions_i) {
    output.push_back(logged_transitions, "log_transitions");
  }
  if (log_sojourns_i) {
    output.push_back(logged_sojourns, "log_sojourns");
  }
  if (log_loglikelihood_i) {
    output.push_back(logged_loglikelihood, "log_loglikelihood");
  }
  if (log_sel_particles_i) {
    output.push_back(logged_sel_particles, "log_sel_particles");
  }
  if (log_acceptance_rate_i) {
    output.push_back(logged_acceptance_rate, "log_acceptance_rate");
  }  
  estimation_method.push_back(output, "output");
  migration.push_back(estimation_method, "estimation_method");
  
  return migration;
}



