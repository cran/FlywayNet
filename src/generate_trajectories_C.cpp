#include "generate_trajectories_C.h"


//' Internal C function (do not use). 
//' 
//' Simulates trajectories of individual birds.
//' 
//' @param migration A migration (migration structure).
//' @param trajectory Set of trajectories partially simulated (trajectory structure).
//'                   Default simulated until time step 1 (initial state of each bird).
//' @param end_time Time of the end of simulation. Default is the horizon (last time step).
//' 
//' @return the simulated trajectories
//' 
//' @export
// [[Rcpp::export]]

Rcpp::NumericMatrix generate_trajectories_C(
    Rcpp::List                          migration, 
    Rcpp::Nullable<Rcpp::NumericMatrix> trajectory = R_NilValue,
    Rcpp::Nullable<int>                 end_time = R_NilValue) 
{
  Rcpp::NumericMatrix internal_trajectory;
  int internal_end_time;
  if (trajectory.isNull()) {
    internal_trajectory = init_trajectories(migration);
  } else {
    internal_trajectory = 
      Rcpp::clone(Rcpp::as<Rcpp::NumericMatrix>(trajectory));
  }
  if (end_time.isNull()) {
    internal_end_time = Rcpp::as<int>(migration["horizon"]);
  } else {
    internal_end_time = Rcpp::as<int>(end_time);
  }
  simulate_trajectories(internal_trajectory, migration, internal_end_time);
  return internal_trajectory;
}





Rcpp::NumericMatrix init_trajectories(const Rcpp::List& migration)
{
  const Rcpp::StringVector site_name = migration["site_name"];
  const Rcpp::NumericVector initial_state = migration["initial_state"];//
  int site_nb = site_name.length();//
  int bird_nb = sum(initial_state);//
  int horizon_col = Rcpp::as<int>(migration["horizon"]) + 1 ;//
  
  // Initialize returned trajectory
  Rcpp::NumericMatrix trajs(bird_nb, horizon_col);
  std::fill(trajs.begin(), trajs.end(), Rcpp::NumericMatrix::get_na()) ;
  int bird_ind = 0;
  for (int i=0; i<site_nb; i++) {
    int init_i = initial_state[i];
    for (int j=0;j<init_i;j++) {
      trajs(bird_ind+j, 0) = i+1;
    }
    bird_ind = bird_ind + init_i;
  }
  return trajs;
}

/**
* see .h file
*/
void simulate_trajectories(Rcpp::NumericMatrix& trajs, 
                           const Rcpp::List& migration, int end_time)
{
  const Rcpp::StringVector site_name = migration["site_name"];
  const Rcpp::NumericVector initial_state = migration["initial_state"];
  const Rcpp::NumericMatrix flight_duration_orig = migration["flight_duration"];
  const Rcpp::NumericMatrix trans_law_param = migration["transition_law_param"];
  const Rcpp::NumericVector soj_law_param = migration["sojourn_law_param"];
  const Rcpp::NumericVector death_probability = migration["death_probability"];
  
  int site_nb = site_name.length();
  int horizon_col = Rcpp::as<int>(migration["horizon"]) + 1 ;
  // extend flight duration with the death site
  Rcpp::NumericMatrix flight_duration(site_nb, site_nb+1);
  for (int i =0; i<site_nb; i++){
    for (int j =0; j<site_nb; j++){
      flight_duration(i,  j) = flight_duration_orig(i, j);
    }
  }
  // one virtual death site is added as a possible destination
  Rcpp::NumericMatrix destination_probability(site_nb, site_nb+1);
  for (int i =0; i<site_nb; i++){
    for (int j =0; j<site_nb; j++){
      destination_probability(i,  j) = trans_law_param(i, j);
    }
    destination_probability(i,  site_nb) = death_probability(i);
  }
  
  //end_col and dest_sites
  int end_col = end_time+1;
  //Rcpp::IntegerVector dest_sites = Rcpp::seq(1, site_nb+1);
  //simulate every bird
  //Rcpp::NumericVector dbgsoj(trajs.rows());
  
  for (int i=0; i<trajs.rows(); i++){
    Rcpp::NumericMatrix::Row trajrow = trajs.row(i);
    for (int t = 0; t < end_col ; t++) {
      int curr_time= t;
      int curr_state = trajrow[t];
      if (curr_state == Rcpp::IntegerMatrix::get_na()){
        curr_state = trajrow[t-1];
        int soj = R::rpois(soj_law_param[curr_state-1])+1;
        // if (curr_state == 1) {
        //   dbgsoj[i] = soj;
        // }
        // if (curr_state == 1 and i < 5) {
        //   Rcpp::Rcout << " dbg tirage i: " << i << " curr_state=1 soj=" << soj-1 << "\n" ;
        // }
        int next = c_sample_site(destination_probability.row(curr_state-1));
        
        int flight = flight_duration(curr_state-1, next-1);
        for (; t < std::min(curr_time+soj-1, horizon_col) ;t++){//fill sojourn
          trajrow[t] = curr_state;
        }
        curr_time = t;
        if (next == -1) { 
          for (; t <  horizon_col ;t++){//fill dead
            trajrow[t] = -1;
          }
        } else {
          for (; t < std::min(curr_time+flight, horizon_col) ;t++){//fill flight
            trajrow[t] = 0;
          }
          if (t<horizon_col){//fill next
            trajrow[t] = next;
          }
        }
      }
    }
    
    // if (i < 5) {
    //   Rcpp::NumericVector trajrowbis(trajrow); 
    //   Rcpp::Rcout << " dbg trajrow i=" << i << " :" << trajrowbis << "\n" ;
    // }
  }
  // Rcpp::Rcout << " dbg dbgsoj mean=" << Rcpp::mean(dbgsoj) << " max=" << Rcpp::max(dbgsoj) << "\n" ;
}

// -----------------
int c_sample_site(Rcpp::NumericMatrix::Row probs) 
{
  double pp = R::runif(0,1);
  Rcpp::NumericMatrix::Row::iterator itb=probs.begin();
  Rcpp::NumericMatrix::Row::iterator ite=probs.end();
  int nb_dest = probs.size();
  int i = 1;
  for (; itb!=ite;itb++){
    pp -= *itb;
    if (pp <0) {
      if (i < nb_dest) {
        return i;
      } else {
        return -1;
      } 
    }
    i++;
  }
  return -1;
}

