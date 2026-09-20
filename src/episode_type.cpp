#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector classify_episode_types_cpp(CharacterVector patient_id,
                                           NumericVector infection_date,
                                           NumericVector last_dose_antibiotic,
                                           CharacterVector organism_key,
                                           LogicalVector valid) {
  int n = patient_id.size();

  // all inputs must be the same length, otherwise the loop reads out of bounds
  if (infection_date.size() != n || last_dose_antibiotic.size() != n ||
      organism_key.size() != n || valid.size() != n) {
    stop("All inputs must have the same length.");
  }

  CharacterVector out(n);

  std::string prior_patient = "";
  double prior_dose = NA_REAL;
  std::string prior_organism_key = "";
  bool have_prior_episode = false;

  for (int i=0; i<n; ++i) {
    // check if current patient is different from the previous row; first PD infection does not have a prior episode
    std::string this_patient = as<std::string>(patient_id[i]);
    if (this_patient != prior_patient) {
      have_prior_episode = false;
    }

    // skip when validation failed (an NA validity flag counts as failed)
    if (LogicalVector::is_na(valid[i]) || !valid[i]) {
      out[i] = NA_STRING;
      continue;
    }

    if (!have_prior_episode) {
      out[i] = NA_STRING;
    } else if (NumericVector::is_na(prior_dose)) {
      out[i] = NA_STRING;
    } else {
      double days_since = infection_date[i] - prior_dose;
      bool within_4_weeks = days_since <= 28.0;   // checks if last dose date (of a prior episode) is within 28 days to this new infection date

      // checks to determine episode category
      std::string current_organism_key = as<std::string>(organism_key[i]);
      bool same_organism = (current_organism_key == prior_organism_key);
      bool current_negative = (current_organism_key == "negative");
      bool prior_negative = (prior_organism_key == "negative");
      bool is_relapse_pair = same_organism || current_negative || prior_negative;

      if (within_4_weeks && is_relapse_pair) {
        out[i] = "relapsing";
      } else if (within_4_weeks && !is_relapse_pair) {
        out[i] = "recurrent";
      } else if (!within_4_weeks && same_organism) {
        out[i] = "repeat";
      } else {
        out[i] = NA_STRING;
      }
    }

    prior_patient = this_patient;
    prior_dose = last_dose_antibiotic[i];
    prior_organism_key = as<std::string>(organism_key[i]);
    have_prior_episode = true;
  }

  return out;

}
