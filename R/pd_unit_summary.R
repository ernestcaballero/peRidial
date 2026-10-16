
# HELPER FUNCTIONS FOR SUMMARY METHOD



#' Does `df` have at least one row and a column called `col`?
#' @noRd
has_col <- function(df, col) nrow(df) > 0 && col %in% names(df)



#' Peritonitis rate against ISPD benchmark
#' @noRd
summarise_rate <- function(x) {
  rate_num <- if (has_col(x$infections, "counts_toward_rate")) {
    sum(x$infections$counts_toward_rate, na.rm = TRUE)
  } else {
    0
  }
  rate_den <- x$tpyar
  rate <- if (is.na(rate_den) || rate_den == 0) NA_real_ else rate_num / rate_den
  # unit-level threshold; fall back to the ISPD standard for objects without the field
  rate_benchmark <- if (is.null(x$rate_benchmark)) 0.40 else x$rate_benchmark
  list(rate = rate, rate_num = rate_num, rate_den = rate_den,
       rate_benchmark = rate_benchmark,
       rate_met = if (is.na(rate)) NA else rate <= rate_benchmark)  # rate_met is NA when rate itself is NA for an empty unit
}



#' Peritonitis-free percentage against ISPD benchmark
#' @noRd
summarise_pf <- function(x) {
  pf_num <- if (has_col(x$patients, "n_episodes")) {
    sum(x$patients$n_episodes == 0, na.rm = TRUE)
  } else {
    0
  }
  pf_den <- x$n_patients
  pf <- if (is.na(pf_den) || pf_den == 0) NA_real_ else pf_num / pf_den
  # unit-level threshold; fall back to the ISPD standard for objects without the field
  pf_benchmark <- if (is.null(x$pf_benchmark)) 0.80 else x$pf_benchmark
  list(pf = pf, pf_num = pf_num, pf_den = pf_den,
       pf_benchmark = pf_benchmark,
       pf_met = if (is.na(pf)) NA else pf > pf_benchmark)  # pf_met is NA when there are no patients
}



#' Episode type counts
#' @noRd
summarise_episode_types <- function(x) {
  if (has_col(x$infections, "episode_type")) {
    types <- x$infections$episode_type
    types[is.na(types)] <- "uncategorised"  # NA episode types relabelled "uncategorised"
    table(types)
  } else {
    table(character(0))
  }
}



#' Patient counts by how their PD ended
#' @noRd
summarise_outcomes <- function(x) {
  if (has_col(x$patients, "transfer_reason")) {
    reasons <- x$patients$transfer_reason
    reasons[is.na(reasons)] <- "still active"      # NA (not yet censored) relabelled as "still active".
    table(reasons)
  } else {
    table(character(0))
  }
}


#' Incident / prevalent
#' @noRd
summarise_cohort <- function(x) {
  c(incident = x$n_new, prevalent = x$n_patients - x$n_new)
}



#' Median patient age at \code{t0}
#' @noRd
summarise_mean_age <- function(x) {
  if (!has_col(x$patients, "date_of_birth")) {
    return(NA_real_)
  }
  ages <- as.numeric(difftime(x$t0, x$patients$date_of_birth, units = "days")) / 365.25
  if (all(is.na(ages))) NA_real_ else mean(ages, na.rm = TRUE)
}




#' Patient demographics, one table per field
#' @param x A \code{pd_unit} object.
#' @noRd
summarise_demographics <- function(x) {
  demo_table <- function(col) {
    if (!has_col(x$patients, col)) {
      return(table(character(0)))
    }
    vals <- x$patients[[col]]
    vals[is.na(vals)] <- "unknown"       # NA relabelled as "unknown"
    table(vals)
  }
  list(
    gender = demo_table("gender"),
    ethnicity = demo_table("ethnicity"),
    primary_kidney_disease = demo_table("primary_kidney_disease"),
    diabetes_status = demo_table("diabetes_status"),
    smoking_status = demo_table("smoking_status"),
    dialysis_type = demo_table("dialysis_type")
  )
}



#' Calculate mean days for time to first infection and time to pd start
#' @noRd
mean_days <- function(v) {
  v <- v[!is.na(v)]
  c(mean = if (length(v) == 0) NA_real_ else mean(v), n = length(v))
}



#' Catheter summary: counts, procedure type, and time to PD start and time to first peritonitis episode
#' @noRd
summarise_catheters <- function(x) {
  n_catheters <- nrow(x$catheters)

  procedure_type <- if (has_col(x$catheters, "procedure_type")) {
    pt <- x$catheters$procedure_type
    pt[is.na(pt)] <- "unknown"
    table(pt)
  } else {
    table(character(0))
  }

  # each incident patient's first catheter: the one with the earliest PD start
  first_catheter <- NULL
  if (has_col(x$patients, "patient_id") && has_col(x$patients, "new_patient_flag") &&
      has_col(x$catheters, "patient_id") && has_col(x$catheters, "pd_start_date")) {
    incident <- x$patients$patient_id[x$patients$new_patient_flag %in% TRUE]
    cath <- x$catheters[x$catheters$patient_id %in% incident &
                          !is.na(x$catheters$pd_start_date), , drop = FALSE]
    cath <- cath[order(cath$patient_id, cath$pd_start_date), , drop = FALSE]
    first_catheter <- cath[!duplicated(cath$patient_id), , drop = FALSE]
  }

  time_to_pd_start_days <- if (!is.null(first_catheter) &&
                               has_col(first_catheter, "insertion_date")) {
    mean_days(as.numeric(first_catheter$pd_start_date - first_catheter$insertion_date))
  } else {
    mean_days(numeric(0))
  }

  # first PD start to each incident patient's first episode (patients with none drop out)
  time_to_first_peritonitis_days <- if (!is.null(first_catheter) &&
                                        has_col(x$infections, "patient_id") &&
                                        has_col(x$infections, "infection_date")) {
    inf <- x$infections[x$infections$patient_id %in% first_catheter$patient_id &
                          !is.na(x$infections$infection_date), , drop = FALSE]
    inf <- inf[order(inf$patient_id, inf$infection_date), , drop = FALSE]
    inf <- inf[!duplicated(inf$patient_id), , drop = FALSE]
    start <- first_catheter$pd_start_date[match(inf$patient_id, first_catheter$patient_id)]
    mean_days(as.numeric(inf$infection_date - start))
  } else {
    mean_days(numeric(0))
  }

  list(n_catheters = n_catheters,
       procedure_type = procedure_type,
       time_to_pd_start_days = time_to_pd_start_days,
       time_to_first_peritonitis_days = time_to_first_peritonitis_days)
}



#' Infection detail: organisms cultured, PE outcomes, culture-negative rate
#' and peritonitis-related catheter removal
#'
#' \code{x$infections$organisms} is a comma-joined string per episode (an
#' episode can culture more than one organism), so this re-splits it to
#' tabulate at the organism level rather than the row level. An episode with
#' no organism recorded serialises to \code{""} or the literal
#' string \code{"NA"} (a list holding a missing value) -- neither is a real
#' organism name, so both are dropped before tabulating.
#'
#' @param x A \code{pd_unit} object.
#'
#' @return A list with \code{organisms} and \code{pe_outcomes} tables
#'   (\code{pe_outcomes}' \code{NA}, no complication flagged, is relabelled
#'   \code{"none"}); \code{culture_negative_n} and \code{culture_negative_pct}
#'   (episodes with no organism identified, out of all episodes); and
#'   \code{peritonitis_removal_n} and \code{peritonitis_removal_pct}
#'   (catheters whose \code{removal_reason} mentions peritonitis, out of all
#'   catheters).
#' @noRd
#'
summarise_infections <- function(x) {
  organisms <- if (has_col(x$infections, "organisms")) {
    raw <- x$infections$organisms
    raw <- raw[!is.na(raw)]
    tokens <- trimws(unlist(strsplit(raw, ",\\s*")))
    tokens <- tokens[nzchar(tokens) & tokens != "NA"]
    if (length(tokens) == 0) table(character(0)) else table(tokens)
  } else {
    table(character(0))
  }

  pe_outcomes <- if (has_col(x$infections, "outcome")) {
    out <- x$infections$outcome
    out[is.na(out)] <- "none"
    table(out)
  } else {
    table(character(0))
  }

  n_infections <- nrow(x$infections)
  culture_negative_n <- if (has_col(x$infections, "organisms")) {
    is_negative <- vapply(x$infections$organisms, function(v) {
      if (is.na(v)) return(TRUE)
      toks <- trimws(unlist(strsplit(v, ",\\s*")))
      toks <- toks[nzchar(toks) & toks != "NA"]
      length(toks) == 0 || all(tolower(toks) == "negative")
    }, logical(1))
    sum(is_negative)
  } else {
    0L
  }
  culture_negative_pct <- if (n_infections == 0) NA_real_ else culture_negative_n / n_infections

  n_catheters <- nrow(x$catheters)
  peritonitis_removal_n <- if (has_col(x$catheters, "removal_reason")) {
    sum(grepl("peritonitis", x$catheters$removal_reason, ignore.case = TRUE), na.rm = TRUE)
  } else {
    0L
  }
  peritonitis_removal_pct <- if (n_catheters == 0) NA_real_ else peritonitis_removal_n / n_catheters

  list(organisms = organisms, pe_outcomes = pe_outcomes,
       culture_negative_n = culture_negative_n,
       culture_negative_pct = culture_negative_pct,
       peritonitis_removal_n = peritonitis_removal_n,
       peritonitis_removal_pct = peritonitis_removal_pct)
}



#' Label a benchmark verdict: MET, NOT MET, or No Data when there is nothing to judge
#' @noRd
verdict_label <- function(met) {
  if (is.na(met)) "No Data" else if (met) "MET" else "NOT MET"
}


# Layout helpers

summary_width <- 56L
summary_label_width <- 32L

# heavy rule under the title plain "="
summary_rule <- function(width = summary_width) {
  char <- if (isTRUE(l10n_info()[["UTF-8"]])) "\u2550" else "="
  strrep(char, width)
}

# section heading
summary_heading <- function(title) {
  cat("\n ", toupper(title), "\n", sep = "")
}

# one aligned "label   value" row, indented under its heading
summary_row <- function(label, value, label_width = summary_label_width) {
  cat("   ", format(label, width = label_width), value, "\n", sep = "")
}

# rows from a named list/vector (episode types, cohort outcomes), a placeholder line when empty
summary_rows <- function(x, empty) {
  if (length(x) == 0) {
    cat("   ", empty, "\n", sep = "")
  } else {
    for (nm in names(x)) summary_row(nm, x[[nm]])
  }
}

# benchmark text and verdict, e.g. "<= 0.40        [ MET ]".
summary_benchmark <- function(op, value, verdict, value_width = 14L) {
  sprintf("%s [ %s ]", format(paste(op, value), width = value_width), verdict)
}





#' Summarise a pd_unit object
#'
#' Reports the unit's headline peritonitis indicators (unit's peritonitis rate
#' and peritonitis free percentage against their ISPD benchmarks),
#' a breakdown of episodes by type, the cohort's outcomes and
#' incident/prevalent split, patient demographics, a catheter summary,
#' and peritonitis episodes summary.
#'
#' @param object A \code{pd_unit} object.
#' @param ... Ignored.
#'
#' @return Invisibly, a list with components \code{rate}, \code{rate_num},
#'   \code{rate_den}, \code{rate_benchmark}, \code{rate_met}, \code{pf},
#'   \code{pf_num}, \code{pf_den}, \code{pf_benchmark}, \code{pf_met}
#'   (\code{rate_met} and \code{pf_met} are \code{NA}, printed as \code{No Data},
#'   when there is no rate or percentage to judge),
#'   \code{episode_types} (a table of episode counts by \code{episode_type}),
#'   \code{outcomes} (a table of patient counts by how their PD ended:
#'   \code{death}, \code{transplant}, \code{permanent transfer to HD},
#'   \code{pd stopped}, or \code{still active}), \code{cohort} (a named
#'   integer vector of \code{incident}/\code{prevalent} counts),
#'   \code{median_age_years} (this cohort's median patient age at \code{t1}),
#'   \code{demographics} (a list of tables: \code{gender}, \code{ethnicity},
#'   \code{primary_kidney_disease}, \code{diabetes_status},
#'   \code{smoking_status}, \code{dialysis_type}), \code{catheters} (a list
#'   with \code{n_catheters}, \code{procedure_type}, and, for incident
#'   patients, the mean \code{time_to_pd_start_days} (insertion to PD start)
#'   and \code{time_to_first_peritonitis_days} (PD start to first episode),
#'   each as \code{mean} and \code{n}) and \code{infections} (a list with
#'   \code{organisms} and \code{pe_outcomes} tables, plus
#'   \code{culture_negative_n}/\code{_pct} and
#'   \code{peritonitis_removal_n}/\code{_pct}).
#' @export
#'
summary.pd_unit <- function(object, ...) {
  x <- object

  rate <- summarise_rate(x)
  pf <- summarise_pf(x)
  episode_types <- summarise_episode_types(x)
  outcomes <- summarise_outcomes(x)
  cohort <- summarise_cohort(x)
  mean_age_years <- summarise_mean_age(x)
  demographics <- summarise_demographics(x)
  catheters <- summarise_catheters(x)
  infections <- summarise_infections(x)

  fmt_demo <- function(tbl) {
    if (length(tbl) == 0) return("(none)")
    paste(sprintf("%s %d", names(tbl), tbl), collapse = ", ")
  }

  fmt_mean_days <- function(m) {
    if (is.na(m[["mean"]])) return("NA")
    sprintf("%.1f days", m[["mean"]])
  }

  fmt_n_pct <- function(n, pct) {
    if (is.na(pct)) sprintf("%d (NA)", n) else sprintf("%d (%.1f%%)", n, pct * 100)
  }

  unit_label <- if (is.na(x$unit_id)) "(Unnamed unit)" else x$unit_id

  cat("<summary.pd_unit>  ", unit_label, "\n", sep = "")
  cat("Reporting period   ", format(x$t0), " to ", format(x$t1), "\n", sep = "")
  cat(summary_rule(), "\n", sep = "")

  summary_heading("Peritonitis rate")
  summary_row("Rate", paste0(
    if (is.na(rate$rate)) "NA" else sprintf("%.3f", rate$rate),
    " episodes/patient-year"))
  summary_row("Countable episodes", rate$rate_num)
  summary_row("Patient-years at risk", sprintf("%.2f", rate$rate_den))
  summary_row("ISPD benchmark",
              summary_benchmark("<=", sprintf("%.2f", rate$rate_benchmark),
                                verdict_label(rate$rate_met)))

  summary_heading("Peritonitis-free (PF)")
  summary_row("PF", if (is.na(pf$pf)) "NA" else sprintf("%.1f%%", pf$pf * 100))
  summary_row("Patients, zero countable eps", paste(pf$pf_num, "of", pf$pf_den))
  summary_row("ISPD benchmark",
              summary_benchmark(">", sprintf("%.0f%%", pf$pf_benchmark * 100),
                                verdict_label(pf$pf_met)))

  summary_heading("Episodes by type")
  summary_rows(episode_types, "(no episodes recorded)")

  summary_heading("Cohort outcomes")
  summary_rows(outcomes, "(no patients recorded)")

  summary_heading("Demographics")
  summary_row("Mean age",
              if (is.na(mean_age_years)) "unknown" else sprintf("%.0f (at t1)", mean_age_years))
  summary_row("Gender",         fmt_demo(demographics$gender))
  summary_row("Ethnicity",      fmt_demo(demographics$ethnicity))
  summary_row("Kidney disease", fmt_demo(demographics$primary_kidney_disease))
  summary_row("Diabetes",       fmt_demo(demographics$diabetes_status))
  summary_row("Smoking status", fmt_demo(demographics$smoking_status))
  summary_row("Dialysis type",  fmt_demo(demographics$dialysis_type))

  summary_heading("Catheters")
  summary_row("Total",                          catheters$n_catheters)
  summary_row("Procedure type",                 fmt_demo(catheters$procedure_type))
  summary_row("Time to PD start (mean)",        fmt_mean_days(catheters$time_to_pd_start_days))
  summary_row("Time to 1st peritonitis (mean)", fmt_mean_days(catheters$time_to_first_peritonitis_days))

  summary_heading("Infections")
  summary_row("Organisms cultured",           fmt_demo(infections$organisms))
  summary_row("PE outcome",                   fmt_demo(infections$pe_outcomes))
  summary_row("Culture-negative peritonitis",
              fmt_n_pct(infections$culture_negative_n, infections$culture_negative_pct))
  summary_row("Peritonitis catheter removal",
              fmt_n_pct(infections$peritonitis_removal_n, infections$peritonitis_removal_pct))

  summary_heading("Cohort")
  summary_row("Patients", paste0(x$n_patients, " (", cohort[["incident"]], " incident, ",
                                 cohort[["prevalent"]], " prevalent)"))
  cat("\n")

  invisible(list(
    rate = rate$rate, rate_num = rate$rate_num, rate_den = rate$rate_den,
    rate_benchmark = rate$rate_benchmark, rate_met = rate$rate_met,
    pf = pf$pf, pf_num = pf$pf_num, pf_den = pf$pf_den,
    pf_benchmark = pf$pf_benchmark, pf_met = pf$pf_met,
    episode_types = episode_types,
    outcomes = outcomes,
    cohort = cohort,
    mean_age_years = mean_age_years,
    demographics = demographics,
    catheters = catheters,
    infections = infections
  ))
}
