library(dplyr)
library(readr)
source("scripts/maxflow.R")

# Functions for calculating matchAhead distances
calipered_dist <- function(x, y, caliper){
  caliper <- as.numeric(caliper)
  if(abs(x-y) < caliper){
    return(0)
  } else{
    return(NA)
  }
}

calc_ess <- function(treatment_school, control_school, student_scores, caliper) {
  # Extract treatment and control school scores
  treatment_indices <- which(
    as.character(student_scores$school_id) == as.character(treatment_school)
  )
  control_indices <- which(
    as.character(student_scores$school_id) == as.character(control_school)
  )
  all_scores <- student_scores$student_score
  treatment_school_scores <- all_scores[treatment_indices]
  control_school_scores <- all_scores[control_indices]

  Nt <- length(treatment_school_scores)

  # Get a distance matrix based on these scores
  distance_matrix <- outer(
    treatment_school_scores,
    control_school_scores,
    Vectorize(function(x, y) calipered_dist(x, y, caliper = caliper))
  )
  # Count valid matches
  num_w_matches <- sum(rowSums(!is.na(distance_matrix)) > 0)

  # Output
  ess <- NA
  if (num_w_matches < Nt) {
    if (num_w_matches == 0) {
      ess <- Inf
    } else {
      ess <- Nt / num_w_matches
    }
  } else {
    mc <- max_controls(distance_matrix, max.controls = 1)
    if (mc == Nt) {
      mc <- max_controls(distance_matrix, max.controls = 5)
    }
    ess <- 1/mc
  }
  return(ess)
}

calc_bias <- function(treatment_school, control_school, school_scores){
  all_scores <- school_scores %>% pull(school_score)
  all_ids <- school_scores %>% pull(school_id)
  treatment_index <- which(
    as.character(all_ids) == as.character(treatment_school)
  )
  control_index <- which(
    as.character(all_ids) == as.character(control_school)
  )
  return(abs(all_scores[treatment_index] - all_scores[control_index]))
}


# Function for calculating Pimentel's distance

calc_pimentel_measures <- function(
  treatment_school,
  control_school,
  school_scores,
  student_scores
) {
  # Extract treatment and control school scores
  treatment_indices <- which(
    as.character(student_scores$school_id) == as.character(treatment_school)
  )
  control_indices <- which(
    as.character(student_scores$school_id) == as.character(control_school)
  )
  all_scores <- student_scores$student_score
  treatment_school_scores <- all_scores[treatment_indices]
  control_school_scores <- all_scores[control_indices]

  # Get a distance matrix based on these scores
  distance_matrix <- outer(
    treatment_school_scores,
    control_school_scores,
    Vectorize(function(x, y) abs(x - y))
  )
  matching <- fullmatch(distance_matrix, min.controls = 0, max.controls = 5)
  bias <- 10
  ess <- effectiveSampleSize(matching)
  return(list(
    bias = bias,
    ess = ess
  ))
}


# Main function that calculates distances given a method
get_distance <- function(
  school_scores,
  student_scores,
  treatment_schools,
  caliper,
  parallel_id,
  method
) {
  # Making sure maxflow is sourced
  source("scripts/maxflow.R")

  # First, we list the treatment and control schools
  school_scores <- school_scores %>%
    mutate(treatment = school_id %in% treatment_schools)
  control_schools <- school_scores %>%
    filter(!treatment) %>%
    pull(school_id)

  # Now, we generate every pair of (treatment, control) schools
  school_pairs <- tibble(
    expand.grid(
      treatment = treatment_schools,
      control = control_schools,
      stringsAsFactors = FALSE
    )
  )

  # We also create columns that track the timing
  # for calculating the bias and effective sample size
  school_pairs <- school_pairs %>%
    mutate(bias_time = NA) %>%
    mutate(ess_time = NA) %>%
    mutate(bias_measure = NA) %>%
    mutate(ess_measure = NA)

  # Now, I will loop through the data
  # I run the method supplied in the function call
  # I calculate bias and effective sample size
  # I time it
  if (method == "matchAhead") {
    for (i in seq_len(nrow(school_pairs))) {
      treatment <- school_pairs$treatment[i]
      control <- school_pairs$control[i]

      # Calculate the bias
      start_time <- Sys.time()
      bias <- calc_bias(
        as.character(treatment),
        as.character(control),
        school_scores
      )
      end_time <- Sys.time()
      bias_time <- end_time - start_time
      bias_time <- as.numeric(bias_time, units = "secs")

      # Calculate the effective sample size
      start_time <- Sys.time()
      ess <- calc_ess(
        as.character(treatment),
        as.character(control),
        student_scores,
        caliper
      )
      end_time <- Sys.time()
      ess_time <- end_time - start_time
      ess_time <- as.numeric(ess_time, units = "secs")

      # Store the results in the dataframe
      school_pairs$bias_measure[i] <- bias
      school_pairs$ess_measure[i] <- ess
      school_pairs$bias_time[i] <- bias_time
      school_pairs$ess_time[i] <- ess_time
    }
  } else if (method == "pimentel") {
    for (i in seq_len(school_pairs)) {
      treatment <- school_pairs$treatment[i]
      control <- school_pairs$control[i]

      # Calculate the measures
      start_time <- Sys.time()
      measures <- calc_pimentel_measures(
        as.character(treatment),
        as.character(control),
        school_scores,
        student_scores
      )
      end_time <- Sys.time()
      total_time <- as.numeric(end_time - start_time, units = "secs")

      # Store the results in the dataframe
      school_pairs$bias_measure[i] <- measures[["bias"]]
      school_pairs$ess_measure[i] <- measures[["ess"]]
      school_pairs$bias_time[i] <- total_time
    }
  } else {
    # Throw an error saying the method name is not recognized
    stop("Method not recognized. Please use 'matchAhead' or 'pimentel'.")
  }
  # Write the output to a CSV
  output_path = file.path("distances",
    paste0(method, "_chunked_distances"),
    paste(parallel_id[1], parallel_id[2], sep = "_"),
    paste(parallel_id[1], parallel_id[2], parallel_id[3], "distances.csv", sep = "_")
  )
  write_csv(school_pairs, output_path)
}