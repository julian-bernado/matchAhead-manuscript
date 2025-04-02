library(dplyr)
library(readr)
source("scripts/maxflow.R")

calipered_dist <- function(x, y, caliper){
  caliper <- as.numeric(caliper)
  if(abs(x-y) < caliper){
    return(0)
  } else{
    return(NA)
  }
}

calc_ess <- function(treatment_school, control_school, student_scores, caliper){
  # Extract treatment and control school scores
  treatment_indices <- which(as.character(student_scores$school_id) == as.character(treatment_school))
  control_indices <- which(as.character(student_scores$school_id) == as.character(control_school))
  all_scores <- student_scores$student_score
  treatment_school_scores <- all_scores[treatment_indices]
  control_school_scores <- all_scores[control_indices]

  Nt <- length(treatment_school_scores)

  # Get a distance matrix based on these scores
  distance_matrix <- outer(treatment_school_scores, control_school_scores, Vectorize(function(x, y) calipered_dist(x, y, caliper = caliper)))
  # Count valid matches
  num_w_matches <- sum(rowSums(!is.na(distance_matrix)) > 0)

  # Output
  ess <- NA
  if(num_w_matches < Nt){
    if(num_w_matches == 0){
      ess <- Inf
    } else{
      ess <- Nt / num_w_matches
    }
  } else{
    mc <- max_controls(distance_matrix, max.controls = 1)
    ess <- 1/mc
  }
  return(ess)
}

calc_bias <- function(treatment_school, control_school, school_scores){
  all_scores <- school_scores %>% pull(school_score)
  all_ids <- school_scores %>% pull(school_id)
  treatment_index <- which(as.character(all_ids) == as.character(treatment_school))
  control_index <- which(as.character(all_ids) == as.character(control_school))
  return(abs(all_scores[treatment_index] - all_scores[control_index]))
}

get_distance <- function(school_scores, student_scores, treatment_schools, caliper, parallel_id){
  # Making sure maxflow is sourced
  source("scripts/maxflow.R")

  # First, we list the treatment and control schools
  school_scores <- school_scores %>% mutate(treatment = school_id %in% treatment_schools)
  control_schools <- school_scores %>% filter(!treatment) %>% pull(school_id)

  # Now, we generate every pair of (treatment, control) schools
  school_pairs <- tibble(expand.grid(treatment = treatment_schools, control = control_schools, stringsAsFactors=FALSE))

  # Now, I will create a new column that is created through an apply call to the bias function
  # Then, I do the same with the variance_measure function
  school_pairs <- school_pairs %>%
    rowwise() %>%
    mutate(bias_measure = calc_bias(as.character(treatment), as.character(control), school_scores)) %>%
    ungroup()

  school_pairs <- school_pairs %>%
    rowwise() %>%
    mutate(ess_measure = calc_ess(treatment, control, student_scores, caliper)) %>%
    ungroup()

  # Write the output to a CSV
  output_path = file.path("distances",
  "chunked_distances",
  paste(parallel_id[1], parallel_id[2], sep = "_"),
  paste(parallel_id[1], parallel_id[2], parallel_id[3], "distances.csv", sep = "_")
  )
  write_csv(school_pairs, output_path)
}