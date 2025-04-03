# This file will take in:
#   - a CSV containing school-level scores
#   - a CSV containing student-level scores
#   - a caliper
# Along with the typical grade and subject arguments
# It will output a set of chunked distances for each (grade, subject) pair in distances/chunked_distances
# This file largely works out of scripts called from distances.R
log <- list(
    file_started = Sys.time(),
    arguments = NA,
    original_nt = NA,
    final_nt = NA,
    parallel_splits = NA,
    total_runtime = NA
)

# Load libraries
set.seed(2025)
library(dplyr)
library(readr)
library(doParallel)
source("scripts/helpers.R")
source("scripts/maxflow.R")
source("scripts/distances.R")

# Parse the input options
arguments <- commandArgs(trailingOnly = TRUE)
grade <- as.character(arguments[1])
subject <- as.character(arguments[2])
method <- as.character(arguments[3])

# Update the log
log$arguments <- paste("Grade:", grade, "Subject:", subject)

# Load in the data we're predicting on and our caliper
school_data_path <- file.path("predictions", "school", paste(grade, subject, "school_predictions.csv", sep = "_"))
student_data_path <- file.path("predictions", "student", paste(grade, subject, "student_predictions.csv", sep = "_"))
caliper_path <- file.path("calipers", paste(grade, subject, "caliper.rds", sep = "_"))
school_scores <- read_csv(school_data_path)
student_scores <- read_csv(student_data_path)
caliper <- readRDS(caliper_path)
caliper <- caliper[1,1] # Returned as a matrix

# Create the treatment variable
treatment_schools <- sample(school_scores$school_id, size = 3, replace = FALSE)
final_nt <- length(treatment_schools)

# Write the log
log$original_nt <- original_nt
log$final_nt <- final_nt

# debug with a much smaller dataset
#school_scores <- school_scores[1:100,]
#student_scores <- student_scores %>% filter(as.factor(student_schools) %in% as.factor(school_scores$school_id))
#chosen_schools <- school_scores$school_id
#treatment_schools <- chosen_schools[1:5]

# Batch the treatment schools by assigning each one to a chunk
num_cores <- 3
n_treatment_schools <- length(treatment_schools)
chunk_sequence <- rep(1:num_cores, length.out = n_treatment_schools)
chunk_ids <- sample(chunk_sequence, size = n_treatment_schools, replace = FALSE)
treatment_partition <- data.frame(school_id = as.numeric(treatment_schools), chunk_id = chunk_ids)

# Write to the log
log$parallel_splits <- num_cores

# Add the chunk_id to the school_scores dataframe
school_scores <- school_scores %>%
    left_join(treatment_partition, by = c("school_id" = "school_id")) %>%
    mutate(chunk_id = ifelse(is.na(chunk_id), 0, chunk_id)) %>% # Assign 0 to schools not in treatment
    mutate(school_id = as.character(school_id))


# Merge the school_scores and student_scores dataframes
student_scores <- student_scores %>%
    mutate(student_schools = as.character(student_schools)) %>%
    left_join(school_scores, by = c("student_schools" = "school_id")) %>%
    rename(school_id = student_schools)

# Make lists that will store the data that gets passed into each parallel video
school_chunks <- vector("list", num_cores)
student_chunks <- vector("list", num_cores)
treatment_chunks <- vector("list", num_cores)

# Assign the chunks
for (i in 1:num_cores){
    school_chunks[[i]] <- school_scores %>%
        filter(chunk_id %in% c(0, i)) %>%
        select(school_id, school_score)
    student_chunks[[i]] <- student_scores %>%
        filter(chunk_id %in% c(0, i)) %>%
        select(school_id, student_score)
    treatment_chunks[[i]] <- treatment_partition %>%
        filter(chunk_id == i) %>%
        pull(school_id)
}

# Now, we call the get_distance function in parallel across the chunks
cl <- makeCluster(num_cores)
registerDoParallel(cl)
foreach(i = 1:num_cores, .packages = c("dplyr", "readr", "optmatch")) %dopar% {
  get_distance(
    school_scores = school_chunks[[i]],
    student_scores = student_chunks[[i]],
    treatment_schools = treatment_chunks[[i]],
    caliper = caliper,
    parallel_id = c(grade, subject, i),
    method = method
  )
}
stopCluster(cl)

# serial version for debugging
#for(i in 1:num_cores){
#   get_distance(school_chunks[[i]], student_chunks[[i]], treatment_chunks[[i]], caliper, c(grade, subject, i))
#}


# Update and write the log
log$total_runtime <- Sys.time() - log$file_started
filename <- paste("get_distances", grade, subject, "log.txt", sep = "_")
write_log(log, filename)