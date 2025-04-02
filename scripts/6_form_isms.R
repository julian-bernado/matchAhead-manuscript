# This file will take in arguments for grade and subject
# It will transform the files in distances/chunked_distances/{grade}_{subject}/{grade}_{subject}_*_distances.csv
# Into distances/{grade}_{subject}_distances.rds
# Where the rds is an infinity sparse matrix
log <- list(
    file_started = Sys.time(),
    arguments = NA,
    original_rows = NA,
    final_rows = NA,
    bias_max = NA,
    num_infinite = NA,
    ess_summary = NA,
    bias_summary = NA,
    total_runtime = NA
)

# Load libraries
library(dplyr)
library(tidyr)
library(readr)
library(optmatch)
source("scripts/helpers.R")

# Parse the input options
arguments <- commandArgs(trailingOnly = TRUE)
grade <- as.character(arguments[1])
subject <- as.character(arguments[2])

# Update the log
log$arguments <- paste("Grade:", grade, "Subject:", subject)

# Now we load the chunked dataframes into memory as a single tibble
chunked_dir <- file.path("distances", "chunked_distances", paste(grade, subject, sep = "_"))
chunked_files <- list.files(chunked_dir)
chunked_paths <- paste(chunked_dir, chunked_files, sep = "/")
df <- read_csv(chunked_paths)

# Write the log
log$original_rows <- nrow(df)

# Remove any duplicate rows
df <- df %>% distinct()

# Write the log
log$final_rows <- nrow(df)

# Now, let's get scale bias by their maximum non-infinite values
ess_max <- df %>% filter(ess_measure != Inf) %>% pull(ess_measure) %>% max()
bias_max <- df%>% pull(bias_measure) %>% max()
df <- df %>% mutate(bias_measure = bias_measure / bias_max)

# Write the log
log$bias_max <- bias_max
log$ess_summary <- df %>% pull(ess_measure) %>% summary()
log$bias_summary <- df %>% pull(bias_measure) %>% summary()

# Then we actually create the distance matrix
df <- df %>% mutate(distance = if_else(is.infinite(ess_measure), Inf, sqrt(ess_measure*bias_measure))) %>%
    select(all_of(c("treatment", "control", "distance")))

# Write the log
log$num_infinite <- df %>% filter(is.infinite(distance)) %>% nrow()

# And we cast it to a matrix
ism <- df %>%
  pivot_wider(names_from  = control, values_from = distance) %>%
  as.matrix()


# We make sure the rownames are the treatment schools
treatment_schools <- df %>% pull(treatment) %>% as.character() %>% unique()
rownames(ism) <- treatment_schools

# Then we convert it to an ISM and save it
ism <- as.InfinitySparseMatrix(ism)
saveRDS(ism, file.path("distances", paste(grade, subject, "distances.rds", sep = "_")))

# Save the log
log$total_runtime <- Sys.time() - log$file_started
write_log(log, paste("form_isms", grade, subject, "log.txt", sep = "_"))