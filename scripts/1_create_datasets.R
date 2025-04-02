# This file will take as options a grade (3, 4, 5) and a subject (glmath, readng)
# This will output a CSV titled 2019_{grade}_{subject}_df.csv in the data subdirectory
# This will output a log in logs/clarify_files.txt
log = list(
    file_started = Sys.time(),
    arguments = NA,
    original_rows = NA,
    original_columns = NA,
    filtered_rows = NA,
    filtered_columns = NA,
    grouped_columns = NA,
    num_NA_raceth = NA,
    num_problematic_attend_p0 = NA,
    total_runtime = NA
)

library(dplyr)
library(haven)
library(readr)
source("scripts/helpers.R")

# Parse the input options
arguments <- commandArgs(trailingOnly = TRUE)
grade <- as.character(arguments[1])
subject <- as.character(arguments[2])
year <- as.character(arguments[3])
outcome_var <- paste0(subject, "_scr_p0")
outcome_lagged_var <- paste0(subject, "_scr_m1")

# Update the log
log$arguments <- paste("Grade:", grade, "Subject:", subject) 


# Pull in the dataset
columns_of_interest <- c(
    "replacement_id",
    "schoolid_state_enroll_p0",
    "gradelevel",
    "age",
    "gender",
    "raceth",
    outcome_var,
    "specialed_ever",
    "homeless_ever",
    "lep_ever",
    "migrant_ever",
    "homeless_ever",
    "attend_p0"
)

if(as.numeric(grade) > 3){
    outcome_lagged_var <- paste0(subject, "_scr_m1")
    columns_of_interest <- c(columns_of_interest, outcome_lagged_var)
}

# Read in the data
data_path = file.path("/home", "tea", "data", "current", paste0("TX", year, "_DRV_UMICH", ".dta"))
df <- read_dta(data_path, col_select = all_of(columns_of_interest), n_max = Inf)

# Update the log
log$original_rows <- nrow(df)
log$original_columns <- colnames(df)


# Filter the data to the grade of interest
df <- df %>% filter(gradelevel == grade) %>% select(-all_of("gradelevel"))

# Update the log
log$filtered_rows <- nrow(df)
log$filtered_columns <- colnames(df)

# Get rid of rows for which raceth is NA
df <- df %>% filter(!is.na(raceth))

# Update the log
log$num_NA_raceth <- log$filtered_rows - nrow(df)

# Use helper function to one-hot encode gender and raceth
df <- df %>% one_hot(c("gender", "raceth"))
current_columns <- df %>% colnames()
covariates <- current_columns[!current_columns %in% c("replacement_id", "schoolid_state_enroll_p0", outcome_var)]

# Change all rows with attend_p0 > 1 to NA (It's a small amount, so we'll just remove them when fitting)
log$num_problematic_attend_p0 <- df %>% filter(attend_p0 > 1) %>% nrow()
df <- df %>% mutate(attend_p0 = ifelse(attend_p0 > 1, NA, attend_p0))

# Divide every age, outcome_var, and lagged_outcome_var value by their max value to get all covariates on the same scale
df <- df %>% mutate(age = age / max(df$age, na.rm = TRUE))
df <- df %>% mutate(!!sym(outcome_var) := .data[[outcome_var]] / max(df[[outcome_var]], na.rm = TRUE))
if(as.numeric(grade)  > 3){
    df <- df %>% mutate(!!sym(outcome_lagged_var) := .data[[outcome_lagged_var]] / max(df[[outcome_lagged_var]], na.rm = TRUE))
}

# Use helper function to add in the school-level averages
df <- df %>% mutate(schoolid_state_enroll_p0 = as.factor(schoolid_state_enroll_p0)) %>% mutate(replacement_id = as.factor(replacement_id))
df <- df %>% make_grouped(grouping = "schoolid_state_enroll_p0", group_level = c(), unit_level = covariates,  outcome = outcome_var)

# Update the log
log$grouped_columns <- colnames(df)

# And write the dataset
filename <- paste(year, grade, subject, "df.csv", sep = "_")
filepath <- file.path("data", filename)
write_csv(df, filepath)

# Update and write the log
log$total_runtime <- Sys.time() - log$file_started
write_log(log, paste("create_datasets", arguments[1], arguments[2], arguments[3], "log.txt", sep = "_"))