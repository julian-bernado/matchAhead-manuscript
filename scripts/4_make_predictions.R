# This file will take in a grade (3, 4, 5) and a subject (glmath, readng)
# This file will output two objects saved in the predictions subdirectoy
# In predictions/school we will save a CSV titled {grade}_{subject}_school_predictions.csv
# In predictions/student we will save a CSV titled {grade}_{subject}_student_predictions.csv
# It will also output a log in logs/make_predictions_{grade}_{subject}_log.txt.
log = list(
    file_started = Sys.time(),
    arguments = NA,
    num_removed = NA,
    schools_predicted = NA,
    students_predicted = NA,
    student_mse = NA,
    total_runtime = NA
)

# Load libraries
library(lme4)
library(dplyr)
library(readr)
source("scripts/helpers.R")

# Parse the input options
arguments <- commandArgs(trailingOnly = TRUE)
grade <- as.character(arguments[1])
subject <- as.character(arguments[2])

# Update the log
log$arguments <- paste("Grade:", grade, "Subject:", subject)

# Now, we load in the data we're predicting on
data_path = file.path("data", paste("2022", grade, subject, "df.csv", sep = "_"))
df <- read.csv(data_path)

# Now we load in the model we're using to predict
model_path <- file.path("models", paste(grade, subject, "model.rds", sep = "_"))
model <- readRDS(model_path)

# Now, we get the covariates that the model's going to use 
covariates <- df %>%
    select(-all_of(c("schoolid_state_enroll_p0", paste0(subject, "_scr_p0")))) %>%
    colnames()

# And we filter the dataset to take out any rows that are missing any of these (some attendance or outcome values may be missing)
previous_size <- df %>% nrow()

for(covar in covariates){
    df <- df %>% filter(!is.na(!!sym(covar)))
}

new_size <- df %>% nrow()

# And we update the log
log$num_removed <- previous_size - new_size

# And we predict student-level scores using that model
student_scores <- predict(model, newdata = df, allow.new.levels = TRUE, na.action = na.omit)
student_schools <- df %>% pull(schoolid_state_enroll_p0)

# Update the log
log$students_predicted <- length(student_schools)
log$student_mse <- mean((df[,paste0(subject, "_scr_p0")] - student_scores)^2, na.rm = TRUE)

# Before predictin school-level scores, we will group the data by school and only keep one observation
# Then we'll set all of the covariates not starting with "avg_" or "schoolid_state_enroll_p0" to 0
df <- df %>%
    group_by(schoolid_state_enroll_p0) %>%
    slice(1) %>%
    mutate(across(
        .cols = -c(starts_with("avg_")),
        .fns = ~ 0
    )) %>%
    ungroup()
    

# Nowe we make our school-level predictions
school_scores <- predict(model, newdata = df, allow.new.levels = TRUE)
school_ids <- df$schoolid_state_enroll_p0

# Update the log
log$schools_predicted <- length(school_ids)

# Now we save our predictions
school_predictions_path <- file.path("predictions", "school", paste(grade, subject, "school_predictions.csv", sep = "_"))
student_predictions_path <- file.path("predictions", "student", paste(grade, subject, "student_predictions.csv", sep = "_"))
write_csv(data.frame(school_id = school_ids, school_score = school_scores), school_predictions_path)
write_csv(data.frame(student_schools = student_schools, student_score = student_scores), student_predictions_path)

# Log the model with helper function
log$total_runtime <- Sys.time() - log$file_started
write_log(log, paste("make_predictions", arguments[1], arguments[2], "log.txt", sep = "_"))