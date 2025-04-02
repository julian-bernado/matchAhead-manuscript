# This file will take in a grade (3, 4, 5) and a subject (glmath, readng)
# This file will output an lme4:lmer model object saved in {grade}_{subject}_model.rds in the models subdirectory.
# It will also output a log in logs/fit_models_{grade}_{subject}_log.txt.
log = list(
    file_started = Sys.time(),
    arguments = NA,
    anova = NA,
    REMLcrit = NA,
    family = NA,
    logLik = NA,
    nobs = NA,
    ngrps = NA,
    total_runtime = NA
)

# Load libraries
library(dplyr)
library(readr)
library(lme4)
source("scripts/helpers.R")

# Parse the input options
arguments <- commandArgs(trailingOnly = TRUE)
grade <- as.character(arguments[1])
subject <- as.character(arguments[2])

outcome_var <- paste0(subject, "_scr_p0")

# Update the log
log$arguments <- paste("Grade:", grade, "Subject:", subject)

# Pull in the dataset
df <- read_csv(file.path("data", paste0("2019_", grade, "_", subject, "_df.csv")))

# Fit the model with helper function
model_formula <- write_formula(df, grade, subject)
model <- lmer(
    formula = model_formula,
    data = df,
    REML = TRUE
)

# Save the model
saveRDS(model, file = file.path("models", paste0(grade, "_", subject, "_model.rds")))

# Log the model with helper function
log <- log_model(log, model)
log$total_runtime <- Sys.time() - log$file_started
write_log(log, paste("fit_models", arguments[1], arguments[2], "log.txt", sep = "_"))