library(dplyr)
library(lme4)

write_log <- function(log, filename){
  # Create logs directory if it doesn't exist
  if(!dir.exists("logs")){
    dir.create("logs")
  }
  
  # Open file connection
  log_file <- file(paste0("logs/", filename), "w")
  
  # Loop through each element in the log list
  for(field_name in names(log)){
    writeLines(field_name, log_file)
    writeLines(as.character(log[[field_name]]), log_file)
    writeLines(paste(rep("-", 40), collapse = ""), log_file)
  }
  
  close(log_file)
}

log_model <- function(log, model){
  log$anova <- anova(model)
  log$REMLcrit <- REMLcrit(model)
  log$family <- family(model)
  log$logLik <- logLik(model)
  log$nobs <- nobs(model)
  log$ngrps <- ngrps(model)
  return(log)
}

one_hot <- function(data, vars){
    one_hots_only <- data %>% select(all_of(vars))
    for(var in vars){
        unique_vals <- one_hots_only %>% pull(var) %>% unique()
        for(val in unique_vals){
            newvarname <- paste0(var, "_", val)
            data <- data %>% mutate(!!newvarname := ifelse(.data[[var]] == val, 1, 0))
        }
        data <- data %>% select(-all_of(var))
    }

    return(data)
}

make_grouped <- function(data, grouping = "Group", group_level, unit_level, outcome = "Y") {
  # Combine all relevant covariates including the outcome
  all_covars <- c(grouping, group_level, unit_level, outcome)
  
  # Check if all specified columns exist in the data
  missing_cols <- setdiff(all_covars, names(data))
  if(length(missing_cols) > 0){
    stop(paste("The following columns are missing in the data:", paste(missing_cols, collapse = ", ")))
  }
  
  # Select only the necessary columns
  df <- data %>%
    select(all_of(all_covars))
  
  # Identify numeric unit-level variables for processing
  numeric_unit_level <- df %>%
    select(all_of(unit_level)) %>%
    select(where(is.numeric)) %>%
    names()
  
  # Warn if some unit_level variables are non-numeric and will be excluded
  non_numeric <- setdiff(unit_level, numeric_unit_level)
  if(length(non_numeric) > 0){
    warning(paste("The following unit_level variables are non-numeric and will be excluded from deviation calculations:", 
                  paste(non_numeric, collapse = ", ")))
  }
  
  # Convert grouping variable to symbol for tidy evaluation
  grouping_sym <- sym(grouping)
  
  # Step 1: Calculate group-wise averages and create new avg_* columns
  avg_df <- df %>%
    group_by(!!grouping_sym) %>%
    summarise(across(
      all_of(numeric_unit_level),
      ~ mean(.x, na.rm = TRUE),
      .names = "avg_{col}"
    )) %>%
    ungroup()
  
  # Step 2: Join the averages back to the original dataframe
  df_with_avg <- df %>%
    left_join(avg_df, by = grouping)
  
  # Step 3: Overwrite original unit-level columns with deviations
  for(col in numeric_unit_level){
    avg_col <- paste0("avg_", col)
    df_with_avg <- df_with_avg %>%
      mutate(!!sym(col) := .data[[col]] - .data[[avg_col]])
  }
  
  return(df_with_avg)
}

write_formula <- function(data, grade, subject){  
  outcome_name <- paste0(subject, "_scr_p0")
  columns <- colnames(data)
  to_remove <- c("schoolid_state_enroll_p0", outcome_name, "gender_1", "avg_gender_1", "raceth_1", "avg_raceth_1")
  if (as.numeric(grade) < 4){
    lagged_outcome_name <- paste0(subject, "_scr_m1")
    avg_lagged_outcome_name <- paste0("avg_", lagged_outcome_name)
    to_remove <- c(to_remove, lagged_outcome_name, avg_lagged_outcome_name)
  }
  covariates <- columns[!columns %in% to_remove]

  lhs_formula <- paste0(outcome_name, " ~ ", " (1 | schoolid_state_enroll_p0) + ")
  
  # Collapse covariates into a single string for the right-hand side
  rhs_formula <- paste(covariates, collapse = " + ")
  
  # Return the formula, concatenating lhs and rhs
  return(formula(paste0(lhs_formula, rhs_formula)))
}