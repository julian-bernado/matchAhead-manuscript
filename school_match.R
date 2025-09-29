library(dplyr)
library(readr)
library(optmatch)
library(propertee)

# Function to generate synthetic multilevel data
generate_multilevel_data <- function(S, Ns, p, gsd = 10, ssd = 1){
  # S: Number of groups
  # Ns: Number of units per group (can be a vector for varying sizes)
  # p: Number of covariates
  # gsd: Group-level standard deviation
  # ssd: Unit-level standard deviation
  
  # Initialize the group structure
  unique_groups <- 1:S
  groupmeans <- rnorm(S, mean = 0, sd = gsd)
  
  if(length(Ns) == 1){
    Ns <- rep(Ns, S)
  }
  
  N <- sum(Ns)
  assigned_groups <- rep(unique_groups, times = Ns)
  
  # Initialize the member-level structure within groups
  X <- matrix(rnorm(N * p, mean = 0, sd = 1), nrow = N, ncol = p)
  beta <- rnorm(p, mean = 0, sd = 1)
  
  member_outcomes <- numeric(N)
  for(i in 1:S){
    group_indices <- which(assigned_groups == i)
    group_X <- X[group_indices, ]
    group_member_outcomes <- rnorm(Ns[i], mean = group_X %*% beta + groupmeans[i], sd = ssd)
    member_outcomes[group_indices] <- group_member_outcomes
  }
  
  # Combine covariates and outcomes into a dataframe
  colnames(X) <- paste0("X", 1:p)
  design <- as_tibble(X) %>%
    mutate(Y = member_outcomes) %>%
    mutate(Group = assigned_groups) %>%
    relocate(Y, Group)
  
  return(design)
}

# Updated assign_treatment function
assign_treatment <- function(df, grouping_var, Nt = NULL){
  # df: Data frame containing the data
  # grouping_var: String. Name of the grouping variable in df
  # Nt: Number of groups to assign to treatment. If NULL, defaults to half of the groups.
  
  # Check if the grouping variable exists in the dataframe
  if(!grouping_var %in% colnames(df)){
    stop(paste("Grouping variable", grouping_var, "not found in the data frame."))
  }
  
  # Extract unique groups
  groups <- unique(df[[grouping_var]])
  S <- length(groups)
  
  # Set Nt (number of treated groups)
  if(is.null(Nt)){
    Nt <- floor(S / 2)
    cat(paste("Nt not provided. Defaulting to half of the groups:", Nt, "\n"))
  } else {
    Nt <- as.integer(Nt)
    if(Nt > S){
      stop("Nt cannot be greater than the number of unique groups.")
    }
  }
  
  # Randomly sample Nt groups to assign treatment
  treated_groups <- sample(groups, size = Nt)
  
  # Assign Treatment: 1 for treated groups, 0 for control groups
  df <- df %>%
    mutate(Treatment = if_else(.data[[grouping_var]] %in% treated_groups, 1, 0))
  
  cat(paste("Assigned Treatment to", Nt, "groups out of", S, "total groups.\n"))
  
  return(df)
}

df <- generate_multilevel_data(S = 1000, Ns = 400, p = 2)
df <- assign_treatment(df, "Group", Nt = 50)
df <- df %>% mutate(std_id = 1:length(df$Y))
schools <- df |> pull(Group) |> unique()
treatment_schools <- df |> filter(Treatment == 1) |> pull(Group) |> unique()
unmatched_controls <- df |> filter(Treatment == 0) |> pull(Group) |> unique()
matched_sets <- setNames(character(length(schools)), schools)
print(length(unmatched_controls))

i <- 1
for (trt in treatment_schools) {
  match <- sample(unmatched_controls, size = 1)
  matched_sets[trt] <- i
  matched_sets[match] <- i
  unmatched_controls <- unmatched_controls[unmatched_controls != match]
  i <- i + 1
}

print(i)
df$school_match <- matched_sets[df$Group]
df <- df |> filter(nchar(school_match) > 0)

# initialize the column
df$student_match <- NA_character_

# iterate over each school-match pair
pair_ids <- unique(df$school_match)

for (pair_id in pair_ids) {
  # restrict to this pair
  pair_idx <- which(df$school_match == pair_id)
  pair_df  <- df[pair_idx, , drop = FALSE]

  # treated and control student ids within the pair
  treated_ids <- pair_df$std_id[pair_df$Treatment == 1]
  control_ids <- pair_df$std_id[pair_df$Treatment == 0]

  # shuffle treated students to randomize allocation order
  treated_ids <- sample(treated_ids, length(treated_ids))
  available_controls <- control_ids

  for (t_i in seq_along(treated_ids)) {
    remaining_controls <- length(available_controls)
    remaining_treated  <- length(treated_ids) - t_i + 1
    max_k <- min(5L, remaining_controls - (remaining_treated - 1L))
    if (is.na(max_k) || max_k < 1L) {
      k <- 1L
      if (remaining_controls < 1L) next
    } else {
      k <- sample.int(max_k, size = 1L)
    }

    chosen_controls <- sample(available_controls, size = k, replace = FALSE)
    available_controls <- setdiff(available_controls, chosen_controls)
    match_id <- paste0("sm", pair_id, "_", t_i)

    df$student_match[df$std_id == treated_ids[t_i]] <- match_id
    df$student_match[df$std_id %in% chosen_controls] <- match_id
  }
}

df <- df |> filter(nchar(student_match) > 2)
df$Treatment <- as.integer(df$Treatment)

####################
# actual propertee #
####################
df <- df |>
  mutate(
    Treatment     = as.integer(Treatment),
    student_match = as.character(student_match),
    school_match  = as.character(school_match),
    Group         = as.integer(Group)
  ) |>
  as.data.frame()


# pass weights into lmitt
output <- lmitt(Y ~ 1,  data = df, specification = Treatment ~ block(student_match) + uoa(std_id), absorb = TRUE)
# spec <- obs_spec(Treatment ~ cluster(Group) + block(school_match), data = df)
# output@StudySpecification <- spec
print(summary(output))