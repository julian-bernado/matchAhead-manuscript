# We're going to simulate a setting here to see how the two methods vary with the size of the final
source("scripts/maxflow.R")
library(optmatch)
library(dplyr)
options(warn=-1)

# MatchAhead distances
calipered_dist <- function(x, y, caliper){
  caliper <- as.numeric(caliper)
  if(abs(x-y) < caliper){
    return(0)
  } else{
    return(NA)
  }
}

calc_ess <- function(treatment_scores, control_scores, caliper, maxcontrols){
  treatment_school_scores <- treatment_scores
  control_school_scores <- control_scores

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
      mc <- max_controls(distance_matrix, max.controls = maxcontrols)
    }
    ess <- 1/mc
  }
  return(ess)
}

# pimentel score
calc_pimentel_measures <- function(treatment_scores, control_scores, maxcontrols) {
  treatment_school_scores <- treatment_scores
  control_school_scores <- control_scores

  # Get a distance matrix based on these scores
  distance_matrix <- outer(
    treatment_school_scores,
    control_school_scores,
    Vectorize(function(x, y) abs(x - y))
  )
  rownames(distance_matrix) <- paste0("treatment_", seq_len(nrow(distance_matrix)))
  colnames(distance_matrix) <- paste0("control_", seq_len(ncol(distance_matrix)))
  matching <- fullmatch(distance_matrix, min.controls = 0, max.controls = maxcontrols)
  bias <- 10
  ess <- effectiveSampleSize(matching)
  return(list(
    bias = bias,
    ess = ess
  ))
}

maxmax <- 10
nreplicants <- 250
matchAhead_times <- matrix(NA, ncol = maxmax, nrow = nreplicants)
pimentel_times <- matrix(NA, ncol = maxmax, nrow = nreplicants)

for (i in 1:maxmax) {
    print(i)
    for (j in 1:nreplicants) {
        treatment_scores <- rbeta(100, 2, 1)
        control_scores <- rbeta(1000, 1, 2)
        caliper <- sd(c(control_scores, treatment_scores))*0.2
        matchAhead_times[j,i] <- system.time(calc_ess(treatment_scores, control_scores, caliper, maxcontrols = i))[3]
        pimentel_times[j,i] <- system.time(calc_pimentel_measures(treatment_scores, control_scores, maxcontrols = i))[3]
    }
}

# Now, I want to make a plot of the times
library(ggplot2)

# For each column, I will make a stacked ggplot
# Each set of replicant runs forms one sample for both matchAhead and pimentel
# For discrete x axis 1 through maxmax, plot a box and whisker plot for both matchAhead and pimentel
# You're getting each of these plots from the replicants for a given value from 1 through maxmax

df <- data.frame(
  maxcontrols = rep(rep(1:maxmax, each = nreplicants), 2),
  time = c(as.vector(matchAhead_times), as.vector(pimentel_times)),
  method = rep(c("matchAhead", "pimentel"), each = maxmax*nreplicants)
)
ggplot(df, aes(x = factor(maxcontrols), y = time, fill = method)) +
  geom_boxplot() +
  labs(x = "maxcontrols", y = "Time (seconds)", fill = "Method") +
  ylim(c(0, 0.5)) 

ggsave("runtimes_plot_control_bigger.png", width = 8, height = 6)
saveRDS(matchAhead_times, "matchAhead_times.rds")
saveRDS(pimentel_times, "pimentel_times.rds")


for (i in 1:length(colSums(matchAhead_times))){
    print(colSums(matchAhead_times)[i]/colSums(pimentel_times)[i])
}

print(colSums(matchAhead_times))
print(colSums(pimentel_times))
options(warn = 0)