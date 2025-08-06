# I wanna do some OOP type stuff here just because I think an object is the clearest way to think about the network that we do matching on
library("rlemon")
library("optmatch")

# Construct match net object
match_net <- function(
  treatment = numeric(),
  control = numeric(),
  caliper = numeric()
) {
  # Make sure we've passed in the necessary items
  stopifnot(
    is.numeric(treatment),
    is.numeric(control),
    is.numeric(caliper),
    length(caliper) == 1
  )

  distances <- outer(
    treatment,
    control,
    Vectorize(function(x, y) abs(x - y))
  )

  rownames(distances) <- paste0("T", seq_along(treatment))
  colnames(distances) <- paste0("C", seq_along(control))

  calipered_distances <- distances
  calipered_distances[calipered_distances > caliper] <- NA
  e1 <- sum(rowSums(!is.na(calipered_distances)) > 0)

  naive_bias <- abs(mean(treatment) - mean(control))

  structure(
    list(
      treatment = treatment,
      control = control,
      caliper = caliper,
      distances = distances,
      calipered_distances = calipered_distances,
      e1 = e1,
      naive_bias = naive_bias,
      resolution = "blurry",
      max_controls = NULL,
      network = NULL,
      maxflow = NULL,
      maxflow_match = NULL,
      e2 = NULL,
      e3 = NULL,
      ess_dist = NULL,
      humble_bias = NULL,
      arrogant_bias = NULL,
      mincost = NULL,
      mincost_match = NULL,
      optimal_bias = NULL,
      times = NULL
    ),
    class = "match_net"
  )
}

# Methods for match_net
nt <- function(x) UseMethod("nt")

nt.match_net <- function(x) {
  length(x$treatment)
}

nc <- function(x) UseMethod("nc")

nc.match_net <- function(x) {
  length(x$control)
}

set_max_controls <- function(x, max_controls) UseMethod("set_max_controls")

set_max_controls.match_net <- function(
  x,
  max_controls
) {
  if (max_controls == 1) {
    res <- "foggy"
  } else {
    res <- "crystal"
  }
  x$max_controls <- max_controls
  x$resolution <- res
  x$network <- NULL
  x$maxflow_match <- NULL
  x$maxflow <- NULL
  x
}

max_flow <- function(x) UseMethod("max_flow")

max_flow.match_net <- function(x) {
  if (x$resolution == "blurry") {
    print("Resolution blurry, no max flow calculated")
    return(x)
  }
  nwk <- generate_network(x$calipered_distances, x$max_controls)
  lemon_result <- MaxFlow(
    arcSources = nwk$from_node,
    arcTargets = nwk$to_node,
    arcCapacities = nwk$capacity,
    sourceNode = 1,
    destNode = 2,
    numNodes = nc(x) + nt(x) + 2
  )

  x$network <- nwk
  x$maxflow <- lemon_result$cost
  x$maxflow_match <- clean_maxflow_match(x, lemon_result$flows)
  if (x$max_controls == 1) {
    x$e2 <- lemon_result$cost
    x$ess_dist <- 1 / x$e2
  } else {
    x$e3 <- lemon_result$cost
    x$ess_dist <- 1 / x$e3
  }
  x
}

calc_bias <- function(x) UseMethod("calc_bias")

calc_bias.match_net <- function(x) {
  if (is.null(x$maxflow_match)) {
    stop("Need to calculate max flow before bias")
  }
  # First, just take the treatment and control units
  # and take the absolute different in averages
  treatment_avg <- mean(x$treatment)
  naive_control_avg <- mean(x$control)
  x$naive_bias <- abs(treatment_avg - naive_control_avg)
  # Next, just identify the matched sample and take
  # the absolute difference between average treatment
  # and average matched control
  control_avg <- mean(x$maxflow_match$control_score)
  x$humble_bias <- abs(treatment_avg - control_avg)
  # Now the average within-match distance
  # Each within-match distance is the average
  # distance within that matched set
  x$arrogant_bias <- abs(sum(
    x$maxflow_match$score_diff / x$maxflow_match$match_size
  ) / nt(x))
  # Finally the optimal bias produced by the min cost matching
  # The same as the arrogant bias but using actual values
  x$optimal_bias <- abs(sum(
    x$mincost_match$score_diff / x$mincost_match$match_size
  ) / nt(x))
  x
}

min_cost <- function(x) UseMethod("min_cost")

min_cost.match_net <- function(x) {
  if (x$resolution != "crystal") {
    min_controls <- 0
    mean_controls <- NULL
  }
  suppressWarnings(
    {
      x$mincost_match <- clean_mincost_match(x, fullmatch(
        x$distances,
        min.controls = min_controls,
        max.controls = x$max_controls,
        mean.controls = mean_controls
      ))
    }
  )
  x
}

# Helper functions
generate_network <- function(M, max.controls) {
  nt <- nrow(M)
  nc <- ncol(M)

  # Find all valid matches
  match_indices <- which(!is.na(M), arr.ind = TRUE)

  # Edges from treatments to controls
  from_nodes <- 2 + match_indices[, 1]
  to_nodes <- 2 + nt + match_indices[, 2]
  capacities <- rep(1, nrow(match_indices))

  # Edges from source to treatments
  from_nodes <- c(from_nodes, rep(1, nt))
  to_nodes <- c(to_nodes, 2 + 1:nt)
  capacities <- c(capacities, rep(max.controls, nt))

  # Edges from controls to sink
  from_nodes <- c(from_nodes, 2 + nt + 1:nc)
  to_nodes <- c(to_nodes, rep(2, nc))
  capacities <- c(capacities, rep(1, nc))

  network <- list(
    from_node = from_nodes,
    to_node = to_nodes,
    capacity = capacities
  )
  network
}

clean_maxflow_match <- function(x, flow) {
  arc_inds <- which(flow == 1)
  match_sources <- x$network$from_node[arc_inds]
  match_sources <- match_sources[
    which(match_sources > 2 & match_sources <= nt(x) + 2)
  ]
  match_dests <- x$network$to_node[arc_inds]
  match_dests <- match_dests[
    which(match_dests > nt(x) + 2)
  ]
  matched_treatment <- match_sources - 2
  matched_control <- match_dests - nt(x) - 2
  df <- data.frame(
    treatment_id = paste0("T", matched_treatment),
    treatment_score = x$treatment[matched_treatment],
    control_id = paste0("C", matched_control),
    control_score = x$control[matched_control],
    stringsAsFactors = FALSE
  )
  df$score_diff <- df$treatment_score - df$control_score
  df$match_size <- as.vector(table(df$treatment_id)[df$treatment_id])
  df <- df[order(df$treatment_id), ]
  df
}

clean_mincost_match <- function(x, flow) {
  flow <- flow[which(!is.na(flow))]
  control_units <- flow[which(startsWith(names(flow), "C"))]
  control_ids <- names(control_units)
  control_matches <- unname(control_units)

  # Create mapping from treatment values to treatment names
  treatment_units <- flow[which(startsWith(names(flow), "T"))]
  treatment_mapping <- setNames(
    names(treatment_units),
    unname(treatment_units)
  )

  # Create dataframe with treatment and control information
  df <- data.frame(
    treatment_id = treatment_mapping[as.character(control_matches)],
    control_id = control_ids,
    control_score = x$control[as.numeric(substring(control_ids, 2))],
    stringsAsFactors = FALSE
  )

  df$treatment_score <- x$treatment[as.numeric(substring(df$treatment_id, 2))]
  df$score_diff <- df$treatment_score - df$control_score
  df$match_size <- as.vector(table(df$treatment_id)[df$treatment_id])

  # Order by treatment ID
  df <- df[
    order(df$treatment_id),
    c(
      "treatment_id",
      "treatment_score",
      "control_id",
      "control_score",
      "score_diff",
      "match_size"
    )
  ]
  df
}