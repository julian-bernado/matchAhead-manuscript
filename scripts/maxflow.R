library("rlemon")

generate_network <- function(M, max.controls){
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