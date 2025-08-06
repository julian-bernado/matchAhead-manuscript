
source("scripts/match_net.R")

treatment <- rnorm(10)
control <- rnorm(25)
# Initialize
observation <- match_net(treatment, control, 0.5)

# determine resolution
if (observation$e1 == nt(observation)) {
  print("blurry -> foggy")
  observation <- set_max_controls(observation, 1)
}
if (observation$e2 == nt(observation)) {
  print("foggy -> crystal")
  observation <- set_max_controls(observation, 5)
}

# max flow calculation
observation <- max_flow(observation)

# min cost calculation
observation <- min_cost(observation)

# bias calculation
observation <- calc_bias(observation)

observation