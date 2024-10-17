# Set the seed for reproducibility
set.seed(2424)

# Generate binary outcomes for 10 mice
outcomes <- rbinom(10, 1, 0.8)

rbinom()

# Print the outcomes
outcomes

U <- runif(5, .6, .8)
successes <- rbinom(5, 100, U)


