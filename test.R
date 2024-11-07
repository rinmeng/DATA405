# --------------------------------------------- 1
n <- 1000

U <- runif(n)           # Uniformly distributed proposals for x in [0, 1]
V <- runif(n, max = 3)   # Uniformly distributed proposals for y in [0, 3]

# Acceptance condition
ACCEPT <- (V < 3 * U^2)  # Check if V is less than f(U) = 3 * U^2

# Only keep accepted samples
X <- U[ACCEPT]

Y <- 21 + 5*X + rnorm(100,  sd= 2.3)


# ---------------------------------------------  5

# Observed values of X1 and X2
X1 <- 0.12
X2 <- 0.5

lambda1 <- 1
lambda2 <- 10

# Possible values for lambda
lambda_values <- c(lambda1, lambda2)

# Calculate the likelihood function for each lambda
likelihood <- function(lambda, X1, X2) {
  lambda^2 * exp(-lambda * (X1 + X2))
}

# Compute likelihoods for each lambda
likelihoods <- sapply(lambda_values, likelihood, X1 = X1, X2 = X2)

# Display the likelihoods
likelihoods
# Determine the lambda with the maximum likelihood
lambda_mle <- lambda_values[which.max(likelihoods)]

# Output the MLE for lambda
lambda_mle

# --------------------------------------------- 8

# Given values
sample_mean <- 17
population_sd <- 3
sample_size <- 25
confidence_level <- 0.95

# Calculate the standard error
standard_error <- population_sd / sqrt(sample_size)

# Calculate the Z value for 95% confidence (1.96 for normal distribution)
z_value <- qnorm(1 - (1 - confidence_level) / 2)

# Calculate the margin of error
margin_of_error <- z_value * standard_error

# Calculate the confidence interval
lower_bound <- sample_mean - margin_of_error
upper_bound <- sample_mean + margin_of_error

# Display the confidence interval
cat("95% Confidence Interval: (", lower_bound, ", ", upper_bound, ")\n")


# --------------------------------------------- 12

# Set the number of random sample
n <- 1000000

# Generate random uniform numbers in the range [-5, 10]
x_samples <- runif(n, min = -5, max = 10)

# Evaluate the function f(x) = e^sin(x) at each random sample
f_values <- exp(sin(x_samples))

# Estimate the integral: Average of f(x) times the width of the integration interval (15)
integral_estimate <- mean(f_values) * 15

# Standard deviation of the estimates
std_dev <- sd(f_values)

# 95% confidence interval calculation
confidence_level <- 0.95
z_value <- qnorm(1 - (1 - confidence_level) / 2)  # 95% confidence z-value

# Margin of error
margin_of_error <- z_value * (std_dev / sqrt(n)) * 15

# Calculate the confidence interval
lower_bound <- integral_estimate - margin_of_error
upper_bound <- integral_estimate + margin_of_error

# Display the results
cat("Integral Estimate:", integral_estimate, "\n")
cat("95% Confidence Interval: (", lower_bound, ", ", upper_bound, ")\n")


# Define the function f(x) = exp(sin(x))
f <- function(x) exp(sin(x))

# Compute the integral numerically using R's integrate function
true_value <- integrate(f, lower = -5, upper = 10)

# Display the true value of the integral
cat("True value of the integral:", true_value$value, "\n")

