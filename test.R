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
X1 <- 1.5
X2 <- 3.4
X3 <- 0.20

lambda1 <- 2
lambda2 <- 3

# Possible values for lambda
lambda_values <- c(lambda1, lambda2)

# Calculate the likelihood function for each lambda
likelihood <- function(lambda, X1, X2, X3) {
  (X1 + X2 + X3)^(lamda - 1) * exp(-x/2)
}

# Compute likelihoods for each lambda
likelihoods <- sapply(lambda_values, likelihood, X1 = X1, X2 = X2, X3 = X3)

# Display the likelihoods
likelihoods
# Determine the lambda with the maximum likelihood
lambda_mle <- lambda_values[which.max(likelihoods)]

# Output the MLE for lambda
lambda_mle

# --------------------------------------------- 8

# Given values
sample_mean <- 16
population_sd <- 32
sample_size <- 16
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
x_samples <- runif(n, min = 0, max = 1)

# Evaluate the function f(x) = e^sin(x) at each random sample
f_values <- exp(cos(x_samples))

# Estimate the integral: Average of f(x) times the width of the integration interval (15)
integral_estimate <- mean(f_values) * 1

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
f <- function(x) exp(cos(-1*x))

# Compute the integral numerically using R's integrate function
true_value <- integrate(f, lower = -5, upper = 10)

# Display the true value of the integral
cat("True value of the integral:", true_value$value, "\n")


#-------------
glm(formula = y ~ x, family = binomial, data = p13.2)




# a) Standard deviation of the chi-square distribution on 32 degrees of freedom
chi_square_data <- rchisq(1000000, df=32)
chi_square_std_dev <- sd(chi_square_data)

# b) Standard deviation of the t distribution on 4 degrees of freedom
t_data <- rt(1000000, df=4)
t_std_dev <- sd(t_data)

# c) Mean of the F distribution on 7 numerator and 5 denominator degrees of freedom
f_data <- rf(1000000, df1=7, df2=5)
f_mean <- mean(f_data)

# d) Standard deviation of the F distribution on 7 numerator and 5 denominator degrees of freedom
f_std_dev <- sd(f_data)

# Print results
round(chi_square_std_dev, 2)
round(t_std_dev, 2)
round(f_mean, 2)
round(f_std_dev, 2)

