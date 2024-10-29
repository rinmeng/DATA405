# Create the data frame
Fire_Spread <- data.frame(
  R = c(30, 32, 18, 35, 12, 17, 20, 22, 24),  # Rate of spread
  W = c(35, 40, 20, 50, 15, 22, 27, 36, 35)   # Wind speed
)

# Fit the linear model
Fire.lm <- lm(R ~ W, data = Fire_Spread)

# Show the summary of the model
summary(Fire.lm)

# Run the ANOVA
anova(Fire.lm)
