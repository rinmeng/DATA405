x <- c(27,48,72,101,98,37,22,55,41,79,58,44,61)
sd(x)
boxplot(x)
qqnorm(x)
qqline(x)
t.test(x, mu=50, conf.level = 0.99)


papergroup <- factor(rep(1:5, 7))
distances <- rnorm(35, mean = 9, sd = 3)


boxplot(distances ~ papergroup, xlab="paper", ylab = "distance", main= "Paper Airplane Experiment Data", las=2)

anova(lm(distances ~ papergroup))

# The p-value is .3316 which is not small. Therefore, there is no evidence that the mean
# distance traveled by the simulated paper airplanes are different for the different groups


b3.lm<- lm(y ~ x7 + x10, data=table.b3)
coef(b3.lm)

summary(b3.lm)

#The fitted model is
# y = 30.375 + 2.048x7 − 0.005x10
# where the error has mean 0 and an estimated standard deviation of 3.16.

predict(b3.lm, newdata=data.frame(x7 = 4, x10 = 5000), interval ="confidence")
# The 95% confidence interval for gas mileage for such cars is (10.8, 18.9).

# This is very similar to the smoother() function described in the textbook, with median in place
# of mean.
TukeySmooth <- function(x, y, x.min, x.max, window=1) {
  xpoints <- seq(x.min, x.max, len=401)
  ymedians<- numeric(401)
  for (i in 1:length(xpoints)) {
    indices <- which(abs(x - xpoints[i]) < window)
    if (length(indices) < 1) {
      stop("Your choice of window width is too small.")
    } else {
      ymedians[i] <- median(y[indices])
    }
  }
  data.frame(x = xpoints, y = ymedians)
}


women.TS <- TukeySmooth(women$height, women$weight, x.min = 58, x.max=72, window=5)
plot(weight ~ height, data = women)
lines(women.TS)

f <- function(x) x/2 - 24.5/x
curve(f(x), 0.1, 10)
abline(0,1)
# Since the function and the line do not intersect, the function does not have a fixed point
x <- 25
for (i in 1:10) {
  x <- f(x)
  print(x)
}
