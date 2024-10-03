sample <- c(27, 48, 72, 101, 98, 37, 22, 55, 41, 79, 58, 44, 61)
sd(sample)

boxplot(sample)
qqnorm(sample)
qqline(sample, col='red')

t.test(sample, mu=50, conf.level = 0.99)

papergroup <- factor(rep(1:5, 7))
distances <- rnorm(35, mean = 9, sd = 3)

# 7 simulation, with 5 different airplanes

# there are no difference because all the distances has a mean of 9.

boxplot(distances ~ papergroup, main='Paper Airplane Experiment Data' , las=2, xlab='paper', ylab ='distance')

anova(lm(distances ~ papergroup))

library(MPV)

y <- table.b3$y
x7 <- table.b3$x7
x10 <- table.b3$x10

b3.lm <- lm(y ~ x7 + x10)
b3.sd <- summary(b3.lm)$sigma

# y = 30.37 + 2.04 x7 + (-0.004) x10 + 3.158

# y = 30.37 + 2.04 (4) + (-0.004)(5000) 

predict(b3.lm, newdata = data.frame(x7=4, x10=5000), interval='confidence')

TukeySmooth <- function(x, y, x.min, x.max, window) {
  xpoints <- seq(x.min, x.max, len=401)
  ymedians <- numeric(401)
  for (i in 1:length(xpoints)) {
    indices <- which(abs(x - xpoints[i]) < window)
    if (length(indices) < 1){
      stop("zzz")
    }else{
      ymedians[i] <- median(y[indices])
    }
  }
  data.frame(x = xpoints, y = ymedians)
}

tukey <- TukeySmooth(women$height,women$weight, x.min=min(women$height), x.max= max(women$height), window=5)

plot(weight ~ height, data = women)
lines(tukey)

f <- function(x){
  return (x/2 - 24.5/x)
}

curve (f(x), 0.1, 10)
abline(0,1)

x = 25 
for (i in 1:10){
  x <- f(x)
  print
}


myrbinom <- function(N, n, p){
  X <- numeric(N)
  for (j in 1:N){
    X[j] = sum(runif(n) <= p)
  }
}


x_k <- function(x_k1, a,b,m){
  val <- ( (a + (b * x_k1)) %% (2^m-1))
  print(paste("x_k =", val, "u_k =", u_k(val, m)))
}

u_k <- function(xk, m) {
  return (xk / 2^m)
}

x_k(28,4,17,6)
x_k(39,4,17,6)
x_k(37,4,17,6)

n <- 2
CDF <- c(.6, .6, .9, 1)
U <- 0.35
X <- numeric(n)
for (x in 0:2) {
  X[U > CDF[x+1]] <- x + 1
}
X

x_k(127, 2^(2^5-1)-1 ,1000000001)


n=12
uj <- runif(n, -1, 1)
uj
uj.112 <- sum(uj)
uj <- c(uj,runif(n,-1,1))
uj
uj.1324 = 0
for(i in 13:24){
  uj.1324 = uj.1324 + uj[i]
}
uj.112
uj.1324

X <- uj.112 + uj.1324

vect.X <- c()

for (i in 1:100){
  n=12
  uj <- runif(n, -1, 1)
  uj.112 <- sum(uj)
  uj <- c(uj,runif(n,-1,1))
  uj.1324 = 0
  for(i in 13:24){
    uj.1324 = uj.1324 + uj[i]
  }
  
  X <- uj.112^2 + uj.1324^2
  vect.X <- c(vect.X, X)
}
summary(vect.X)


badrandom <- function (n,a,b,m){
  val = ((a+(b*28)) %% ((2^m) - 1))
  u_k = val / 2^m
  print(paste("x_k =", val, "u_k =", u_k))
  
}
badrandom(2,4,17,6)



n <- 2
CDF <- c(.3, .9, .9, 1)
U <- 0.5
X <- 1
for (x in 0:2) {
  X[U > CDF[x+1]] <- x + 1
}
X

