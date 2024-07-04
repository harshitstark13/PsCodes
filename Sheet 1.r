# 1.1 Function definition
f <- function(x, mu, beta, K) {
  K * exp(-(x - mu)/beta) / (1 + exp(-(x - mu)/beta))^2
}

# Solve for K in each case
mu <- c(0, 0, 2)
beta <- c(2, 4, 2)

K_values <- numeric(length(mu))

for (i in seq_along(mu)) {
  integral <- integrate(f, lower = -10, upper = 10, mu = mu[i], beta = beta[i], K = 1)$value
  K_values[i] <- 1 / integral
}

K_values


# 1.2 Plotting PDFs for each case
x <- seq(-10, 10, length.out = 1000)

pdf_case <- function(x, mu, beta, K) {
  K * exp(-(x - mu)/beta) / (1 + exp(-(x - mu)/beta))^2
}

pdfs <- sapply(1:length(mu), function(i) pdf_case(x, mu[i], beta[i], K_values[i]))

plot(x, pdfs[,1], type = "l", col = "blue", ylim = c(0, 0.3), xlab = "x", ylab = "Density", main = "Probability Density Functions")
lines(x, pdfs[,2], col = "red")
lines(x, pdfs[,3], col = "green")
legend("topright", legend = c(paste("mu =", mu, ", beta =", beta)), col = c("blue", "red", "green"), lty = 1)


# 1.3 Expected value and variance function
E <- function(mu, beta) {
  mu
}

V <- function(mu, beta) {
  (pi^2 * beta^2) / 3
}

# Calculate for each case
E_values <- sapply(mu, E, beta = beta)
V_values <- sapply(mu, V, beta = beta)

E_values
V_values


# 2.1 Function definition
f2 <- function(x, mu, sigma, K) {
  K * x * exp(-(log(x) - mu)^2 / (2 * sigma^2))
}

# Solve for K in each case
mu <- c(-1, -1, 2)
sigma <- c(1, 2, 2)

K_values <- numeric(length(mu))

for (i in seq_along(mu)) {
  integral <- integrate(f2, lower = 0, upper = Inf, mu = mu[i], sigma = sigma[i], K = 1)$value
  K_values[i] <- 1 / integral
}

K_values


# 2.2 Plotting PDFs for each case
x <- seq(0.1, 10, length.out = 1000)

pdf_case2 <- function(x, mu, sigma, K) {
  K * x * exp(-(log(x) - mu)^2 / (2 * sigma^2))
}

pdfs2 <- sapply(1:length(mu), function(i) pdf_case2(x, mu[i], sigma[i], K_values[i]))

plot(x, pdfs2[,1], type = "l", col = "blue", ylim = c(0, 1), xlab = "x", ylab = "Density", main = "Probability Density Functions")
lines(x, pdfs2[,2], col = "red")
lines(x, pdfs2[,3], col = "green")
legend("topright", legend = c(paste("mu =", mu, ", sigma =", sigma)), col = c("blue", "red", "green"), lty = 1)


# 2.3 Calculate for each case
E_values2 <- sapply(mu, function(m) exp(m + sigma^2 / 2))
V_values2 <- sigma^2 * (exp(sigma^2) - 1)

E_values2
V_values2


# 3.1 Load iris dataset
data(iris)

# Compare Sepal.Length and Sepal.Width graphically
plot(iris$Sepal.Length, iris$Sepal.Width, xlab = "Sepal.Length", ylab = "Sepal.Width", main = "Comparison of Sepal.Length and Sepal.Width")

# Histogram of Sepal.Width
hist(iris$Sepal.Width, col = "lightblue", main = "Histogram of Sepal.Width")


# 4.1 Probability X >= 3 for normal distribution with mean 2 and variance 3
1 - pnorm(3, mean = 2, sd = sqrt(3))

# Probability 1 < X < 9 for normal distribution with mean 2 and variance 3
pnorm(9, mean = 2, sd = sqrt(3)) - pnorm(1, mean = 2, sd = sqrt(3))

# Find x such that F(x) = 0.75
qnorm(0.75, mean = 2, sd = sqrt(3))

# Y follows standard normal distribution


# Generate random sample of size 1000 from Y
sample_Y <- rnorm(1000, mean = 0, sd = 1)

# Compute mean and variance
mean_Y <- mean(sample_Y)
var_Y <- var(sample_Y)

mean_Y
var_Y

# Compute variance of X
var_X <- 3

# Verify the relationship
3 * var_Y == var_X


# Load trees dataset
data(trees)

# Box plot comparison
boxplot(trees$Height, trees$Volume, names = c("Height", "Volume"), col = c("lightblue", "lightgreen"), main = "Comparison of Height and Volume")

# Histogram of Height
hist(trees$Height, col = "lightblue", main = "Histogram of Height")


# 6 X follows Binomial distribution

# Probability X = 3 for Binomial distribution
dbinom(3, size = 10, prob = 0.3)

# Probability 2 < X <= 5 for Binomial distribution
pbinom(5, size = 10, prob = 0.3) - pbinom(2, size = 10, prob = 0.3)

# Median value of X for Binomial distribution
qbinom(0.5, size = 10, prob = 0.3)

# Generate random sample of size 1000 from X
sample_X <- rbinom(1000, size = 10, prob = 0.3)

# Compute mean and variance
mean_X <- mean(sample_X)
var_X <- var(sample_X)

mean_X
var_X

# Transform to Y
Y <- 10 - sample_X

# Compute variance of Y
var_Y <- var(Y)

# Verify equality of variances
var_Y == var_X




