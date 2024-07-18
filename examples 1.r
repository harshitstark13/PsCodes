# 1.1 Example data
sizes <- c(20, 30, 50)
labels <- c("Apples", "Oranges", "Bananas")

# Plotting a pie chart
pie(sizes, labels = labels, main = "Pie Chart of Fruit Distribution")


# 1.2 Example data
data <- rnorm(100, mean = 0, sd = 1)

# Plotting a histogram
hist(data, breaks = 10, col = "blue", main = "Histogram of Random Data")


# 1.3 Example data
group <- rep(1:3, each = 50)
values <- rnorm(150)

# Plotting a box plot
boxplot(values ~ group, col = c("red", "blue", "green"),
        main = "Box Plot of Grouped Data")


# Example function to calculate area of a circle
area_circle <- function(radius) {
  return(pi * radius^2)
}

# 2.1 Using the function
# Define the function to calculate area of a circle
area_circle <- function(radius) {
  return(pi * radius^2)
}

# Example usage
radius <- 5
area <- area_circle(radius)
print(paste("Area of circle with radius", radius, "is", area))


# 2.2 Example of integrating a function
f <- function(x) { return(x^2) }

result <- integrate(f, lower = 0, upper = 1)
print(paste("Integration result:", result$value))


# 3.1 Example of normal distribution
data <- rnorm(1000, mean = 0, sd = 1)

# Plotting histogram of normal distribution
hist(data, breaks = 30, col = "lightblue", main = "Histogram of Normal Distribution")


# 3.2Example of Poisson distribution
lambda <- 2
data <- rpois(1000, lambda)

# Plotting histogram of Poisson distribution
hist(data, breaks = 20, col = "lightgreen", main = "Histogram of Poisson Distribution")


