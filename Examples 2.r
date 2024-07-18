# 1. Regression Line
x<-c(1,2,3,4,5)
y<-c(2,3,5,7,11)
data<-data.frame(x,y)
model<-lm(x~y,data = data)
plot(x,y,main="Regression Line",xlab="Value of x",ylab="Value of y",pch="19")
abline(model, col="red")
predicted_length<-predict(model,data.frame(x=350))
predicted_length

# 2. T-test
# Generate some sample data
set.seed(42)
group1 <- rnorm(30, mean = 5)
group2 <- rnorm(30, mean = 5.5)

# Perform t-test
t_test_result <- t.test(group1, group2)

# Print the result
print(t_test_result)


# 3. Z-test
# Z-test using the BSDA package
if (!require(BSDA)) install.packages("BSDA", dependencies = TRUE)
library(BSDA)

# Generate some sample data
set.seed(42)
x <- rnorm(50, mean = 5)
mu <- 5.5 # Population mean

# Perform z-test
z_test_result <- z.test(x, mu = mu, sigma.x = sd(x))

# Print the result
print(z_test_result)

# 4. F-test
# Generate some sample data
set.seed(42)
group1 <- rnorm(30, mean = 5, sd = 1)
group2 <- rnorm(30, mean = 5, sd = 2)

# Perform F-test
f_test_result <- var.test(group1, group2)

# Print the result
print(f_test_result)


# 5. Paired T-test
# Generate some sample data
set.seed(42)
before <- rnorm(30, mean = 5)
after <- before + rnorm(30, mean = 0.5)

# Perform paired t-test
paired_t_test_result <- t.test(before, after, paired = TRUE)

# Print the result
print(paired_t_test_result)















