# 1. Comparing Average SAT Scores
# SAT scores
uq_scores <- c(1340, 1500, 1430, 1440, 1380, 1470, 1290)
unsw_scores <- c(1540, 1480, 1390, 1450, 1440, 1350, 1520, 1400, 1600)

# Perform t-test
t_test_result <- t.test(uq_scores, unsw_scores, var.equal = TRUE)

# Print the result
print(t_test_result)


# 2. Scatter Plot and Regression Lines for Sales and Purchases
# Data
sales <- c(91, 97, 108, 121, 67, 124, 51, 73, 111, 57)
purchases <- c(71, 75, 69, 97, 70, 91, 39, 61, 80, 47)

# Plot scatter diagram
plot(purchases, sales, main = "Scatter plot of Sales vs Purchases", xlab = "Purchases (lakhs)", ylab = "Sales (lakhs)", pch = 19, col = "blue")

# Regression: Sales ~ Purchases
reg_sales_on_purchases <- lm(sales ~ purchases)
abline(reg_sales_on_purchases, col = "red")

# Regression: Purchases ~ Sales
reg_purchases_on_sales <- lm(purchases ~ sales)
abline(reg_purchases_on_sales, col = "green")

# Summary of models
summary(reg_sales_on_purchases)
summary(reg_purchases_on_sales)

# Estimate sales if purchases are 95 lakhs
new_purchase <- data.frame(purchases = 95)
predicted_sales <- predict(reg_sales_on_purchases, new_purchase)
print(predicted_sales)


# 3. Variance in Battery Life between Brands A and B
# LBC data
brand_a <- c(3.2, 3.4, 2.8, 3.0, 3.0, 3.0, 2.8, 2.9, 3.0, 3.0)
brand_b <- c(3.0, 3.5, 2.9, 3.1, 2.3, 2.0, 3.0, 2.9, 3.0, 4.1)

# Perform F-test
f_test_result <- var.test(brand_a, brand_b)

# Print the result
print(f_test_result)


# 4. Effectiveness of a Module on Student Scores
# Scores
pre_module <- c(18, 21, 16, 22, 19, 24, 17, 21, 23, 18, 14, 16, 16, 19, 18, 20, 12, 22, 15, 17)
post_module <- c(22, 25, 17, 24, 16, 29, 20, 23, 19, 20, 15, 15, 18, 26, 18, 24, 18, 25, 19, 16)

# Perform paired t-test
paired_t_test_result <- t.test(pre_module, post_module, paired = TRUE)

# Print the result
print(paired_t_test_result)


