# 1. Basic Probability - Simulating a Coin Toss

set.seed(123)  # Set seed for reproducibility
coin_toss <- sample(c("Heads", "Tails"), size = 10, replace = TRUE, prob = c(0.5, 0.5))
print(coin_toss)

:-Simulates 10 coin tosses with equal probability for Heads and Tails.

