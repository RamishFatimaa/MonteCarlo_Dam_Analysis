# Title: "ALY6050_Module2_Project2"
# Author: "Ramish Fatima"
# Instructor: "Zhi He, Northeastern University"

# Clear the environment and set initial options
cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
options(scipen = 100) # disables scientific notation for entire R

# Custom triangular random number generation function
rtriangle_custom <- function(n, a, b, c) {
  r <- runif(n)
  K <- (c - a) / (b - a)
  M <- (b - a) * (c - a)
  N <- (b - a) * (b - c)
  
  x <- ifelse(r <= K, a + sqrt(r * M), b - sqrt((1 - r) * N))
  return(x)
}

# Define a function to perform Monte Carlo simulation using custom triangular distribution
simulate_benefit_cost_ratio_custom <- function(min_benefits, mode_benefits, max_benefits, min_costs, mode_costs, max_costs, n_simulations) {
  # Generate random samples for benefits and costs using the custom triangular distribution
  benefits <- rowSums(sapply(1:length(min_benefits), function(i) rtriangle_custom(n_simulations, min_benefits[i], max_benefits[i], mode_benefits[i])))
  costs <- rowSums(sapply(1:length(min_costs), function(i) rtriangle_custom(n_simulations, min_costs[i], max_costs[i], mode_costs[i])))
  
  # Calculate the benefit-cost ratios
  benefit_cost_ratios <- benefits / costs
  return(list(benefits = benefits, costs = costs, ratios = benefit_cost_ratios))
}

# Theoretical calculations for triangular distribution
theoretical_mean <- function(a, b, c) {
  (a + b + c) / 3
}

theoretical_sd <- function(a, b, c) {
  sqrt((a^2 + b^2 + c^2 - a*b - a*c - b*c) / 18)
}

# Define benefit and cost parameters for Dam #1
min_benefits_dam1 <- c(2, 3, 0, 1, 2, 6)
mode_benefits_dam1 <- c(4, 4, 1, 3, 4, 7)
max_benefits_dam1 <- c(5, 6, 2, 6, 6, 10)

min_costs_dam1 <- c(2, 3, 1)
mode_costs_dam1 <- c(3, 4, 2)
max_costs_dam1 <- c(4, 5, 5)

# Define benefit and cost parameters for Dam #2
min_benefits_dam2 <- c(3, 2, 3, 2, 0, 3)
mode_benefits_dam2 <- c(4, 3, 4, 3, 1, 4)
max_benefits_dam2 <- c(7, 5, 7, 6, 3, 6)

min_costs_dam2 <- c(2, 2, 1)
mode_costs_dam2 <- c(3, 3, 2)
max_costs_dam2 <- c(4, 5, 4)

# Number of simulations
n_simulations <- 10000

# Perform the simulations
results_dam1 <- simulate_benefit_cost_ratio_custom(min_benefits_dam1, mode_benefits_dam1, max_benefits_dam1, min_costs_dam1, mode_costs_dam1, max_costs_dam1, n_simulations)
results_dam2 <- simulate_benefit_cost_ratio_custom(min_benefits_dam2, mode_benefits_dam2, max_benefits_dam2, min_costs_dam2, mode_costs_dam2, max_costs_dam2, n_simulations)


# Define the number of bins
number_of_bins <- 30

# Function to create frequency distribution
create_frequency_distribution <- function(ratios, dam_name) {
  # Calculate breaks and frequencies
  breaks <- seq(min(ratios), max(ratios), length.out = number_of_bins + 1)
  counts <- hist(ratios, breaks = breaks, plot = FALSE)$counts
  midpoints <- breaks[-length(breaks)] + diff(breaks) / 2
  
  # Create data frame for tabular display
  frequency_distribution <- data.frame(
    ClassLeft = breaks[-length(breaks)],
    ClassRight = breaks[-1],
    ClassMidpoint = midpoints,
    ClassFrequency = counts
  )
  
  # Print and optionally save the frequency distribution table
  print(paste("Frequency Distribution for", dam_name, ":"))
  print(frequency_distribution)
  
  # Write to CSV
  write.csv(frequency_distribution, paste0("frequency_distribution_", dam_name, ".csv"), row.names = FALSE)
}

# Generate and save frequency distributions for both dams
create_frequency_distribution(results_dam1$ratios, "dam1")
create_frequency_distribution(results_dam2$ratios, "dam2")


# Plot the graphical distribution for both α1 and α2
hist(results_dam1$ratios, breaks=50, col=rgb(1,0,0,0.5), xlim=c(min(results_dam1$ratios, results_dam2$ratios), max(results_dam1$ratios, results_dam2$ratios)), main="Benefit-Cost Ratio Simulation", xlab="Benefit-Cost Ratio", freq=FALSE)
hist(results_dam2$ratios, breaks=50, col=rgb(0,0,1,0.5), add=TRUE, freq=FALSE)
legend("topright", legend=c("Dam #1 (α1)", "Dam #2 (α2)"), fill=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), cex=0.5)

# Plot histograms for graphical frequency distributions
hist(results_dam1$ratios, main = "Histogram of Benefit-Cost Ratios for Dam 1 (𝛼1)",
     xlab = "Benefit-Cost Ratio", ylab = "Frequency", col = "blue", breaks = number_of_bins)

hist(results_dam2$ratios, main = "Histogram of Benefit-Cost Ratios for Dam 2 (𝛼2)",
     xlab = "Benefit-Cost Ratio", ylab = "Frequency", col = "green", breaks = number_of_bins)

# Plot the graphical distribution for both α1 and α2
hist(results_dam1$ratios, breaks=50, col=rgb(1,0,0,0.5), xlim=c(min(results_dam1$ratios, results_dam2$ratios), max(results_dam1$ratios, results_dam2$ratios)), main="Benefit-Cost Ratio Simulation", xlab="Benefit-Cost Ratio", freq=FALSE)
hist(results_dam2$ratios, breaks=50, col=rgb(0,0,1,0.5), add=TRUE, freq=FALSE)
legend("topright", legend=c("Dam #1 (α1)", "Dam #2 (α2)"), fill=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), cex=0.5)


# print results 
print(summary(results_dam1$ratios))
print(summary(results_dam2$ratios))


# Calculate observed values
observed_mean_benefits_dam1 <- mean(results_dam1$benefits)
observed_sd_benefits_dam1 <- sd(results_dam1$benefits)
observed_mean_costs_dam1 <- mean(results_dam1$costs)
observed_sd_costs_dam1 <- sd(results_dam1$costs)
observed_mean_ratios_dam1 <- mean(results_dam1$ratios)
observed_sd_ratios_dam1 <- sd(results_dam1$ratios)

observed_mean_benefits_dam2 <- mean(results_dam2$benefits)
observed_sd_benefits_dam2 <- sd(results_dam2$benefits)
observed_mean_costs_dam2 <- mean(results_dam2$costs)
observed_sd_costs_dam2 <- sd(results_dam2$costs)
observed_mean_ratios_dam2 <- mean(results_dam2$ratios)
observed_sd_ratios_dam2 <- sd(results_dam2$ratios)

# Calculate theoretical values
theoretical_mean_benefits_dam1 <- sum(sapply(1:length(min_benefits_dam1), function(i) theoretical_mean(min_benefits_dam1[i], max_benefits_dam1[i], mode_benefits_dam1[i])))
theoretical_sd_benefits_dam1 <- sqrt(sum(sapply(1:length(min_benefits_dam1), function(i) theoretical_sd(min_benefits_dam1[i], max_benefits_dam1[i], mode_benefits_dam1[i])^2)))

theoretical_mean_costs_dam1 <- sum(sapply(1:length(min_costs_dam1), function(i) theoretical_mean(min_costs_dam1[i], max_costs_dam1[i], mode_costs_dam1[i])))
theoretical_sd_costs_dam1 <- sqrt(sum(sapply(1:length(min_costs_dam1), function(i) theoretical_sd(min_costs_dam1[i], max_costs_dam1[i], mode_costs_dam1[i])^2)))

theoretical_mean_benefits_dam2 <- sum(sapply(1:length(min_benefits_dam2), function(i) theoretical_mean(min_benefits_dam2[i], max_benefits_dam2[i], mode_benefits_dam2[i])))
theoretical_sd_benefits_dam2 <- sqrt(sum(sapply(1:length(min_benefits_dam2), function(i) theoretical_sd(min_benefits_dam2[i], max_benefits_dam2[i], mode_benefits_dam2[i])^2)))

theoretical_mean_costs_dam2 <- sum(sapply(1:length(min_costs_dam2), function(i) theoretical_mean(min_costs_dam2[i], max_costs_dam2[i], mode_costs_dam2[i])))
theoretical_sd_costs_dam2 <- sqrt(sum(sapply(1:length(min_costs_dam2), function(i) theoretical_sd(min_costs_dam2[i], max_costs_dam2[i], mode_costs_dam2[i])^2)))

# Output the results
cat("Dam 1:\n")
cat("Observed Mean of Total Benefits:", observed_mean_benefits_dam1, "\n")
cat("Theoretical Mean of Total Benefits:", theoretical_mean_benefits_dam1, "\n")
cat("Observed SD of Total Benefits:", observed_sd_benefits_dam1, "\n")
cat("Theoretical SD of Total Benefits:", theoretical_sd_benefits_dam1, "\n")
cat("Observed Mean of Total Costs:", observed_mean_costs_dam1, "\n")
cat("Theoretical Mean of Total Costs:", theoretical_mean_costs_dam1, "\n")
cat("Observed SD of Total Costs:", observed_sd_costs_dam1, "\n")
cat("Theoretical SD of Total Costs:", theoretical_sd_costs_dam1, "\n")
cat("Observed Mean of Benefit-Cost Ratios:", observed_mean_ratios_dam1, "\n")
cat("Observed SD of Benefit-Cost Ratios:", observed_sd_ratios_dam1, "\n")

cat("\nDam 2:\n")
cat("Observed Mean of Total Benefits:", observed_mean_benefits_dam2, "\n")
cat("Theoretical Mean of Total Benefits:", theoretical_mean_benefits_dam2, "\n")
cat("Observed SD of Total Benefits:", observed_sd_benefits_dam2, "\n")
cat("Theoretical SD of Total Benefits:", theoretical_sd_benefits_dam2, "\n")
cat("Observed Mean of Total Costs:", observed_mean_costs_dam2, "\n")
cat("Theoretical Mean of Total Costs:", theoretical_mean_costs_dam2, "\n")
cat("Observed SD of Total Costs:", observed_sd_costs_dam2, "\n")
cat("Theoretical SD of Total Costs:", theoretical_sd_costs_dam2, "\n")
cat("Observed Mean of Benefit-Cost Ratios:", observed_mean_ratios_dam2, "\n")
cat("Observed SD of Benefit-Cost Ratios:", observed_sd_ratios_dam2, "\n")

# Create a function to generate tabular distributions with class left, class right, class midpoint, and class frequency
generate_tabular_distribution_detailed <- function(data, title) {
  # Create a histogram and capture the counts for the tabular distribution
  hist_data <- hist(data, breaks=50, plot=FALSE)
  class_left <- hist_data$breaks[-length(hist_data$breaks)]
  class_right <- hist_data$breaks[-1]
  class_midpoint <- (class_left + class_right) / 2
  class_frequency <- hist_data$counts
  
  # Create the tabular distribution
  tabular_distribution <- data.frame(
    Class_Left = class_left,
    Class_Right = class_right,
    Class_Midpoint = class_midpoint,
    Class_Frequency = class_frequency
  )
  
  # Print the tabular distribution
  cat("\nTabular Distribution for", title, ":\n")
  print(tabular_distribution)
}

# Generate detailed tabular distributions for α1
generate_tabular_distribution_detailed(results_dam1$ratios, "Dam #1 (α1)")

# Generate detailed tabular distributions for α2
generate_tabular_distribution_detailed(results_dam2$ratios, "Dam #2 (α2)")

# Plot the graphical distribution for both α1 and α2
hist(results_dam1$ratios, breaks=50, col=rgb(1,0,0,0.5), xlim=c(min(results_dam1$ratios, results_dam2$ratios), max(results_dam1$ratios, results_dam2$ratios)), main="Benefit-Cost Ratio Simulation", xlab="Benefit-Cost Ratio", freq=FALSE)
hist(results_dam2$ratios, breaks=50, col=rgb(0,0,1,0.5), add=TRUE, freq=FALSE)
legend("topright", legend=c("Dam #1 (α1)", "Dam #2 (α2)"), fill=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), cex=0.5)


# Plot the graphical distribution for α1 separately
hist(results_dam1$ratios, breaks=50, col=rgb(1,0,0,0.5), main="Benefit-Cost Ratio Simulation for Dam #1", xlab="Benefit-Cost Ratio", freq=FALSE)
legend("topright", legend="Dam #1 (α1)", fill=rgb(1,0,0,0.5), cex=0.5)

# Plot the graphical distribution for α2 separately
hist(results_dam2$ratios, breaks=50, col=rgb(0,0,1,0.5), main="Benefit-Cost Ratio Simulation for Dam #2", xlab="Benefit-Cost Ratio", freq=FALSE)
legend("topright", legend="Dam #2 (α2)", fill=rgb(0,0,1,0.5), cex=0.5)


#PArt2:
# Load necessary library
library(MASS)

# Plot the histogram of the benefit-cost ratio for Dam #1
hist(results_dam1$ratios, breaks=50, col=rgb(1,0,0,0.5), main="Benefit-Cost Ratio Simulation for Dam #1", xlab="Benefit-Cost Ratio", freq=FALSE)

# Fit a normal distribution to the benefit-cost ratio data
fit <- fitdistr(results_dam1$ratios, "normal")

# Generate the expected frequencies
breaks <- hist(results_dam1$ratios, breaks=50, plot=FALSE)$breaks
observed_frequencies <- hist(results_dam1$ratios, breaks=breaks, plot=FALSE)$counts
expected_frequencies <- diff(pnorm(breaks, fit$estimate[1], fit$estimate[2])) * length(results_dam1$ratios)

# Perform the Chi-squared Goodness-of-fit test
chi_sq_test <- chisq.test(observed_frequencies, p=expected_frequencies, rescale.p=TRUE)

# Output the test statistic and p-value
cat("Chi-squared test statistic:", chi_sq_test$statistic, "\n")
cat("P-value of the test:", chi_sq_test$p.value, "\n")

# Interpretation
if (chi_sq_test$p.value < 0.05) {
  cat("The p-value is less than the significance level (0.05). We reject the null hypothesis, indicating that the normal distribution is not a good fit for the observed data.\n")
} else {
  cat("The p-value is greater than the significance level (0.05). We fail to reject the null hypothesis, suggesting that the normal distribution is a good fit for the observed data.\n")
}


chi_sq_test_selected <- chisq.test(observed_frequencies, p=expected_frequencies, rescale.p=TRUE)

# Output the test results
cat("Chi-squared test statistic for selected distribution:", chi_sq_test_selected$statistic, "\n")
cat("P-value of the test for selected distribution:", chi_sq_test_selected$p.value, "\n")


#Part3
# Load necessary library
library(e1071) # for skewness and kurtosis

# Calculate Minimum and Maximum
min_alpha1 <- min(results_dam1$ratios)
max_alpha1 <- max(results_dam1$ratios)

min_alpha2 <- min(results_dam2$ratios)
max_alpha2 <- max(results_dam2$ratios)

# Calculate Skewness and Kurtosis
skewness_alpha1 <- skewness(results_dam1$ratios)
kurtosis_alpha1 <- kurtosis(results_dam1$ratios)

skewness_alpha2 <- skewness(results_dam2$ratios)
kurtosis_alpha2 <- kurtosis(results_dam2$ratios)

# Calculate probabilities for different thresholds
P_alpha1_greater_than_3.00 <- mean(results_dam1$ratios > 3.00)
P_alpha1_greater_than_2.75 <- mean(results_dam1$ratios > 2.75)
P_alpha1_greater_than_2.50 <- mean(results_dam1$ratios > 2.50)
P_alpha1_greater_than_2.00 <- mean(results_dam1$ratios > 2.00)
P_alpha1_greater_than_1.50 <- mean(results_dam1$ratios > 1.50)

P_alpha2_greater_than_3.00 <- mean(results_dam2$ratios > 3.00)
P_alpha2_greater_than_2.75 <- mean(results_dam2$ratios > 2.75)
P_alpha2_greater_than_2.50 <- mean(results_dam2$ratios > 2.50)
P_alpha2_greater_than_2.00 <- mean(results_dam2$ratios > 2.00)
P_alpha2_greater_than_1.50 <- mean(results_dam2$ratios > 1.50)

# Calculate the probability P(alpha1 < alpha2)
P_alpha1_less_than_alpha2 <- mean(results_dam1$ratios < results_dam2$ratios)

# Output all values to fill the table
cat("Minimum α1:", min_alpha1, "\n")
cat("Maximum α1:", max_alpha1, "\n")
cat("Mean α1:", observed_mean_ratios_dam1, "\n")
cat("Standard Deviation α1:", observed_sd_ratios_dam1, "\n")
cat("Skewness α1:", skewness_alpha1, "\n")
cat("Kurtosis α1:", kurtosis_alpha1, "\n")
cat("P(α1 > 3.00):", P_alpha1_greater_than_3.00, "\n")
cat("P(α1 > 2.75):", P_alpha1_greater_than_2.75, "\n")
cat("P(α1 > 2.50):", P_alpha1_greater_than_2.50, "\n")
cat("P(α1 > 2.00):", P_alpha1_greater_than_2.00, "\n")
cat("P(α1 > 1.50):", P_alpha1_greater_than_1.50, "\n")

cat("Minimum α2:", min_alpha2, "\n")
cat("Maximum α2:", max_alpha2, "\n")
cat("Mean α2:", observed_mean_ratios_dam2, "\n")
cat("Standard Deviation α2:", observed_sd_ratios_dam2, "\n")
cat("Skewness α2:", skewness_alpha2, "\n")
cat("Kurtosis α2:", kurtosis_alpha2, "\n")
cat("P(α2 > 3.00):", P_alpha2_greater_than_3.00, "\n")
cat("P(α2 > 2.75):", P_alpha2_greater_than_2.75, "\n")
cat("P(α2 > 2.50):", P_alpha2_greater_than_2.50, "\n")
cat("P(α2 > 2.00):", P_alpha2_greater_than_2.00, "\n")
cat("P(α2 > 1.50):", P_alpha2_greater_than_1.50, "\n")

cat("P(α1 < α2):", P_alpha1_less_than_alpha2, "\n")

