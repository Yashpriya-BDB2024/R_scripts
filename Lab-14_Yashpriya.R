######## LAB-14 (DATE: 24-4-25) ########

######## Section-1 (Error bars, covariance and correlation) ########
# An error is a small line segment around each point (X,Y) representing the uncertainity in the measured value Y.

### Q-1.1 (Error bars on bar plots)

means <- c(20.34, 19.49, 25.68)
stderr <- c(0.83, 1.51, 1.39)
mean_labels <- c('A', 'B', 'C')

# Make a bar plot with the following features: the three means should be labeled as ‘A’, ‘B’ and ‘C’, grey filled bars, plot title as ’Errors on bar plot’.
bar_pos <- barplot(means, names.arg = mean_labels, col='grey', ylim=c(0, max(means+stderr)+2), main="Errors on bar plot", ylab="Mean Values")

# Use the arrows() function to plot the error bars. 
# arrows(x0, y0, x1, y1, length, angle, code = 2, col, lty, lwd ) where the arrow is drawn from (x0,y0) to (x1,y1), with given length, angle between the arrow head and arrow
# head, and code to specify whether arrow heads at one end or both.
arrows(x0 = bar_pos, y0 = means + stderr, x1 = bar_pos, y1 = means - stderr, angle = 90, code = 3, length = 0.06, col='red')

# Interpretation:
# Group-A: Moderate mean, smallest standard error, so most precise with less uncertainty.
# Group-B: Lowest mean, largest standard error, so it is least precise with the most uncertainty in the mean.
# Group-C: Highest mean, standard error is bit lower than group-B but higher than group-A (moderate uncertainty).
# Hence, group-A is the most reliable and group-B is the least reliable.

### Q-1.2 (Error bars on (x,y) plots)

x <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
y <- c(5, 9, 13, 17, 20, 24, 26, 27, 28, 27)
errors <- c(0.5, 0.9, 1.4, 1.5, 2.0, 2.2, 2.3, 2.5, 2.9, 3.0)

# Plot (x,y) with points and xlabel ‘concentration’ and ylabel ‘optical activity’ and plot title as ‘Error bars on data points”.
plot(x, y, pch = 16,    # solid circle point
  col = "blue", main = "Error bars on data points", xlab = "concentration", ylab = "optical activity",     
  ylim = c(min(y - errors)-1, max(y + errors)+1)   # extend y-limits for error bars
)

# Use the arrows() function to plot the error bars, only difference is in the first 4 arguments: arrows(x,y+errors,x,y-errors,.....).
arrows(x0 = x, y0 = y + errors, x1 = x, y1 = y - errors, angle = 90, code = 3, length = 0.06, col='red')

# Interpretation:
# It shows a positive relationship b/w concentration & optical activity. As the concentration increases, the optical activity generally increases.
# Red vertical error bars represent the variability (standard errors) in the optical activity measurements.
# Error margins widen slightly at higher concentrations, indicating more variability in those measurements.

### Q-1.3 (Covariance and Pearson’s correlation coefficient)

# For a univariate sample, the functions cov() and cor() return a number, for multivariate samples, these functions returns a matrix.
x <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
y <- c(95, 220, 279, 424, 499, 540, 720, 880, 950, 1200)

# Covariance measures how two variables change together, indicating the direction of their relationship (positive or negative).
# Formula: Cov(x, y) = (1/n)*summation((xi-X_bar)*(yi-Y_bar)
cat("Covariance between", x, "and", y, ":", cov(x, y), "\n")    # Output: 10549.44

# Pearson's correlation coefficient quantifies the strength and direction of a linear relationship between two variables.
# Formula: r = Cov(x, y) / (sigma_x * sigma_y)
cat("Correlation between", x, "and", y, ":", cor(x, y), "\n")   # Output: 0.9882467
cat("Correlation matrix of 'longley' dataset:\n", cor(longley))   # Built-in multivariate dataset

######## Section-2 (One sample tests) ########

# Q-2.1 (One sample Z test)

# Q-2.1.a: Write a function to perform one sample Z test, where given a data set x that is randomly drawn from a Gaussian distribution of population mean μ and standard
# deviation σ, the function returns the conclusions of the test along with the computed statistic values. The function should be defined as one_sample_Ztest(x,sigma,muzero, alpha,null)
# where x is the data vector, sigma is the population standard deviation, muzero is the population mean for comparison, alpha is the significance level (usually 0.05) and 
# null is a string indicating type of null hypothesis. The possible values of null should be equal, less_than_or_equal or more_than_or_equal. 
# The function should return a vector with two numbers and the conclusion: p-value and the Z-value and the statistical conclusion.

generate_gaussian_data <- function(n, mu, sigma) {  # n: no. of data points to generate
    if (n<=0 || !is.numeric(n)) {
      stop("n must be a positive no.")
    }
    if (!is.numeric(mu) || !is.numeric(sigma) || sigma<=0) {
      stop("mu must be numeric & sigma must be a positive no.")
    }
    x <- rnorm(n, mean=mu, sd=sigma)
    return(x)
  }

one_sample_Ztest <- function(x, sigma, muzero, alpha, null) {
  sample_mean <- mean(x)
  sample_size <- length(x)
  z_value <- (sample_mean - muzero) / (sigma / sqrt(sample_size))   # Z-statistic formula: (x_bar - mu0) / (sigma / sqrt(n))
  # Calculate p-value based on null hypothesis -
  if (null == 'equal') {    # Sample mean = Population mean
    p_value <- 2 * (1-pnorm(abs(z_value)))   # Two-tailed test
  } else if (null == "less than or equal") {    # Sample mean is less than or equal to the population mean.
    p_value <- pnorm(z_value)   # Left-tailed test ; p-value is the probability to the left of the Z-value on the Z-distribution.
  } else if (null == "more than or equal") {
    p_value <- 1-pnorm(z_value)   # Right-tailed test ; p-value is the probability to the right of the Z-value on the Z-distribution.
  } else {
    stop("Invalid null hypothesis type.")
  }
  result <- c(z_value=z_value, p_value=p_value)
  # Make decision based on p-value and alpha -
  if (p_value < alpha) {
    conclusion <- "Reject null hypothesis (H0)"
  } else {
    conclusion <- "Fail to reject null hypothesis"
  }
  cat("Statistical Conclusion:", conclusion, "\n")
  return(result)
}
x <- generate_gaussian_data(n=40, mu=125, sigma=15)   # n>30 (for Z-test)
one_sample_Ztest(x, sigma=15, muzero=120, alpha=0.05, null='equal')

# Q-2.1.b: 
x1 <- c(141.5, 152.3, 121.2, 123.0, 151.6, 124.8, 138.9, 137.4, 145.6, 135.6, 135.4, 121.5)
one_sample_Ztest(x1, sigma=14.5, muzero=124.6, alpha=0.05, null='equal')   # null hypo.: mu=mu0

# Q-2.2 (One sample t-test)

# Q-2.2.a: Write a function to perform a one sample t-test given a data set x that is randomly drawn from a Gaussian distribution of population mean μ and standard deviation σ.
# The function should return the conclusions of the test along with the statistic values. Function should be defined as one_sample_t_test(x, muzero, alpha, null),
# where the arguments have the same meaning as above.
one_sample_t_test <- function(x, muzero, alpha, null) {
  sample_mean <- mean(x)
  sample_sd <- sd(x)  # Population S.D. is unknown, so sample S.D. is used as an estimate of the poulation S.D. 
  sample_size <- length(x)
  t_value <- (sample_mean - muzero) / (sample_sd / sqrt(sample_size))
  df <- sample_size - 1   # Degrees of freedom
  # Calculate p-value based on null hypothesis -
  if (null == 'equal') {    # Two-tailed test
    p_value <- 2 * (1 - pt(abs(t_value), df))   # pt() function is used to calculate the CDF for the t-distribution.
  } else if (null == "less_than_or_equal") {    # Left-tailed test
    p_value <- pt(t_value, df)
  } else if (null == "more_than_or_equal") {    # Right-tailed test
    p_value <- 1 - pt(t_value, df)
  } else {
    stop("Invalid null hypothesis type.")
  }
  result <- c(t_value = t_value, p_value = p_value)
  # Decision based on p-value and alpha
  if (p_value < alpha) {
    conclusion <- "Reject null hypothesis (H0)"
  } else {
    conclusion <- "Fail to reject null hypothesis"
  }
  cat("Statistical Conclusion:", conclusion, "\n")
  return(result)
}
x2 <- generate_gaussian_data(n = 15, mu = 125, sigma = 15)   # n < 30 for t-test
one_sample_t_test(x2, muzero = 120, alpha = 0.05, null = 'equal')

# Q-2.2.b:
x3 <- c(96.0, 104.0, 99.1, 97.6, 99.4, 92.8, 105.6, 97.2, 96.8, 92.1, 100.6, 101.5, 100.7, 97.3, 99.6, 105.9)
one_sample_t_test(x3, muzero = 100, alpha = 0.05, null = 'equal')    # H0: mu=mu0