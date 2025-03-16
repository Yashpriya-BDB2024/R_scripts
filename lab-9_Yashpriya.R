### LAB-9 (DATE: 7.3.25)

# Q-1 (Plot the point (2,4) with square point character type and magenta color.)
plot(x=2, y=4, pch=0, col="magenta")

# Q-2 (Create a seqence of function values corresponding to sin(x) and cos(x) function from −pi to pi and plot the two functions on the same graph with appropriate titles, axes labels,
# blue color for the sine curve, red color for the cosine curve, star point character type for sine, cross type point character for cosine overlaid by lines joining the points.)
xval=seq(-pi, pi, 0.1)
yval1=sin(xval)
yval2=cos(xval)
plot(xval, yval1, col="blue", pch=8, main="Sine vs Cosine Function", xlab="x-values", ylab="y-values")
points(xval, yval2, col="red", pch=4, type="o", main="Sine vs Cosine Function", xlab="x-values", ylab="y-values")

# Q-3 (Reproduce the bar graph type of plot in Fig. 4.2.1 in the Biostatistics book by Daniel using the appropriate settings for the appearance.)
xvalues=c(1,2,3,4,5,6,7,8)
yvalues=c(0.2088, 0.1582, 0.1313, 0.1313, 0.1953, 0.1246, 0.0135, 0.0370)
barplt <- barplot(yvalues, names.arg = xvalues, col = "lightgray", border = "black", width = 0.8, space = 0.5, ylim = c(0, 0.25),
                  xlab = expression(italic(x)~"(number of assistance programs)"), ylab = "Probability", las=1)    # las=1 : makes y-axis labels horizontal
abline(h = 0, col = "black", lwd = 0.05)     # Horizontal line at the x-axis bottom
axis(1, at = barplt, labels = xvalues, tck = 0.02)    # Upward ticks at mid-bars
axis(2, las = 1, tck = 0.02)    # Rightward ticks at y-axis

# Q-4 (Make a 2x3 grid of 6 graphs with the following specifications and their respective titles.)
par(mfrow=c(2,3))
# Q-4.1 (x vs cos(x) with red color and lines)
x = seq(-pi, pi, 0.1)
y = cos(x)
plot(x, y, col="red", type="l", main = "x vs cos(x)")

# Q-4.2 (x vs ((x^2)/3) + 4.2 with violet color, points and lines, linewidth 2 and linetype 1)
x = seq(-5, 5, 0.3)
y = ((x**2)/3)+4.2
plot(x, y, col="violet", type="b", lwd=2, lty=1, main="x vs ((x^2)/3)")

# Q-4.3 & 4.4 (Histogram plot of binomial distribution for 12 trials & p=0.3, and 12 trials & p=0.8)
factorial_custom <- function(num) {
  if (num == 0) return(1)
  prod(1:num)
}
binomial_coeff <- function(n, m) {
  factorial_custom(n) / (factorial_custom(m) * factorial_custom(n - m))
}
binomial_pmf_custom <- function(n, p) {
  x_vals <- 0:n    # Possible successes
  probabilities <- sapply(x_vals, function(m) {
    binomial_coeff(n, m) * (p^m) * ((1 - p)^(n - m))
  })
barplot(probabilities, names.arg = x_vals, col = "lightblue", border = "black",
        main = paste("Manual Binomial Histogram\n(n =", n, ", p =", p, ")"),
        xlab = "Number of Successes", ylab = "Probability", ylim = c(0, max(probabilities) * 1.2))
}
# Comparison with built-in PMF function
binomial_pmf_builtin <- function(n, p) {
  x_vals <- 0:n    
  probabilities <- dbinom(x_vals, size = n, prob = p)  
  
  barplot(probabilities, names.arg = x_vals, col = "orange", border = "black",
          main = paste("Built-in Binomial Histogram\n(n =", n, ", p =", p, ")"),
          xlab = "Number of Successes", ylab = "Probability", ylim = c(0, max(probabilities) * 1.2))
}
par(mfrow = c(1, 2))
binomial_pmf_custom(12, 0.3)
binomial_pmf_builtin(12, 0.3)
binomial_pmf_custom(12, 0.8)
binomial_pmf_builtin(12, 0.8)

# Q-4.5 (Histogram plot using type=’h’ option in plot() function for x sequence of 1 to 10 with 0.5 spacing and y function 50x/(x + 2) with colors blue and orange alternating between the points.)
x=seq(1, 10, 0.5)
y=50*x/(x+2)
colors <- rep(c("blue", "orange"), length.out = length(x))    # colors blue and orange alternating b/w the points
plot(x, y, type='h', col=colors, main="Histogram-like plot", xlab="x-values", ylab="y-values (y=50x/(x+2))")

# Q-4.6 (x vs log(x) with orange color and ‘step’ line type.)
x=seq(1, 10, 0.5)
y=log(x)
plot(x, y, col="orange", type='s', lwd=2,main="Plot of x vs log(x)", xlab="x values", ylab="log(x) values")

# Q-5 (Write a script to recreate the plot in the slide with the plot title 'This is a graph'.)
x <- c(1, 3, 5, 7, 9, 11)
y <- c(2, 7, 5, 10, 8, 10)  
labels <- c(1, 3, 5, 7, 9, 11)  
plot(x, y, type = "n", xlab = "Time", ylab = "Performance", main = "This is a graph", col.main = "blue", font.main = 2)
lines(x, y, lty = 2, col = "lightpink", lwd = 3)
points(x, y, col = "lightpink", pch = 16, cex = 1.5)
text(x+0.3, y-0.3, labels = labels, col = "red", cex = 1.5, pos = 3)
legend(x=1,y=10, legend = "Per curve", lty = 2, col = "lightpink", bty = "o", box.lwd = 2)

# Q-6 (Plot a histogram representation of hypergeometric distribution with N=500, K=50 and n=30)
# Method-1 
# Define the hypergeometric probability function
hypergeo_pmf <- function(k, N, K, n) {
  choose(K, k) * choose(N - K, n - k) / choose(N, n)
}
N <- 500  # Total population size
K <- 50   # Number of successes in population
n <- 30   # Sample size
x_values <- 0:min(K, n)  # Possible values of k (successes in sample)
probs <- sapply(x_values, hypergeo_pmf, N=N, K=K, n=n)  # Compute probabilities
barplot(probs, names.arg = x_values, col = "skyblue", border = "black",
        main = "Hypergeometric Distribution (N=500, K=50, n=30)",
        xlab = "Number of Successes", ylab = "Probability")
# Method-2 (for comparison)
N <- 500  # Total population size
K <- 50   # Number of successes in population
n <- 30   # Sample size
# Generate probabilities for different values
x_values <- 0:min(K, n)  # Possible no. of successes in sample
probs <- dhyper(x_values, K, N-K, n)
barplot(probs, names.arg = x_values, col = "skyblue", border = "black",
        main = "Hypergeometric Distribution (N=500, K=50, n=30)",
        xlab = "Number of Successes", ylab = "Probability")

# Q-7 (Write few lines of script to explore what happens to a hypergeomtric distribution when n is increased and gets closer to N. Show that it approaches the binomial distribution by
# plotting histograms of both hypergeometric and binomial on the same plot. Use a 3x3 grid of 9 graphs to show the convergence of hypergeometric to binomial distribution.)
hypergeo_pmf <- function(k, N, K, n) {
  choose(K, k) * choose(N - K, n - k) / choose(N, n)
}
binom_pmf <- function(k, n, p) {
  choose(n, k) * (p^k) * ((1 - p)^(n - k))
}
N <- 50    # Total population size
K <- 25    # Number of successes in the population
n_values <- seq(5, 45, length.out = 9)  # Increasing sample sizes
par(mfrow = c(3, 3))   # Set up a 3x3 plot grid
for (n in n_values) {
  x_values <- 0:min(K, n)  # Possible successes in sample (k)
  hyper_probs <- sapply(x_values, hypergeo_pmf, N=N, K=K, n=n)   # Compute hypergeometric probabilities
  binom_probs <- sapply(x_values, binom_pmf, n=n, p=K/N)    # Compute binomial probabilities
  barplot(hyper_probs, names.arg = x_values, col = "skyblue", border = "black",main = paste("n =", n), xlab = "Successes", ylab = "Probability")
  lines(x_values, binom_probs, col = "red", pch = 16)    # Overlay binomial as red lines
}

# Q-8 (On the same plot, draw 3 Poisson distributions with λ values of 3,20,45 (Code the probability distribution function).)
poisson_pmf <- function(k, lambda) {
  (lambda^k * exp(-lambda)) / factorial(k)
}
lambda_values <- c(3, 20, 45)
x_values <- 0:80   # Define x-axis range (enough to cover all three distributions)
# Compute probabilities for each λ
poisson_probs <- lapply(lambda_values, function(lambda) {
  sapply(x_values, poisson_pmf, lambda=lambda)
})
plot(x_values, poisson_probs[[1]], type="h", col="blue", lwd=2, 
     ylim=c(0, max(unlist(poisson_probs))), 
     main="Poisson Distributions for λ = 3, 20, 45",
     xlab="Number of Events", ylab="Probability")
lines(x_values, poisson_probs[[2]], type="h", col="red", lwd=2)
lines(x_values, poisson_probs[[3]], type="h", col="green", lwd=2)
legend("topright", legend=c("λ = 3", "λ = 20", "λ = 45"), 
       col=c("blue", "red", "green"), lwd=2)

# Q-9 (Load the csv file for heights and weights of 25000 people and do the following:)
# data <- read.csv("/home/ibab/Downloads/SOCR-HeightWeight.csv")
data <- read.csv("C:\\Users\\Yash Priya Baid\\SEM-2\\BS_and_R\\SOCR-HeightWeight.csv")
print(head(data))

# Q-9.1 (Plot a histogram of the height variable and determine it’s mean and standard deviation.)
hist(data$Height.Inches., breaks="Sturges", col="skyblue", border="black", main="Histogram of Height", xlab="Height (Inches)", ylab="Frequency")   
height_mean <- mean(data$Height.Inches., na.rm=TRUE)
print(height_mean)    # Output: 67.99311
height_sd <- sd(data$Height.Inches., na.rm=TRUE)
print(height_sd)      # Output: 1.901679

# Q-9.2 (Plot a histogram of the weight variable and determine it’s mean and standard deviation.)
hist(data$Weight.Pounds., breaks="Sturges", col="pink", border="black", main="Histogram of Weight", xlab="Weight (Pounds)", ylab="Frequency")   
weight_mean <- mean(data$Weight.Pounds., na.rm=TRUE)
print(weight_mean)   # Output: 127.0794
weight_sd <- sd(data$Weight.Pounds., na.rm=TRUE)
print(weight_sd)     # Output: 11.6609 

# Q-9.3 (Draw a Gaussian curve (recall the Gaussian PDF) with the above calculated mean and standard deviation for both height and weight variables as Z vs P(Z) 
# (i.e. Z-transformed). Plot using either plot() function or curve() function.)
# Compute Z-scores for Height and Weight
z_height <- (data$Height.Inches. - height_mean) / height_sd
z_weight <- (data$Weight.Pounds. - weight_mean) / weight_sd
gaussian_pdf <- function(z) {    # Define Gaussian PDF function
  (1 / sqrt(2 * pi)) * exp(-0.5 * z^2)
}
z_values <- seq(min(z_height, z_weight, na.rm=TRUE), max(z_height, z_weight, na.rm=TRUE), length.out=100)    # Generate Z-values based on transformed data
p_z <- gaussian_pdf(z_values)    # Compute Probability Density for Z
plot(z_values, p_z, type="l", col="blue", lwd=2, main="Gaussian PDF (Z-Transformed for Height & Weight)", xlab="Z-Score", ylab="P(Z)")
lines(z_values, p_z, col="red", lwd=2, lty=2)
legend("topright", legend=c("Height (Z-Transformed)", "Weight (Z-Transformed)"), col=c("blue", "red"), lwd=2, lty=c(1,2))

# Q-9.4 (What happens when you decrease the size of the bins in the histogram plots? Make a 1x3 grid of 3 plots that show the trend for decreasing bin sizes.)
par(mfrow=c(1,3))   # 1 row, 3 columns
hist(data$Height.Inches., breaks=5, col="skyblue", main="Bins=5", xlab="Height", ylab="Frequency", border="black")    # Plot with large bins
hist(data$Height.Inches., breaks=15, col="skyblue", main="Bins=15", xlab="Height", ylab="Frequency", border="black")   # Plot with medium bins
hist(data$Height.Inches., breaks=50, col="skyblue", main="Bins=50", xlab="Height", ylab="Frequency", border="black")   # Plot with small bins
# Large bins: Histogram is overly simplified, it hides details of the distribution & the data appears to be grouped into broad categories.
# Medium bins: A more detailed view of data & the distribution starts resembling a bell curve (normal distribution).
# Small bins: Most detailed but introduces some noise & now it closely follows the true shape of the data.

# Q-10 (Plot the PDF and CPDF for the uniform distribution U(1,2). Find a way to shade the region under the PDF up to x = 1.5)
x_vals <- seq(0.5, 2.5, length.out = 100)    # Set up a sequence of x values ranging from 0.5 to 2.5 (for better visualization))
# The density is 1 between x = 1 and x = 2, and 0 otherwise
pdf_vals <- ifelse(x_vals >= 1 & x_vals <= 2, 1, 0)    # Define the Probability Density Function (PDF) of U(1,2
# If x < 1, CPDF = 0
# If 1 ≤ x ≤ 2, CPDF increases linearly from 0 to 1
# If x > 2, CPDF = 1
cpdf_vals <- pmax(0, pmin((x_vals - 1) / (2 - 1), 1))   # Define the Cumulative Probability Density Function (CPDF) of U(1,2)
par(mfrow=c(1,2))  
# Plot PDF -
plot(x_vals, pdf_vals, type="l", lwd=2, col="blue", ylim=c(0,1.2), main="PDF of U(1,2)", xlab="x", ylab="Density")  
# Shade the area under the PDF from x = 1 to x = 1.5
# polygon() fills the region under the curve
polygon(c(1, seq(1, 1.5, length.out=20), 1.5),   # x-coordinates
        c(0, rep(1, 20), 0),   # Corresponding y-values (height = 1)
        col="lightblue",  
        border=NA)  # No border for the shaded region
# Plot CPDF -
plot(x_vals, cpdf_vals, type="l", lwd=2, col="red", main="CPDF of U(1,2)", xlab="x", ylab="Cumulative Probability")  
# Add horizontal and vertical reference lines for better visualization
abline(h=c(0,1), lty=2, col="gray")   # Horizontal dashed lines at y=0 and y=1
abline(v=c(1,2), lty=2, col="gray")   # Vertical dashed lines at x=1 and x=2

# Q-11 (Plot the PDF and CPDF for the exponential distribution with λ = 10. Shade the region under the PDF up to x = 2.8)
lambda <- 10    # Set the rate parameter λ (lambda) for the Exponential distribution
x_vals <- seq(0, 3, length.out = 100)   # Define a sequence of x values from 0 to 3 for better visualization
pdf_vals <- lambda * exp(-lambda * x_vals)    # Compute the Probability Density Function (PDF)
cpdf_vals <- 1 - exp(-lambda * x_vals)    # Compute the Cumulative Probability Density Function (CPDF)
par(mfrow=c(1,2))  
# Plot PDF -
plot(x_vals, pdf_vals, type="l", lwd=2, col="blue", main="PDF of Exp(λ=10)", xlab="x", ylab="Density", ylim=c(0, max(pdf_vals)))  
shade_x <- seq(0, 2.8, length.out = 100)    # Define x-values for shading
shade_y <- lambda * exp(-lambda * shade_x)   # Compute corresponding y-values
polygon(c(0, shade_x, 2.8), c(0, shade_y, 0), col="lightblue", border=NA)    # Use polygon() to fill the area under the curve
# Plot CPDF -
plot(x_vals, cpdf_vals, type="l", lwd=2, col="red", main="CPDF of Exp(λ=10)", xlab="x", ylab="Cumulative Probability", ylim=c(0, 1))
abline(h=c(0,1), lty=2, col="gray")   # Horizontal dashed lines at y=0 and y=1
abline(v=2.8, lty=2, col="gray")      # Vertical dashed line at x=2.8

# Q-12 (Plot the PDF and CPDF for the Gamma distribution with α = 5 and θ = 3.)
gamma_pdf <- function(x, alpha, theta) {
  ifelse(x > 0, (x^(alpha-1) * exp(-x/theta)) / (theta^alpha * gamma(alpha)), 0)
}
gamma_cdf <- function(x, alpha, theta) {
  ifelse(x > 0, pgamma(x, shape = alpha, scale = theta), 0)  # pgamma() for incomplete gamma function
}
# Parameters for the Gamma distribution
alpha <- 5
theta <- 3
x_vals <- seq(0, 30, length.out = 300)
pdf_vals <- gamma_pdf(x_vals, alpha, theta)
cdf_vals <- gamma_cdf(x_vals, alpha, theta)
# Plot PDF -
par(mfrow = c(1, 2))
plot(x_vals, pdf_vals, type = "l", col = "blue", lwd = 2,main = "Gamma PDF (α=5, θ=3)", xlab = "x", ylab = "Density")
# Plot CPDF -
plot(x_vals, cdf_vals, type = "l", col = "red", lwd = 2,main = "Gamma CDF (α=5, θ=3)", xlab = "x", ylab = "Probability")

# Q-13 (Plot the PDF and CPDF for the Chi-square distribution for 20 degrees of freedom. Shade the region under the PDF up to x = 1.0)
chi_square_pdf <- function(x, df) {
  ifelse(x > 0, (x^(df/2 - 1) * exp(-x/2)) / (2^(df/2) * gamma(df/2)), 0)
}
chi_square_cpdf <- function(x, df) {
  pchisq(x, df)  # Built-in function for cumulative probability
}
# Parameters for the Chi-square distribution
df <- 20   # Degrees of freedom
x_vals <- seq(0, 50, length.out = 300)   # X values for plotting
pdf_vals <- chi_square_pdf(x_vals, df)   # PDF values
cpdf_vals <- chi_square_cpdf(x_vals, df)  # CPDF values
par(mfrow = c(1, 2))  
plot(x_vals, pdf_vals, type = "l", col = "blue", lwd = 2, main = "Chi-square PDF (df=20)", xlab = "x", ylab = "Density")
shade_x <- seq(0, 1.0, length.out = 100)  # X values for shading
shade_y <- chi_square_pdf(shade_x, df)    # Corresponding PDF values
polygon(c(0, shade_x, 1.0), c(0, shade_y, 0), col = "darkred", border = NA)  
# Plot CPDF -
plot(x_vals, cpdf_vals, type = "l", col = "red", lwd = 2,main = "Chi-square CPDF (df=20)", xlab = "x", ylab = "Probability")
par(mfrow = c(1, 1))
