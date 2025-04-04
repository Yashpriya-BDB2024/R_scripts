# Q-6 (Gaussian Distribution)

# Q-6.1 (Compute and print the unit normal PDF value for μ = 12 and σ = 2)
mu <- 12
sigma <- 2
x <- 12   # The point at which we evaluate the PDF
gaussian_pdf <- dnorm(x, mu, sd=sigma)
print(gaussian_pdf)

# Q-6.2 (Calculate and print the cumulative probability for Z = 2.0. Is this same as 1-CPDF(Z=-2)?)
Z <- 2.0
cpdf_val <- pnorm(Z)   
print(cdf_val)   # Output: 0.9772499
Z1 <- -2
cpdf_val1 <- 1-pnorm(Z1)
print(cpdf_val1)   # Output: 0.9772499 (same as above)

# Q-6.3 (Plot a unit normal curve for the above parameters with X range of ±4σ and add a text box to the plot showing the parameter symbols and their values.)


# Q-7 (Uniform Distribution)

# Q-7.1 (Generate 5 uniform random numbers between 0 and 1 and print them.)
print(runif(5, min=0, max=1))

# Q-7.2 (Generate 5 random samples from a uniform distribution between 50 and 100.)
print(runif(5, min=50, max=100))

# Q-7.3 (Generate 10,000 uniform deviates and plot a histogram with x-limits 1 and 2.)
sample_size1 <- 10000
uniform_data <- runif(sample_size1, min=1, max=2)    # By default, min=0 & max=1, which falls outside the xlim, so we set min to 1 and max to 2.
hist(uniform_data, breaks=30, col="skyblue", border="black", main="Histogram of Uniform Distribution (1 to 2)", xlab="Values", ylab="Frequency", xlim=c(1,2))

# Q-8 (Exponential Distribution)

# Q-8.1 (What is the probability density corresponding to x = 3 and λ = 2?)
x <- 3
lambda <- 2
exponential_prob_density <- dexp(x, rate=lambda)
print(exponential_prob_density)   # Output: 0.004957504

# Q-8.2 (What is the quantile value corresponding to cumulative probability value of 0.995 for the above distribution?)
quantile_995 <- qexp(0.995, rate=lambda)
print(quantile_995)   # Output: 2.649159

# Q-8.3 (Plot the exponential cumulative probability distributions on the same graph for λ = 2, 10 and 100)
x_vals <- seq(0,5,length.out=500)
plot(x_vals, pexp(x_vals, rate=2), type="l", col="black", lwd=2, main="Exponential CPDFs", xlab="x values", ylab="Cumulative Probability")
lines(x_vals, pexp(x_vals, rate=10), col="blue", lwd=2)
lines(x_vals, pexp(x_vals, rate=100), col="red", lwd=2)
legend("bottomright", legend=c("lambda=2", "lambda=10", "lambda=100"), col=c("black", "blue", "red"), lwd=2)

# Q-8.4 (Compute and print 4 random deviates from an exponential distribution with λ = 3)
lambda1 <- 3
exponential_random_deviates <- rexp(4, rate=3)
print(exponential_random_deviates)

# Q-9 (Gamma Distribution)

# Q-9.1 (Plot the PDFs on the same graph with alpha values of 1,2,3,4 and θ value 4 with colors black, blue, red and magenta respectively. This is one of the two graphs on
# a 1x2 grid. Plot the second graph with θ values of 1,2,3,4 and α = 4 with colors black, blue, red and magenta respectively.)
x_values <- seq(0, 30, length.out=500)
par(mfrow=c(1,2))
# First plot: Vary alpha, fix theta = 4
plot(x_values, dgamma(x_values, shape=1, scale=4), type="l", col="black", lwd=2, main="Gamma PDFs: Varing alpha values", xlab="x values", ylab="Density")
lines(x_values, dgamma(x_values, shape=2, scale=4), col="blue", lwd=2)    # shape option: alpha ; scale option: theta
lines(x_values, dgamma(x_values, shape=3, scale=4), col="red", lwd=2)
lines(x_values, dgamma(x_values, shape=4, scale=4), col="magenta", lwd=2)
legend("topright", legend=c("alpha=1", "alpha=2", "alpha=3", "alpha=4"), col=c("black", "blue", "red", "magenta"), lwd=2)
# Second plot: Vary theta, fix alpha = 4
plot(x_values, dgamma(x_values, shape=4, scale=1), type="l", col="black", lwd=2, main="Gamma PDFs: Varying theta", xlab="x values", ylab="Density")
lines(x_values, dgamma(x_values, shape=4, scale=2), col="blue", lwd=2)
lines(x_values, dgamma(x_values, shape=4, scale=3), col="red", lwd=2)
lines(x_values, dgamma(x_values, shape=4, scale=4), col="magenta", lwd=2)
legend("topright", legend=c("theta=1", "theta=2", "theta=3", "theta=4"), col=c("black", "blue", "red", "magenta"), lwd=2)

# Q-9.2 (Compute and print the probability density corresponding to x = 6, α = 4 and θ = 1.)
alpha <- 4
theta <- 1
x <- 6
pdf_gamma <- dgamma(x, shape=alpha, scale=theta)
print(pdf_gamma)   # Output: 0.08923508

# Q-9.3 (Compute and print the cumulative probability up to x=6 for the above gamma PDF.)
cumulative_prob_gamma <- pgamma(x, shape=alpha, scale=theta)
print(cumulative_prob_gamma)   # Output: 0.848796

# Q-9.4 (Compute the x value which corresponds to a cumulative probability of 0.95)
quantile_95 <- qgamma(0.95, shape=alpha, scale=theta)
print(quantile_95)   # Output: 7.753657

# Q-9.5 (Obtain 10,000 random deviates from the above gamma distribution and plot a histogram of this set.)
sample_size <- 10000
random_gamma <- rgamma(sample_size, shape=alpha, scale=theta)
hist(random_gamma, breaks=30, col="red", border="black", main="Histogram of Gamma Distribution", xlab="x", ylab="Frequency")

# Q-10 (Chi-square Distribution)

# Q-10.1 (Plot the χ2 distribution with degree of freedom 2,3,5 and 10.)
x <- seq(0, 30, length.out=500)   # Define x values
df_vals <- c(2,3,5,10)   # Degree of freedom
colour <- c("red", "blue", "green", "yellow")
plot(x, dchisq(x, df_vals[1]), type="l", col=colour[1], lwd=2, main="Chi-square Distribution", xlab="x values", ylab="Density", ylim=c(0, 0.25))
for (i in 2:length(df_vals)) {
  lines(x, dchisq(x, df_vals[i]), col=colour[i], lwd=2)
}
legend("topright",legend=paste("df=", df_vals),col=colour,lwd=2)
# The curves are skewed right; as the degree of freedom increases, the peak of the curve shifts further right.

# Q-10.2 (Compute and print the probability density for x=6 and 5 degrees of freedom.)
x_val <- 6
df <- 5
prob_density <- dchisq(x_val, df)
print(prob_density)   # Output: 0.09730435

# Q-10.3 (Compute and print the cumulative probability up to x=6 and 10 degrees of freedom.)
x_val1 <- 6
df1 <- 10
cumulative_prob <- pchisq(x_val1, df1)
print(cumulative_prob)   # Output: 0.1847368

# Q-10.4 (Obtain the 85th quantile for this distribution for 6 degrees of freedom.)
df2 <- 6
quantile_85 <- qchisq(0.85, df2)
print(quantile_85)   # Output: 9.446103

# Q-10.5 (Plot a histogram of 10,000 random deviates from this distribution with 6 degrees of freedom with 30 bins, red filled bars and text within the plot ”r=6” in an appropriate blank portion.)
sample_size <- 10000
df3 <- 6
random_chisq <- rchisq(sample_size, df3)
hist(random_chisq, breaks=30, col="red", border="black", main="Histogram of Chi-square Distribution (df=6)", xlab="Values", ylab="Frequency")
text(20, 1200, "r = 6", col="black", cex = 1.5, font = 2)    # Add text annotation in a blank portion.

# Q-10.6 (Assuming μ = 2 and σ = 1 compute the variable Z^2 = (x − μ)^2/σ^2 and plot the χ2 PDF with 1 degree of freedom.)
mu <- 2
sigma <- 1
x <- seq(0,10,length.out=500)  # Define x values
z_sq <- ((x-mu)^2)/sigma^2
plot(z_sq, dchisq(z_sq, df=1), type="l", col="blue", lwd=2, main="Chi-square PDF with df=1", xlab="Z^2", ylab="Density")
# The density is high only near zero, and then it flattens out quickly.
