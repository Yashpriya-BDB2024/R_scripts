###### To be continued ------ ####### 
###### DATE: 29.3.25 ######

# Q-6 (Gaussian Distribution)

# Q-6.1 (Compute and print the unit normal PDF value for μ = 12 and σ = 2)
mu <- 12
sigma <- 2
x <- 12   # The point at which we evaluate the PDF
gaussian_pdf <- dnorm(x, mean=mu, sd=sigma)
print(gaussian_pdf)   # Output: 0.1994711

# Q-6.2 (Calculate and print the cumulative probability for Z = 2.0. Is this same as 1-CPDF(Z=-2)?)
Z <- 2.0
cpdf_val <- pnorm(Z)   
print(cpdf_val)   # Output: 0.9772499
Z1 <- -2
cpdf_val1 <- 1-pnorm(Z1)
print(cpdf_val1)   # Output: 0.9772499 (same as above)

# Q-6.3 (Plot a unit normal curve for the above parameters with X range of ±4σ and add a text box to the plot showing the parameter symbols and their values.)
x <- seq(mu - 4*sigma, mu + 4*sigma, length.out=500)
y <- dnorm(x, mean=mu, sd=sigma)
plot(x, y, type="l", lwd=2, col="blue", main="Normal Distribution (mean=12, sigma=2)", xlab="x values", ylab="Density")
text(x=mu+2, y=max(y)*0.8, labels=paste("mu =", mu, "\nsigma =", sigma), pos=4, cex=0.8, col="darkred")   # pos=4 : right to the (x,y) point

# Q-6.4 (Generate the 75th quantile point for a unit normal distribution with the above parameters.)
quantile_75 <- qnorm(0.75, mean=mu, sd=sigma)
print(quantile_75)  # Output: 13.34898

# Q-6.5 (Generate 10,000 random deviates from the unit normal distribution and plot a histogram. On top of this plot the unit normal curve from which you sampled.)
normal_random_deviates <- rnorm(10000, mean=mu, sd=sigma)
hist(normal_random_deviates, breaks=30, col="skyblue", probability=TRUE, main="Histogram with normal curve", xlab="Values", ylab="Density")
lines(x, y, col="red", lwd=2)   # On top of this, plot the unit normal density curve 

# Q-6.6 (Make a histogram plot of a ‘normalized’ binomial distribution with μ = np = 10 and p = 0.5. 
# ‘Normalized’ here means computing a variable of type W = m−np/sqrt(np(1−p)) where m is the number of successes in n trials. 
# On top of this, plot a unit normal distribution N(np,np(1-p)). Do the two distributions agree?)
n <- 100  # Sample size (n<100 - doesn't agree)
p <- 0.5
mu <- n*p
sd_binom <- sqrt(n*p*(1-p))
m <- rbinom(10000, size=n, prob=p)   # Generate 10,000 random binomial samples
W <- (m - mu) / sd_binom   # W: normalized binomial
hist(W, probability=TRUE, breaks=30, col="yellow", main="Normalized Binomial Distribution vs Normal Unit Distribution", xlab="W")
curve(dnorm(x, mean=0, sd=1), col="darkgreen", lwd=2, add=T)   # add=T - overlays the normal curve onto the existing histogram instead of replacing it.
# Acc. to CLT, as sample size increases, the sampling distribution of the binomial (even though it's discrete) becomes increasingly bell-shaped and approximates a normal distribution, especially when p≈0.5.

# Q-6.7 (Plot 4 Poisson probability distribution functions corresponding to λ values 1, 10, 100 and 1000 in the same graph. 
# On the same graph of each Poisson PDF plot, plot a unit normal distribution with Z = (m-λ)/sqrt(λ). 
# For what value of λ(s) do the two plots agree? Use a 2x2 grid for this problem.)

# Method-1 (Mean centered at 0) -
par(mfrow = c(2,2))  # 2x2 Grid
lambda_vals <- c(1,10,100,1000)
for (lambda in lambda_vals) {
  m <- 0:(lambda + 3 * sqrt(lambda))  
  z_vals <- (m - lambda) / sqrt(lambda)
  plot(z_vals, dnorm(z_vals), type="h", lwd=2, col="blue", main=paste("λ =", lambda), xlab="Z = (m - λ) / sqrt(λ)", ylab="Probability")   # Plot Poisson PMF (normalized)
  z_norm <- seq(-4, 4, length.out =100)
  lines(z_norm, dnorm(z_norm), col = "red", lwd = 2)   # Overlay Standard Normal Curve
}
par(mfrow=c(1,1))

# Method-2 (Original mean restored after normalization)
par(mfrow = c(2,2))  # 2x2 Grid
lambda_vals <- c(1,10,100,1000)
for (lambda in lambda_vals) {
  x <- 0:(lambda + 3 * sqrt(lambda))  
  pois_probs <- dpois(x, lambda)
  plot(x, pois_probs, type ="h")
  z_vals <- (x - lambda) / sqrt(lambda)
  lines(x, dnorm(x, mean = lambda, sd = sqrt(lambda)))
}
par(mfrow=c(1,1))
 
# Q-6.8 (The library MASS is used to generate two or more vectors of normally distributed random numbers that are correlated with one another to a specified degree.)
install.packages('MASS')
library('MASS')
xy <- mvrnorm(1000, mu=c(50,60), matrix(c(4,3.7,3.7,9),2))   
print(xy)
# It will generate 2 sets of 1000 no. each with a mean of 50 for the 1st set & 60 for the 2nd set. The matrix option specifies the covariance matrix of the variables.

# Q-6.8.1 (Execute var(xy) to check how close the variances are to our specified values – what is the covariance from these sample sets?)
sample_var_cov_matrix <- var(xy)  # Since, we're drawing random samples, the variance values will be close to our specified values but not exact due to sampling variation.
print(sample_var_cov_matrix)
# Covariance: It tells us how two variables change together.
# If two variables increase together, then covariance is positive, if one increases while other decreases, then its negative and if changes independently, then its zero.
print(sample_var_cov_matrix[1,2])  # Specified covariance is 3.7 and what we get is close to 3.7 

# Q-6.8.2 (Extract the separate vectors x and y as x <- xy[,1] and y <- xy[,2] and plot them to look at the correlation. Print the individual variances as var(x) and var(y).)
x <- xy[,1]
y <- xy[,2]
plot(x, y, main = "Scatter plot of x and y (for correlation)", xlab="x", ylab="y")
print(var(x))
print(var(y))

# Q-6.8.3 (Are the two samples independent? If so then the sum of their variances should be equal to the variance of their sum.).
var_sum <- var(x+y)
sum_var <- var(x)+var(y)
print(var_sum)  
print(sum_var)
if (var_sum == sum_var) { print("x and y are independent.") } else { print("x and y are not independent.") }
# Output: Since, we introduced covariance (3.7), so variables are not independent.

# Q-6.8.4 (The reported covariance is var(xy). Compute the covariance using the correlation coefficient cor(x,y) and the individual variances and make sure this matches with the reported value.)
cor_xy <- cor(x,y)   # correlation coefficient
cov_xy <- cor_xy * sqrt(var(x)) * sqrt(var(y))   # covariance formula
reported_cov_xy <- var(xy)[1,2]
print(cov_xy)  
print(reported_cov_xy)
# Yes, the computed covariance exactly matches with the reported covariance value.

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

##### To be continued ------ #####
##### DATE: 4.4.25 #####

# Section-3 (Central Limit Theorem)

# Section-3.1 (CLT case with sampling from non-normal distribution: )

# Q.1 (Generate a sample set of 5 random deviates from a uniform distribution in the interval [0,10]. Repeat this 10,000 times. We now have 10,000 samples of 5 numbers each.)
set.seed(42)  # to ensure reproducibility
samples <- replicate(10000, runif(5, min=0, max=10))  # replicate(10000, ........) - will repeat this 10,000 times

# Q.2 (Calculate the mean for each of the 10,000 samples and store it in a separate array. Plot this distribution of means as a histogram. 
# Print the mean and standard deviation of the distribution of means.)
sample_mean <- colMeans(samples)
print(sample_mean)
hist(sample_mean, breaks=30, main="Distribution of Means (Uniform [0,10])", xlab="Sample Mean", col="skyblue")
mean_distri <- mean(sample_mean)
print(mean_distri)
sd_distri <- sd(sample_mean)
print(sd_distri)

# Q.3 (Generate a sequence of numbers between 0 and 10 with 0.1 spacing. Obtain the normal probability densities using dnorm function with the calculated mean and
# standard deviation in the last question and store it as a separate vector.)
x_vals <- seq(0,10,by=0.1)
normal_pdf <- dnorm(x_vals, mean=mean_distri, sd=sd_distri)

# Q.4 (Since we have 10,000 samples, we have to scale the normal probability density function to our particular case (since the area is 1.0 otherwise). The height and
# bin width of a histogram are related to each other – if we doubled the bin width, there would be roughly twice as many numbers in the bin and the bar would be
# twice as high on the y-axis. So to get the height of the bars on our frequency scale, we multiply the total frequency, i.e., 10,000 by the bin width 0.5 to get 5000. This
# is the scaling factor for the PDF. Perform this and save the scaled probabilities as a separate array.)
scaling_factor <- 10000 * 0.5  # total frequency: 10000, bin width: 0.5
scaled_pdf <- normal_pdf * scaling_factor

# Q.5 (Plot the normal curve on top of the histogram to see the level of agreement b/w the normal behaviour of the sample means and the normal curve.)
hist(sample_mean, breaks=30, col="green", main="CLT: Uniform Distribution", xlab="Sample Mean")
lines(x_vals, scaled_pdf, col="skyblue", lwd=2)   # Overlay normal curve on top of histogram
legend("topright", legend=c("Normal behaviour of sample means", "Normal PDF"), col=c("green", "skyblue"), lwd=2)

# Section-3.2 (CLT demo with sampling from non-normal, non-functional distribution: )

# Q.1 (Create 10,000 samples of single dice throw using the sample() function. Make a plot of this distribution. You should see a uniform distribution.)
a <- sample(1:6, size=10000, replace=TRUE)
hist(a, main="Single Dice Throw - Uniform Distribution", xlab="Dice Face", ylab="Frequency", col="skyblue",
     breaks=seq(0.5, 6.5, 1),  # Breaks between each dice face
     xaxt="n")                 # Disable x-axis ticks
axis(1, at=1:6)                # Add custom ticks at 1 to 6

# Q.2 (Throw two dice and add the scores together (this is the ancient game of craps). Generate a new object b similar to the above. 
# Plot a histogram of a+b. You should see a triangular shape developing for the histogram.)
# Two independent dice throws
a <- sample(1:6, size=10000, replace=TRUE)
b <- sample(1:6, size=10000, replace=TRUE)
sum_2_dice <- a+b  # Sum of the two dice throws
# Plot the histogram (summing gives values 2 to 12)
hist(sum_2_dice, main="Sum of Two Dice Throws - Triangular Distribution", xlab="Sum of Dice Faces", ylab="Frequency", col="orange", breaks=seq(1.5, 12.5, 1), xaxt="n")
axis(1, at=2:12)

# Q.3 (Repeat the exercise with three dice. The histogram should start showing the distinct bell shape.)
a <- sample(1:6, size=10000, replace=TRUE)
b <- sample(1:6, size=10000, replace=TRUE)
c <- sample(1:6, size=10000, replace=TRUE)
sum_3_dice <- a+b+c  # Sum of three dice throws
hist(sum_3_dice, main="Sum of Three Dice Throws - Bell Shape", xlab="Sum of Dice Faces", ylab="Frequency",col="lightgreen",
     breaks=seq(2.5, 18.5, 1),   # Range: 3 to 18, so breaks from 2.5 to 18.5
     xaxt="n")
axis(1, at=3:18)

# Q.4 ( Repeat this with five dice. The histogram is now very close to a normal curve. Use the mean and standard deviation of the 5 dice to generate a normal PDF. 
# As in the last problem, one has to scale the PDF to match the height of the normal curve with the height of the histogram.)
a <- sample(1:6, size=10000, replace=TRUE)
b <- sample(1:6, size=10000, replace=TRUE)
c <- sample(1:6, size=10000, replace=TRUE)
d <- sample(1:6, size=10000, replace=TRUE)
e <- sample(1:6, size=10000, replace=TRUE)
sum_5_dice <- a+b+c+d+e  # Sum of five dice throws
h <- hist(sum_5_dice, main="Sum of Five Dice Throws - Very close to normal curve", xlab="Sum of Dice Faces", ylab="Frequency", col="lightcoral", breaks=seq(4.5, 30.5, 1), xaxt="n")
axis(1, at=5:30)
mean_val <- mean(sum_5_dice)
sd_val <- sd(sum_5_dice)
x <- seq(5, 30, length.out = 100)
y <- dnorm(x, mean=mean_val, sd=sd_val)
max_hist <- max(h$counts)   # Get max frequency from histogram
max_y <- max(y)   # Get max height of the unscaled normal curve
scaled_y <- y*(max_hist/max_y)   # Scale the normal y-values so that max(y) matches max(h$counts)
lines(x, scaled_y, col="red", lwd=2)

# Section-4 (ROC Curve)

# Q-1 (Read in the white wine data into a dataframe, and create additional columns that classifies the data as good or bad wine based on threshold quality scores of 6, 7, 8, 9 and 10.)
wine_data <- read.csv("\home\ibab\Downloads\winequality-white.csv", header=TRUE, sep=";") 
print(str(wine_data))   # View structure of the data

# Create good (label: 1) or bad (label: 0) wine binary classification columns based on different thresholds -
# If the quality is greater than or equal to the threshold, then it will be labelled as 1, otherwise will be labelled as 0.
wine_data$threshold_6 <- ifelse(wine_data$quality >= 6, 1, 0)
wine_data$threshold_7 <- ifelse(wine_data$quality >= 7, 1, 0)
wine_data$threshold_8 <- ifelse(wine_data$quality >= 8, 1, 0)
wine_data$threshold_9 <- ifelse(wine_data$quality >= 9, 1, 0)
wine_data$threshold_10 <- ifelse(wine_data$quality >= 10, 1, 0)
print(head(wine_data, 20))   # To see the appended new columns of labels

# Q-2 (Use plot.roc() function to plot the ROC curves for each threshold value. Which threshold value brings the ROC curve to the perfect classifier?)
install.packages('pROC')
library('pROC')
plot.roc(wine_data$threshold_6, wine_data$alcohol, main="ROC Curve (threshold >= 6)", legacy.axes=TRUE, ci=TRUE, print.auc=TRUE, identity.lwd=2, print.thres=TRUE)   # AUC: 0.735
plot.roc(wine_data$threshold_7, wine_data$alcohol, main="ROC Curve (threshold >= 7)", legacy.axes=TRUE, ci=TRUE, print.auc=TRUE, identity.lwd=2, print.thres=TRUE)   # AUC: 0.732
plot.roc(wine_data$threshold_8, wine_data$alcohol, main="ROC Curve (threshold >= 8)", legacy.axes=TRUE, ci=TRUE, print.auc=TRUE, identity.lwd=2, print.thres=TRUE)   # AUC: 0.750
plot.roc(wine_data$threshold_9, wine_data$alcohol, main="ROC Curve (threshold >= 9)", legacy.axes=TRUE, ci=TRUE, print.auc=TRUE, identity.lwd=2, print.thres=TRUE)   # AUC: 0.850
plot.roc(wine_data$threshold_10, wine_data$alcohol, main="ROC Curve (threshold >= 10)", legacy.axes=TRUE, ci=TRUE, print.auc=TRUE, identity.lwd=2, print.thres=TRUE)  
# Since, threshold >= 10 classifies the data into one class only (label: 0), so there will be no ROC / AUC. 
# A perfect classifier has an AUC of 1.0, which means it perfectly distinguishes b/w the positive and negative classes without any error.
# The threshold >= 9 gives the highest AUC (0.850) and therefore comes closest to being a "perfect classifier".
