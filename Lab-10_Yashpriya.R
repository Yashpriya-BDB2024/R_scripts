####### LAB-10 (Date: 28.3.25) ########

# Section-1: Exercise on sampling, permutations & combinations -

# Q.1 (Sampling from a vector)
x <- seq(1, 100)    # generates a sequence of numbers from 1 to 100
s <- sample(x, 10)   # By default behaviour of sample() is without replacement (i.e., once a no. is picked, it can't be picked again).
print(s)    # randomly selects 10 unique numbers from x
# replace=TRUE - allows sampling with replacement (after a no. is picked, it is put back into the original pool & can be picked again).
sample(x, 10, replace=TRUE)    # may contain duplicate no.

# Q.2 (The package gtools has permutations and combinations functions.)
install.packages("gtools")
library(gtools)    # loads the gtools package into the R session

x <- c("A", "B", "C", "D")
# r = size of the target vector (i.e., no. of elements to select in each permutation), v = source vector, n = size of the source vector
per <- permutations(n=length(x), r=3, v=x, repeats.allowed = TRUE)   # repeat.allowed=TRUE - allows repeated elements in the permuatations
print(per)
comb <- combinations(n=length(x), r=3, v=x)   # Unlike permutations, order does not matter in combinations; and by default - it doesn't consider repeatitions
print(comb)

# Section-2: Exercise on distributions -

# Q.1 (Binomial distribution - the shape of this PDF is decided by the parameter p)
n <- 10   # no. of trials in the binomial exp.
p <- 0.4   # probability of success
m <- 3   # no. of successes we are interested in
# Q-1.1 (Print the probability value (PDF) for the above combination of numbers.)
print(dbinom(m, n, p))   # Output: 0.2149908 (Computes P(X=3), i.e., the probability of getting exactly 3 successes in 10 trials when p=0.4)

# Q-1.2 (Print the cumulative probability value (CPDF) for the above.)
print(pbinom(m, n, p))   # Output: 0.3822806 (Computes P(X<=3), i.e., the probability of getting 3 or fewer successes in 10 trials when p=0.4)

# Q-1.3 (Find the 'm' value corresponding to cumulative probability of 0.8)
cum_prob <- 0.8 
print(qbinom(cum_prob, n, p))   # Output: 5 (finds the'm' such that P(X<=m)>=0.8 This means it determines the no. of successes where the cumulative probability reaches 0.8)

# Q-1.4 (Print 5 points randomly sampled from the Binomial distribution.)
npts <- 5
print(rbinom(npts, n, p))

# Q-1.5 (Plot the probability density function (PDF) for the above parameters. On the same plot, plot the PDF for p=0.7 with a different colour.)
p2=0.7
x_vals=0:n   # possible values of successes
pdf1 <- dbinom(x_vals,n,p)   # computes the binomial probability P(X=x) for each x when p=0.4
pdf2 <- dbinom(x_vals,n,p2)  # computes the binomial probability P(X=x) for each x when p=0.7
plot(x_values, pdf1, type="b", col="red", pch=15, lty=1, ylim=c(0,max(pdf1,pdf2)), xlab="No. of successes", ylab="Probability", main="Binomial distri. PDF")
lines(x_vals, pdf2, type="b", col="blue", pch=16, lty=1)
legend("topright", legend=c("p=0.4","p=0.7"), col=c("red","blue"), pch=c(15,16), lty=c(1,1))

# Q-1.6 (Generate 100 and 10000 points randomly from this distribution and make a frequency table of the sampled points. Plot these as bar plots in a 2x1 grid.)
set.seed(42)    # ensures reproducibility
par(mfrow=c(2,1))   # 2 by 1 grid (2 rows, 1 column)
n=10
p=0.4
sample1 <- rbinom(100, n, p)    # generates 100 random samples from the Binomial distribution with n=10, p=0.4
sample2 <- rbinom(10000, n, p)  # generates 10000 random samples from the same distribution
freq1 <- table(sample1)   # table() - frequency table ; counts how many times each value (0-10) appears in the 100 samples.
freq2 <- table(sample2)  # counts how many times each value (0-10) appears in the 10000 samples.
barplot(freq1, main="Binomial distri. of 100 samples", col="purple", xlab="No. of successes", ylab="Frequency")
barplot(freq2, main="Binomial distri. of 10000 samples", col="yellow", xlab="No. of successes", ylab="Frequency")

# Q-2 (Hypergeometric distribution)

# Q-2.1 (Plot a histogram type plot of the hypergeometric probability density function with N=100, K=70, p=0.3, n=12 and add text within the plot window of the parameter names and their values.)
N <- 100   # total population size
K <- 70   # No. of successes in population
p <- 0.3   # no use here
n <- 12   # sample size
k_vals <- 0:n    # k (no. of possible successes)
hyper_pdf <- dhyper(k_vals, K, N-K, n)
barplot(hyper_pdf, names.arg=k_vals, col="green", ylim=c(0,max(hyper_pdf)), main="Hypergeometric distribution", xlab="No. of successes (k)", ylab="Probability", border="darkgreen")
text(6, y=max(hyper_pdf)*0.8, labels=paste("N=", N, "\nK=", K, "\nn=", n), col="black", cex=0.8)     # Add text annotation for parameter values

# Q-2.2 (Compute the cumulative probability up to x=10 and print the result after rounding off to 3 decimal places.)
# q = 10 (we want the prob. of drawing at most 10 successes, i.e., P(X<=10)), m=K=70 (no. of successes in population), n=N-K=30 (no. of failures in population), k=n=12 (sample size)
rounded_cumulative_prob <- round(phyper(10, K, N-K, n), 3)    
print(rounded_cumulative_prob)     # Output: 0.928

# Q-2.3 (Obtain the x value corresponding to a cumulative probability value of 0.9)
hyper_x_val <- qhyper(0.9, K, N-K, n)
print(hyper_x_val)     # Output: 10

# Q-2.4 (Sample 5 points randomly from this distribution and print these with two significant digits.)
hyper_samples <- signif(rhyper(5, K, N-K, n), 2)
print(hyper_samples)

# Q-3 (Geometric Distribution)

# Q-3.1 (Plot 2 probability density functions for this distribution in a 1x2 grid with (i)p=0.3 and (ii) p=0.8. What differences do you see?)
par(mfrow=c(1,2))   # 1 by 2 grid
m_vals <- 1:10   # first success occurs at trial 'm'
p1 <- 0.3
p2 <- 0.8
geo_pdf1 <- dgeom(m_vals-1, p1)   # m_vals-1 counts the no. of failures before the 1st success
geo_pdf2 <- dgeom(m_vals-1, p2)
barplot(geo_pdf1, names.arg=m_vals, col="darkred", main="Geometric Distribution (p=0.3)", xlab="Trial no. (m)", ylab="Probability", border="black")
barplot(geo_pdf2, names.arg=m_vals, col="seagreen", main="Geometric Distribution (p=0.8)", xlab="Trial no. (m)", ylab="Probability", border="black")
# In case of plot-1 (left side, where p=0.3), PMF is more spread out, highest probability is seen in 1st trial, and as 'm' increases, the probability decreases.
# In case of plot-2 (right side, where p=0.8), the prob. is heavily concentrated at m=1, and as 'm' increases, the probability drops off very sharply, meaning almost all successes happen very early.

# Q-3.2 (Compute the cumulative probability up to x=4)
cumulative_geo_prob1 <- pgeom(4-1, p1)  # The geometric distri. in R (dgeom and pgeom) defines 'x' as the no. of failures before the first success, so m-1 done.
print(cumulative_geo_prob1)
cumulative_geo_prob2 <- pgeom(4-1, p2)
print(cumulative_geo_prob2)

# Q-3.3 (Compute the value of m at which the cumulative probability is 0.2)
geo_m_value <- qgeom(0.2, 0.3)+1   
print(geo_m_value)    # Output: 1

# Q-3.4 (Generate 6 random deviates or sample points from this distribution with p=0.4)
geo_random <- rgeom(6, 0.4)+1
print(geo_random)

# Q-4 (Negative binomial distribution)

# Q-4.1 (Compute and print the negative binomial probability density for y=5, r=3 and p=0.3)
y <- 5   # no. of failures before 'r' successes 
r <- 3   # no. of successes desired
p <- 0.3
neg_binomial_pdf <- dnbinom(y, r, p)   # P(Y=5) - probability of getting 5 failures before 3 successes
print(neg_binomial_pdf)   # Output: 0.09529569

# Q-4.2 (Compute and print the cumulative negative binomial probability density up to y=5)
cumulative_neg_binom <- pnbinom(5, r, p)
print(cumulative_neg_binom)   # Output: 0.4482262

# Q-4.3 (What is the 'y' value corresponding to a cumulative probability value of 0.5? (i.e., the median))
y_val_neg_binom <- qnbinom(0.5, r, p)
print(y_val_neg_binom)    # Output: 6

# Q-4.4 (Print 4 random points sampled from this distribution with r=3 and p=0.3)
random_neg_binom <- rnbinom(4, r, p)
print(random_neg_binom)

# Q-4.5 (Plot the negative binomial distribution function using r=10, p=0.3)
r1 <- 10
y_vals <- 0:70
pmf_vals <- dnbinom(y_vals, r1, p)
barplot(pmf_vals, names.arg=y_vals, col="skyblue", main="Negative Binomial Distribution (r=10, p=0.3)", xlab="No. of failures (y)",ylab="Probability",border="black")

# Q-4.6 (Generate a frequency histogram of 10,000 random deviates from this distribution with r=10 and p=0.3)
random_neg_binom <- rnbinom(10000, r1, p)
hist(random_neg_binom, breaks=30, col="pink", main="Histogram of Negative Binomial Distribution (r=10, p=0.3)", 
     xlab="No. of failures (y)", freq=TRUE)

# Q-5 (Poisson Distribution)

# Q-5.1 (Compute and print the Poisson probability given λ = 10 and m = 7)
lambda <- 10   # mean (expected value) of the Poisson distribution
m <- 7   # the no. of occurrences we're interested in
poisson_prob <- dpois(m, lambda)
print(poisson_prob)   # Output: 0.09007923

# Q-5.2 (Calculate and print the cumulative probability for the same values above.)
poisson_cpdf <- ppois(m, lambda)
print(poisson_cpdf)   # Output: 0.2202206

# Q-5.3 (Make two bar plots showing a binomial probability distribution with n=1000, p=0.3 and a Poisson PDF with λ=np. Do the two distributions agree? Why? Why not?)
# The two distributions don't agree when p is 0.3, because the poisson distribution is an approximation of the binomial only when p is small (typically p < 0.1) and n is large.
n = 1000
p = 0.01
lambda = n * p
m_values = 0:150
binom_values <- dbinom(m_values, size = n, prob = p)
pois_values <- dpois(m_values, lambda)
plot(m_values, binom_values,xlim=c(0,150),ylim=c(0,0.5), type = "h", col = "hotpink2", lwd = 2, main = "Binomial vs Poisson Distribution", xlab = "m values", ylab = "Probability")
# Overlay Poisson Distribution
lines(m_values, pois_values,xlim=c(0,150),ylim=c(0,0.5), type = "l", col = "steelblue2", pch=16,lwd = 2)
legend("topright", legend = c("Binomial", "Poisson"), col = c("hotpink2", "steelblue2"), lwd = 2)

# Q-5.4 (Find the quantile value corresponding to cumulative probability of 0.22 and λ = 10.)
lambda=10
m_val <- qpois(0.22,lambda)
print(m_val)    # Output: 7

# Q-5.5 (Obtain 10000 random sample points from a Poisson distribution with λ = 9 and make a histogram plot.)
sample_size <- 10000
lambda1 <- 9
random_poisson <- rpois(sample_size, lambda1)
hist(random_poisson, brea-ks = seq(min(random_poisson), max(random_poisson), by = 1), border = "black", main = "Histogram of Poisson Distribution (lambda=9)", xlab = "No of events (m)", ylab="Frequency")
