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
barplot(hyper_pdf, names.arg=k_vals, col="green", ylim=c(0,max(hyper_pdf), main="Hypergeometric distribution"), xlab="No. of successes (k)", ylab="Probability", border="darkgreen")
text(6, y=max(hyper_pdf)*0.8, labels=paste("N=", N, "\nK=", K, "\nn=", n), col="black", cex=0.8)     # Add text annotation for parameter values
