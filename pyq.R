##### TERM PAPER #####

# Q.1.a 
a <- 22
b <- 51.6
c <- 11.4
compute_Q <-  function(a, b, c){
  mean_of_squares <- ((a**2)+(b**2)+(c**2))/3
  Sum <- a+b+c
  exponential <- exp(b/(a+c))
  return((mean_of_squares * exponential)/Sum)
}
result <- compute_Q(a, b, c)
print(result)   # Output: 60.2

# Q.1.b 
X <- c(12, 34, 9, 32, 24, 43, 52, 16, 2, 17, 39, 57, 72, 82, 92, 4, 59, 79, 99)
Y <- c(33, 24, 15, 49, 22, 48, 66, 10, 4, 24, 42, 50, 66, 75, 99, 9, 61, 72, 90)
Xsmall <- X[X > 50]
print(Xsmall)
Ywindow <- Y[Y>20 & Y<70]
print(Ywindow)

# Q.2
dat <- read.csv("sample data.csv")
print(dat)
subdat <- subset(dat, `standard sample` <= 800)
print(subdat)
par(mfrow=c(1, 2))
hist(dat$`standard sample`, xlabel="Standard sample values", col="blue")
hist(dat$new_sample, xlabel="New sample values", col="red")
par(mfrow=c(1,1))

# Q.3.1 (9000 random deviates from Gaussian distri with mean 16, S.D.=3)
random_data <- rnorm(9000, mean=16, sd=3)
# Q.3.2
minimum <- min(random_data)
median_val <- median(random_data)
mean_val <- mean(random_data)
variance <- var(random_data)
max_val <- max(random_data)
print(c(min=minimum, median=median_val, mean=mean_val, var=variance, max=max_val))
# Q.3.3
D1 <- random_data[1:4500]
D2 <- random_data[4501:9000]
plot(D1, D2, main="Scatter plot", xlab="D1", ylab="D2")


##### IA1 PYQ #####

#Q.1.1
dat <- read.csv("expression.csv")
# or dat <- read.table("expression.txt", header=TRUE, sep=",")
print(dat)
print(colnames(dat))

# Q.1.2
control_mean <- rowMeans(dat[,1:8])    # assuming 8 control columns
treatment_mean <- rowMeans(dat[,9:16])    # assuming 8 treatment columns
dat$RatioCol <- control_mean/treatment_mean
print(head(dat$RatioCol, 10))

# Q.1.3
par(mfrow=c(2,2))
hist(dat$control3, col="blue", border="black", main="histogram of control3")
hist(dat$treatment3, col="red", border="black", main="histogram of treatment3")
plot(dat$control1, dat$control3, col="blue", xlab="control1", ylab="control3", main="Scatter plot b/w control1 & 3")
plot(dat$control6, dat$treatment6, col="red", xlab="control6", ylab="treatment6", main="Scatter plot b/w control6 & treatment6")

# Q.2.1
sub1 <- subset(dat, control1>50 & control2>50)
print(sub1)

# Q.2.2
subrow <- dat[c("gene4", "gene20", "gene37", "gene100"),]    # row of these genes
print(subrow)

# Q.2.3
set.seed(42)    # ensure reproducibility
samp <- dat[sample(nrow(dat), 200),]    # sample(nrow(dat)) - will shuffle the rows

# Q.2.4
boxplot(dat[,c('control4', 'control5', 'control6', 'treatment4', 'treatment5', 'treatment6')],
        main="Comparative boxplots of selected columns",
        col=c('blue', 'blue', 'blue', 'red', 'red', 'red'))

# Q-3.1 
random_deviates <- rnorm(1000, mean=20, sd=4)
hist(random_deviates, xlab="random_deviates values", col="blue", border="black", main="Histogram of Gaussian distri.")

# Q-3.2





