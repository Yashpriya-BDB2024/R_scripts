### DATE: 10.1.25 (LAB-2)

# Q-1.1
round(123456789,digits=3)        # Output: 123456789
round(12.1343, digits=3)         # Output: 12.134 (R correctly rounds the no. based on standard rounding rules)

# Q-1.2
round(123.12344, digits=3)       # Output: 123.123 (rounds the no. to three decimal places)

# Q-1.3
round(1234.12344, digits=3)      # Output: 1234.123

# Q-1.4
round(12345.12344, digits=3)     # Output: 12345.12 (But, in case of large numbers, even though the rounded value should have been 12345.123, it only displays 12345.12 bec. R is limited in how many digits it shows) 

# Q-1.5 
options(digits=15)     # options(digits=15) tells R to display more digits, so we will now see the full rounded result
round(12345.12344, digits=3)     # Output: 12345.123 

# Q-1.6 (formatC() - ensures that the result is displayed with exact formatting. The format="f" argument specifies fixed-point notation)
formatC(round(12345.12344, digits=3), format="f", digits=3)     # Output: "12345.123"

# Q-1.7 (ptint() - displays numbers with up to 7 significant digits) 
print(1234.12344)     # Output: 1234.12344 (Since, it already fits within this limit, R prints the no. exactly as it is.)

# Q-1.8
# If the number of digits is less than or equal to the no. of digits before the decimal point, 
# then print() retains the nearest rounded integer
print(1234.723, digits=3)      # Output: 1235
# If the digits value is greater than the no. of digits before the decimal point, then the appropriate number of digits
# after the decimal point are retained.
print(1234.723, digits=5)      # Output: 1234.7

# Q-1.9
round(123456788.123, digits=3)   # Output: 123456788.123

# Q-1.10 (print(round() - prints the rounded no. to 2 decimal places, and displays 20 significant digits)
print(round(123456788.123, digits=2), digits=20)    # Output: 123456788.12000000477

# Q-1.11 (This will round the no. to four decimal places & then print it with 20 significant digits)
print(round(123456789.1234, digits=4), digits=20)    # Output: 123456789.12340000272

# Q-1.12 (paste() - concatenates strings with a space by default)
paste("Hello World")        # Output: "Hello World" (Since, it is a single string, so returns as it is.)
paste("Hello", "World")     # Output: "Hello World"

# Q-1.13
paste(1:10)       # Output: "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10"  (converts the numbers into character strings)
paste(1:10)[4]    # Output: "4" (selects the fourth element of the resulting vector)

# Q-1.14 (convert strings back to numbers)
as.numeric(paste(1:10))     # Output: 1  2  3  4  5  6  7  8  9 10

# Q-1.15 (collapse="." - combines 1 to 10 numbers into single string with '.' b/w each no.)
paste(1:10, collapse=".")   # Output: "1.2.3.4.5.6.7.8.9.10" 

# Q-1.16
paste(c("Hello", "World"), 1:10, sep="-")    # Output: "Hello-1"  "World-2"  "Hello-3"  "World-4"  "Hello-5"  "World-6"  "Hello-7"  "World-8"  "Hello-9"  "World-10"
print(paste("Hello", 1:10, sep="-"))         # Output: "Hello-1"  "Hello-2"  "Hello-3"  "Hello-4"  "Hello-5"  "Hello-6"  "Hello-7"  "Hello-8"  "Hello-9"  "Hello-10"

# Generating sequences -
# Q-2.1 (generates a seq. from 0 to 10)
0:10         # Output: 0  1  2  3  4  5  6  7  8  9 10 

# Q-2.2 (generates a seq. from 15 to 5 - in decreasing order)
15:5         # Output: 15 14 13 12 11 10  9  8  7  6  5

# Q-2.3 (generates a seq. from 0 to 1.5 with step size of 0.1)
seq(0,1.5,0.1)   # Output: 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0 1.1 1.2 1.3 1.4 1.5

# Q-2.4 (generates a decreasing seq. from 6 to 4 with step size of -0.2)
seq(6,4,-0.2)    # Output: 6.0 5.8 5.6 5.4 5.2 5.0 4.8 4.6 4.4 4.2 4.0

# Q-2.5 (c() - concatenate function - creates a numeric vector & assigns it to 'N') 
N <- c(55,76,92,103,84,88,121,91,65,77,99)
print(N)    # Output: 55  76  92 103  84  88 121  91  65  77  99

# Q-2.6
sequence_val <- seq(from=0.04, by=0.01, length=11)     # creates a sequence starting from 0.04, increasing by 0.01, generating 11 values
print(sequence_val)      # Output: 0.04 0.05 0.06 0.07 0.08 0.09 0.10 0.11 0.12 0.13 0.14 
plot(sequence_val, N)
N <- 1:11
seq(0.04,by=0.01,along=N)   # Same output as above

# Q-2.7
seq(from=0.04, to=0.14, along=N)     # Yes, it matches with the above sequence

# Q-2.8 (sequence() - to make a vector of sequences of unequal lengths)
sequence(c(4, 3, 4, 4, 4, 5))      # 1 2 3 4 1 2 3 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 5

# Q-2.9
# The first argument of rep() is the 'vector', second argument is 'times' by default & the third argument is ‘each’
rep(9, 5)          # Output: 9 9 9 9 9
rep(1:4, 2)        # Output: 1 2 3 4 1 2 3 4
rep(1:4, each=2)   # Output: 1 1 2 2 3 3 4 4
rep(1:4, each=2, times=3)   # Output: 1 1 2 2 3 3 4 4 1 1 2 2 3 3 4 4 1 1 2 2 3 3 4 4
rep(1:4, 1:4)    # Output: 1 2 2 3 3 3 4 4 4 4

# Q-2.10
rep(1:4, c(4,1,4,2))    # Output: 1 1 1 1 2 3 3 3 3 4 4
rep(c("cat","dog","goldfish","rat"), c(2,3,2,1))     # Output: "cat"      "cat"      "dog"      "dog"      "dog"      "goldfish" "goldfish" "rat" 

# Q-2.11 (Both generates a sequence from -1 to 1 in steps of 0.1)
seq(-1, 1, by=0.1)    # Output: -1.0 -0.9 -0.8 -0.7 -0.6 -0.5 -0.4 -0.3 -0.2 -0.1  0.0  0.1  0.2  0.3  0.4  0.5  0.6  0.7  0.8  0.9  1.0
seq(-1, 1, 0.1)       # Same output as above

# Q-2.12 (Divides the interval from -1 to 1 into 7 evenly spaced values)
seq(-1, 1, length=7)    # Output: -1.000000000000000 -0.666666666666667 -0.333333333333333  0.000000000000000  0.333333333333333  0.666666666666667  1.000000000000000

# Q-2.13 (Generate a set of numbers from -1 to 1 with an interval of 0.1 without using seq())
x <- (-10:10) * 0.1
x     # Output: -1.0 -0.9 -0.8 -0.7 -0.6 -0.5 -0.4 -0.3 -0.2 -0.1  0.0  0.1  0.2  0.3  0.4  0.5  0.6  0.7  0.8  0.9  1.0

# Missing values, infinity and NaN, NA
# Q-3.1
3/0    # Output: Inf

# Q-3.2 (Exponentiating negative infinity)
exp(-Inf)   # Output: 0

# Q-3.3
(0:3)**Inf   # Output: 0   1 Inf Inf

# Q-3.4
0/0     # Output: NaN

# Q-3.5
Inf - Inf    # Output: NaN

# Q-3.6
Inf / Inf    # Output: NaN

# Q-3.7 
is.finite(10)    # Output: TRUE (since, 10 is a finite no.)

# Q-3.8
is.infinite(10)   # Output: FALSE

# Q-3.9
is.infinite(Inf)    # Output: TRUE 

# Q-3.10
y <- c(4, NA, 7)
y=="NA"     # Output: FALSE    NA FALSE
is.na(y)    # Output: FALSE  TRUE FALSE   (is.na() - correctly identifies missing values)

# Q-3.11
y[!is.na(y)]    # Strip y of the NA entry (Output: 4  7)

# Q-3.12
c1 <- c(1, 2, 3, NA)
c2 <- c(5, 6, NA, 8)
c3 <- c(9, NA, 11, 12)
c4 <- c(NA, 14, 15, 16)
full.frame <- data.frame(c1, c2, c3, c4)
full.frame
reduced.frame <- full.frame[! is.na(full.frame$c1),]     # This will remove the row containing NA with respect to column 1
reduced.frame    
mean(full.frame$c1)    # Output: NA (mean(x) - will not work in case of missing values, so alternative way is given below)
mean(full.frame$c1, na.rm=T)    # Output: 2 (bec; (1+2+3)/2=2) 

# Q-3.13 (Get the indices of a vector where NA is present)
v <- c(1:6, NA, NA, 9:12)
v
seq(along=v)[is.na(v)]   # Output: 7  8  (seq(along=v) - generates indices, filtered by is.na(v))
which(is.na(v))    # Same output as above  (which() - directly finds the indices of TRUE values)
