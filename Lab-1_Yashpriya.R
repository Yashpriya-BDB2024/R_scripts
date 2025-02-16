# LAB-1 (Date: 3.1.25)

# Q-1.1
a <- 2.7/2    # '/' gives quotient
print(a)      # Output: 1.35

# Q-1.2
b <- 2.7%/%2   # '%/%' - integer division
print(b)       # Output: 1 ; bec. the quotient, i.e., 1.35 will be truncated, leaving just the integer part.
b <- 2.7%%2    
print(b)       # '%%' - gives the remainder (here 0.7)

# Q-1.3
c <- 10+5i/2   # Mixed arithmetic operation (both real & imaginary parts)
print(c)       # Output: 10+2.5i (here, firstly 5i is divided by 2, giving 2.5i & 10 is added to it)

# Q-1.4
d <- round(2.5)    # round() - rounds a no. to the nearest integer
print(d)           # Output: 2 ; bec. it uses round-half-to-even, i.e., instead of rounding off to 3, it will round off to 2 (nearest even integer)

# Q-1.5
e <- round(-2.5)  
print(e)           # Output: -2 

# Q-1.6
f <- 2%/%4-1   # %/% - integer division
print(f)       # Output: -1 ; bec. 2%/%4 = 0 and then 0-1 = -1

# Q-1.7
g <- 3*2**2 
print(g)       # Output: 12 
g <- 3*2^2     # ** or ^ - exponentiation operator - will give same output
print(g)       # Output: 12 ; bec. 2^2 = 4 ( 2 raise to power 2) and then 4*3 = 12 (multiplication)

# Q-1.8
h <- 3**2*2
print(h)       # Output: 18 ; based on higher precedence of exponentiation over multiplication ; 3**2=9 and then 9*2=18

# Q-1.9
i <- 7%/%4     # %/% - integer division (returns only integer part of the quotient)
print(i)       # Output: 1 

# Q-1.10
j <- 7%%4      # %% - gives the remainder
print(j)       # Output: 3

# Q.1.11
k <- -7%%4     # Result of modulus operation should have the same sign as that of divisor.
print(k)       # Output: 1 ; bec. the remainder is actually -3, but to have same sign as 4 (divisor), it adds 4 to -3 

# Q.1.12
l <- trunc(5.7)  # trunc() - It doesn't round-off, rather just truncates it, thereby discarding the decimal part 
print(l)         # Output: 5

# Q.1.13
m <- trunc(-5.7)  # trunc() works similarly for negative no. 
print(m)          # Output: -5

# Q-2
# It mimics the ceiling function (rounding a no. up to nearest integer), i.e., ceiling(5.7) = 6
ceil_floor_func <- function(x) floor(x+0.5)       # floor() - rounds down the value to the nearest integer
ceil_floor_func(5.7)     # Output: 6 ; bec 5.7+0.5=6.2 & floor(6.2)=6

# Q-3
a <- 1   
b <- 2
c <- 4
# Q-3.1
d <- a & b     # a and b
print(d)       # Output: TRUE (since both are non-zero (true), so results in TRUE)

# Q-3.2
e <- !(a<b) | (c>b)    # not a<b or c>b
print(e)               # Output: TRUE (a<b is TRUE, but ! negates it making it FALSE, and c>b, i.e., TRUE; since one of the two is TRUE so results in TRUE)

# Q-4.1
x <- c(5,3,7,8)    # c() - concatenate function - creates a numeric (double data type) vector
print(x)           # Output: 5 3 7 8

# Q-4.2
# is.integer() checks if 'x' is of integer data type 
is.integer(x)     # Output: FALSE (bec., 'x' is a numeric (double) vector)
# Q-4.3
is.numeric(x)     # Output: TRUE 

# Q-4.4
x <- integer(x)     # integer() function is not intended to convert the existing data; rather it creates a vector of zeroes of specified length.
print(x)            # Output: Error in integer(x) : invalid 'length' argument (bec; 'x' is not a single value)

# Q-4.5
x <- c(5,3,7,8)    
is.integer(x)
is.numeric(x)
x <- as.integer(x)    # as.integer() function converts the values in 'x' to the integer data type
is.integer(x)         # Output: TRUE

# Q-5.1
x <- sqrt(2)
print(x)        # Output: 1.414214

# Q-5.2
# == checks for exact equality
# Theoretically, this should return exactly 2, but due to floating point precision errors, it gives slightly less than or greater than 2, so returns FALSE.
# Real no. is stored in binary format, which can't exactly represent some decimal no. like sqrt(2), so leads to floating-point errors.
x*x == 2        # Output: FALSE

# Q-5.3
x*x - 2         # Output: 4.440892e-16 (small error due to rounded-off representation of real no.)

# Q-5.4
# all.equal(x, y) checks whether the two values are close enough to be considered equal; while accounting for small floating-point errors.
all.equal(x*x, 2)     # Output: TRUE  
