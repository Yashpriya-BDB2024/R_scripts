# Arithmetic operations in R -

print(10+2)
print(10-2)
print(10*2)
print(10/2)
print(10^2)   

# print() used for long scripts.
# paste() converts the argument into a string.
X <- 2
print(X)  # Output: 2
paste(X)  # Output: "2"

# Concatenate function c() gives an object k/a "vector".
print(c(1,2,3,4))  # Output: 1 2 3 4

# Math functions in R -
print(abs(-10))  # absolute value (positive)
print(sqrt(10))
print(sqrt(10-1))  # sq. root of 9
print(log(10))  # log base e
print(log10(10))  # log base 10
print(log2(10))  # log base 2
print(log(10,2))  # log to base 2 of 10 (same as above)
print(exp(1))   # antilog of 1, i.e., e raise to the power 1

print(sin(pi/2))  # pi is constant in R = 3.14
print(asin(1))    
print(cos(pi))
print(acos(-1))
print(tan(0))
print(atan(0))

print(factorial(10))
# choose(n, x) : binomial coefficients , i.e., n! / x!(n-x)!
print(gamma(x))
print(lgamma(x))  # natural log of gamma(x)

print(floor(x))   # greatest integer less than x
print(ceiling(x))  # smallest integer greater than x
print(trunc(x))  # closest integer to x b.w x & 0
print(round(x, digits=0))
