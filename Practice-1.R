######### LAB-1 PRACTICE #########

print(2.7/2)  # division of floating point no. will result in floating-point value
print(2.7 %/% 2)   # gives quotient (only the integer part taken)
print(2.7 %% 2)   # gives remainder (2.7 - (1*2)=0.7)
print((10+5*1i)/2)   # can be written as (10+5i)/2 = 5+2.5i
print(round(2.5))  # follows 'bankers' rounding , i.e., rounds off to the nearest even no. when halfway (here, b.w 2 and 3; 2 is even)
print(round(-2.5))   # same rule applies here also
print(round(-3.2))   # no halfway, so normal rounding up to -3
print(round(-3.7))   # normal rounding up to -4
print(2 %/% 4 -1)    # 2 %/% 4 = 0.5 ; only integer part; 0-1=-1
print(3 * 2**2)   # 2^2=4 ; 4*3=12
print(3*2^2)   # same as above
print(3**2*2)    # 3^2=9; 9*2=18
print(7 %/% 4)   # quotient = 1.75 ; truncated to 1
print(7 %% 4)   # gives remmainder (7-(1*4)=3)
print(-7 %% 4)  # R follows Euclidean division, i.e., remainder has same sign as divisor (so, closest  multiple of 4 less than -7 is -8, so -8+1=-7; output: 1)
print(trunc(5.7))   # trunc(x) - removes the decimal part without rounding.
print(trunc(-5.7))

print(ceiling(5.7))   # returns smallest integer greater than or equal to x (output: 6)
ceil_alt <- function(x){floor(x+0.5)}   # floor(5.7+0.5) = floor(6.2) = 6
print(paste('Alternate ceiling function: ', ceil_alt(5.7)), quote=FALSE)  # paste - convert into string; print with quote=FALSE - removes quotes from string

a <- 1
b <- 2
c <- 4
print(a&b)   # TRUE & TRUE = TRUE ; bec. a & b are non-zero 
print(! a<b | c>b)   # not a<b or c>b ; FALSE | TRUE = TRUE

x <- c(5,2,7,8)   # returns a vector object having 4 input objects (concatenate function)
print(typeof(x))   # all the no.(s) are of 'double' type
is.integer(x)   # FALSE 
is.numeric(x)   # TRUE
x <- integer(length(x))   # create a integer vector of zeros of same length
print(x)   # 0 0 0 0
x <- c(5,2,7,8)
as.integer(x)    # converts 'double' vector into integer vector
is.integer(x)   # TRUE (integer vector due to as.integer())

x <- sqrt(2)
print(x*x == 2)   # FALSE: equality needs exact no. not the floating point no.
print(x*x - 2)    # small error due to rounded off representation
print(all.equal(x*x, 2))    # TRUE: accounts for small floating-point errors; checks whether the 2 values are close enough to be considered equal.


####### QUIZ PRACTICE #######

# For any input integer k and another integer input n where n/=0 write a function that prints the results of all arithmetic operations between k and n. 
arith_ops <- function(k, n){
  sum_kn <- k+n
  diff_kn <- k-n
  pro_kn <- k*n
  div_kn <- k/n
  mod_kn <- k%%n  
  exp_kn <- k^n
  int_div <- k%/%n
  output = c(sum_kn, diff_kn, pro_kn, div_kn, mod_kn, exp_kn, int_div)
  return(output)
}
result = arith_ops(2, 3)
print(paste('Sum=', result[1]), quote=FALSE)
print(paste('Difference=', result[2]), quote=FALSE)
print(paste('Product=', result[3]), quote=FALSE)
print(paste('Quotient=', result[4]), quote=FALSE)
print(paste('Remainder=', result[5]), quote=FALSE)
print(paste('Exponent=', result[6]), quote=FALSE)
print(paste('Integer division=', result[7]), quote=FALSE)

# Input real numbers a,b and c.Treating these as coefficients of the quadratic equation ax2+bx+c=0, find and print the roots.
a <- 1
b <- 3
c <- 2
find_quad_roots <- function(a, b, c){
  discriminant <- sqrt(b^2-4*a*c)
  root1 <- (-b-discriminant)/(2*a)
  root2 <- (-b+discriminant)/(2*a)
  return(c(root1, root2))
}
result=find_quad_roots(a, b, c)
print(paste('Roots of quadratic equation are: ', result[1], ',', result[2]), quote=FALSE)    
# print(c('The two roots are : ',result[1],',',result[2]),quote=FALSE): not to be used; bec. it creates avector & prints its each element in separate line with index.


####### LAB-2&3 PRACTICE #######

print(round(12.1343, digits=3))     # round(x, digits=n) - round off x to n decimal places
print(round(123.12344, digits=3))
print(round(1234.12344, digits=3))
print(round(12345.12344, digits=3))   # output: 12345.12 & not 12345.123; bec. in case of large no., though it rounds off to .123, but it shows .12, as R is limited in how many digits it shows.




