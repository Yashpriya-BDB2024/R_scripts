### R-QUIZ (15.1.25)

# Question-1
# For any input integer 'k' and integer 'n' not equals to 0, write a function that prints the results of all the arithmetic operations between k and n.
arithmetic_func <- function(k, n) {    # creating a function
  sum <- k+n
  difference <- k-n
  product <- k*n
  quotient <- k/n
  remainder <- k %% n
  exponent <- k^n     # k raise to power n
  result <- c(sum, difference, product, quotient, remainder, exponent)     
  print("Sum, difference, product, quotient, remainder, exponent of k=2 and n=4 are:")
  return (result)
}
arithmetic_func(2, 4)   # calling the function by passing the arguments

# Question-2
# Input real numbers a, b, c. Treating these as coefficients of the quadratic equation ax^2+bx+c=0. Find and print the roots.
quadratic_func <- function(a, b, c) {      # creating a function
  D <- b^2 - 4*a*c      # Formula to calculate find roots/ solutions
  root_1=(-b+(D^0.5))/2*a
  root_2=(-b-(D^0.5))/2*a
  result <- c(root_1, root_2)     # Storing the output 
  print("The roots of the quadratic equation are:")
  return (result)
}
quadratic_func(1, 2, -15)  # calling the function by passing the arguments