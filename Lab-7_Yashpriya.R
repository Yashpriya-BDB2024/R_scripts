### LAB-7 (DATE: 14.2.25)

# Q-1 (Solving matrix equations and review of matrix operations.)
# Q-1.1 (Form a matrix amat of dimensions 3×4 containing numbers from 10 to 120 in steps of 10.)
amat <- matrix(seq(10, 120, by=10), nrow=3, ncol=4, byrow=TRUE)     # Filled row-wise
print(amat)
amat2 <- matrix(seq(10, 120, by=10), nrow=3, ncol=4, byrow=FALSE)   # Filled column-wise
print(amat2)

# Q-1.2 (Assign the row names to amat as R1, R2 and R3 and column names as C1, C2, C3, C4.)
rownames(amat) <- c('R1', 'R2', 'R3')
colnames(amat) <- c('C1', 'C2', 'C3', 'C4')
print(amat)

# Q-1.3 
matrix_A <- matrix(c(2,5,7,3,1,8,9,10,1,12,5,10,4,17,15,11), nrow=4, ncol=4, byrow=TRUE)
print(matrix_A)
matrix_B <- matrix(c(12,5,3,17,1,18,9,10,1,12,5,10,4,15,15,4), nrow=4, ncol=4, byrow=TRUE)
print(matrix_B)
element_wise_product <- matrix_A * matrix_B    # Element-wise multiplication of the matrices A & B
print(element_wise_product)
matrix_matrix_product <- matrix_A %*% matrix_B   # Matrix-matrix multiplication 
print(matrix_matrix_product)

# Q-1.4
X <- c(5,6,8,9)
Y <- c(8, 10, 12, 5)
outer_product <- X %o% Y    # Method-1
print(outer_product)
outer_product1 <- outer(X, Y)   # Method-2
print(outer_product1)
inner_product <- X %*% Y    # Method-1
print(inner_product)     # Output: 241
inner_product1 <- crossprod(X, Y)   # Method-2
print(inner_product1)    # Output: 241

# Q-1.5 (Form a diagonal matrix using the entries of the vector X above.)
diagonal_matrix <- diag(X)
print(diagonal_matrix)

# Q-1.6 (Print the diagonal elements of above diagonal matrix.)
diag_A <- diag(diagonal_matrix)
print(diag_A)

# Q-1.7 (Create an identity matrix of dimensions 6x6 in one line.)
identity_matrix <- diag(6)
print(identity_matrix)

# Q-1.8 (Create a 3x3 matrix 'A' using the elements 3,4,-2,4,-5,1,10,-6,5 with default options.)
A <- matrix(c(3,4,-2,4,-5,1,10,-6,5), nrow=3, ncol=3)   # By default, R will fill the elements column-wise (byrow = FALSE)
print(A)  

# Q-1.9 (Create a 3x1 matrix 'B' with elements 5,-3,13.)
B <- matrix(c(5, -3, 13), nrow=3, ncol=1)
print(B)

# Q-1.10 (Find the unknown vector 'X' of the equation AX=B using the command X=solve(A,B). Print X to see the results. What type of object is X?)
X = solve(A, B)
print(X)
print(typeof(X))    # Output: "double" (gives the data type of the object)

# Q-1.11 (Find the inverse of matrix A by using the command Ainv = solve(A).)
Ainv = solve(A)
print(Ainv)  
identity_mat <- Ainv %*% A   # Verify by multiplying Ainv with A (gives the identity matrix)
print(identity_mat)

# Q-1.12 (Find the eigenfunctions and eigenvalues of A by using the command eigen(A).)
results <- eigen(A)
print(results)
print(typeof(results))
second_eigen_vector <- results$vectors[,2]   # Extracting the 2nd eigen vector
print(second_eigen_vector)
matrix_vector_product <- A %*% second_eigen_vector    # matrix-vector multiplication 
print(matrix_vector_product)
# Verifying the above output -
# Original matrix (A) * 2nd eigen vector = 2nd eigen value * 2nd eigen vector
second_eigen_val <- results$values[2]
print(second_eigen_val)
expected_output <- second_eigen_val * second_eigen_vector
print(expected_output)      # matrix_vector_product = expected_output

# Q-2 (Removing a column from a data frame.)
data = read.csv("/home/ibab/Downloads/BrainCancer.csv", header=TRUE)   
print(data)

# Q-2.1
sq_added_to_time <- data$time + (data$gtv**2)
print(sq_added_to_time)
data$gtv_sq_added_to_time <- sq_added_to_time
print(data)

# Q-2.2 (Print the row and column names of the modified data.)
print(rownames(data))
print(colnames(data))

# Q-2.3 (Change the row names of the data such that each row has ’Row-’ followed by the row no.)
rownames(data) <- paste("Row-",1:88, sep="")
print(rownames(data))

# Q-2.4 (Remove the column ’ki’ by assigning NULL value to this column.)
data$ki <- NULL
print(data)

# Q-3 (Reading excel files)
# Q-3.1 (Install the package called readxl by using the command install.packages("readxl").)
install.packages("readxl")

# Q-3.2 (Load the package in the current R environment by using the command library(readxl).)
library(readxl)

# Q-3.3 (create a dataframe of the excel file)
data1 <- read_excel("/home/ibab/Downloads/pone.0148733.s001.xlsx", 1)    # The 1 stands for the sheet no. in the excel file.    
print(data1)

# Q-3.4 (Print the column names and dimensions of the data in the 'data1' data frame.)
print(colnames(data1))
print(dim(data1))     # Output: 88  9

# Q-4 (Sets and operations on sets)
# Q-4.1 
A <- c("a","b","c","d","e")
print(A)
B <- c("d","e","f","g")
print(B)

# Q-4.2 (Perform a union operation between the two sets.)
print(union(A, B))      # Output: "a" "b" "c" "d" "e" "f" "g"   # Method-1

# Q-4.3 (Perform an intersection operation between the two sets.)
print(intersect(A, B))    # Output: "d" "e" (common in both A and B)

# Q-4.4 (Perform a difference operation between the two sets.)
print(setdiff(A, B))    # Output: "a" "b" "c" (Those elements of A that are not in B)
print(setdiff(B, A))    # Output: "f" "g" (Those elements of B that are not in A)

# Q-4.5 (The function setequal() checks for the equality of two objects.)
print(c(setdiff(A, B), intersect(A, B), setdiff(B, A)))    # Method-2 (Output: "a" "b" "c" "d" "e" "f" "g")
print(setequal(c(setdiff(A, B), intersect(A, B), setdiff(B, A)), union(A, B)))     # Method-3 (Output: TRUE)

# Q-4.6 (List the elements of B present in A using two different approaches.)
print(intersect(B, A))    # Approach-1 (Output: "d" "e")
print(B %in% A)           # Output: TRUE TRUE FALSE FALSE
print(B[B %in% A])        # Approach-2 (Output: "d" "e")

# Q-4.7 (Print the elements of A present in B.)
print(intersect(A, B))    # Approach-1 (Output: "d" "e")
print(A %in% B)           # Output: FALSE FALSE FALSE TRUE TRUE
print(A[A %in% B])        # Approach-2 (Output: "d" "e")

# Q-5 (Practice with subsets)
# Q-5.1 
vec <- c(8,10,12,7,14,16,2,4,9,19,20,3,6)
greater_than_12 <- vec[vec > 12]
print(greater_than_12)      # Output: 14 16 19 20
greater_than_10_and_less_than_20 <- vec[vec>10 & vec< 20]
print(greater_than_10_and_less_than_20)    # Output: 12 14 16 19

# Q-5.2 
A <- c(2,7,29,32,41,11,15,NA,NA,55,32,NA,42,109)
new_A <- A[!is.na(A) & A<100]      # NA removed & values less than 100
print(new_A)     # Output: 2 7 29 32 41 11 15 55 32 42

# Q-5.3 (Assign the value 0 to the elements ’NA’ in A and print the new array.)
new_A2 <- A
new_A2[is.na(new_A2)] <- 0
print(new_A2)

# Q-5.4 (Create a vector with gene names “gene-1”,“gene-2” . . . “gene-6”.)
# genes <- paste("gene", 1:6, sep="-")    # Will throw an error while creating the dataframe - 'differing no. of rows' 
genes <- paste("gene", 1:7, sep="-")
print(genes)
# Create a vector for gender with entries M,M,F,M,F,F,M.
gender <- c("M","M","F","M","F","F","M")
print(gender)

# Q-5.5 
result1 = c(12.3, 11.5, 13.6, 15.4, 9.4, 8.1, 10.0)
result2 = c(22.1, 25.7, 32.5, 42.5, 12.6, 15.5, 17.6)
result3 = c(15.5, 13.4, 11.5, 21.7, 14.5, 16.5, 12.1)
result4 = c(14.4, 16.6, 45.0, 11.0, 9.7, 10.0, 12.5)
result5 = c(12.2, 15.5, 17.4, 19.4, 10.2, 9.8, 9.0)
result6 = c(13.3, 14.5, 21.6, 17.9, 15.6, 14.4, 12.0)
result7 = c(11.0, 10.0, 12.2, 14.3, 23.3, 19.8, 13.4)

# Q-5.6 (Create a dataframe with the following columns: genes, gender, result1, result2, result3, result4, result5, result6, result7.)
datframe <- data.frame(genes, gender, result1, result2, result3, result4, result5, result6, result7)
print(datframe)

# Q-5.7 (Add column names to this dataframe: “GeneName”, “Gender”, “expt1”, “expt2”, “expt3”, “expt4”, “expt5”, “expt6”, “expt7”.)
colnames(datframe) <- c("GeneName", "Gender", "expt1", "expt2", "expt3", "expt4", "expt5", "expt6", "expt7")
print(datframe)

# Q-5.8 (Create a subset of data with “expt2” values greater than 20)
expt2_greater_than_20 <- subset(datframe, expt2 > 20)
print(expt2_greater_than_20)

# Q-5.9 (Create a subset of data with only Female gender.)
female_subset <- subset(datframe, Gender == "F" )
print(female_subset)

# Q-5.10 (Create a subset of data with Male gender for which “expt2” is less than 30)
male_and_expt2_less_than_30 <- subset(datframe, Gender == "M" & expt2 < 30)
print(male_and_expt2_less_than_30)

# Q-6 (If-else-if structure)
# Q-6.1 (Write an if-else-if structure that explores and prints the quadrant in which an angle belongs. For example, if you input 45 degree it should print ‘First quadrant’.)
angle <- as.numeric(readline("Enter an angle (in degrees): "))    # Note: Firstly run only this line; enter input, then run rest of the if-else part. 

if (is.na(angle)) {
  print("Invalid input! Please enter a numeric value.")
} else if (angle >= 0 & angle < 90) {
  print("First quadrant")
} else if (angle >= 90 & angle < 180) {
  print("Second quadrant")
} else if (angle >= 180 & angle < 270) {
  print("Third quadrant")
} else if (angle >= 270 & angle < 360) {
  print("Fourth quadrant")
} else {
  print("Invalid angle (it should be between 0 and 360 degrees)")
}

# Q-6.2 (Write an if-else-if structure that takes 3 numeric inputs & uses this str. alone to put the 3 numbers in decreasing order.)
# Note: Run all the below three lines one by one to enter the inputs; and then run the if-else part all together.
num1 <- as.numeric(readline("Enter the first no.: "))  
num2 <- as.numeric(readline("Enter second no.: "))  
num3 <- as.numeric(readline("Enter third no.: "))  

if (num1 >= num2 & num1 >= num3) {      # Check if num1 is the largest no.
  if (num2 >= num3) {         # If num1 is largest, check if num2 is greater than or equal to num3
    print(c(num1, num2, num3))  
  } else {  
    print(c(num1, num3, num2))  
  }  
} else if (num2 >= num1 & num2 >= num3) {      # Check if num2 is the largest number
  if (num1 >= num3) {        # If num2 is largest, check if num1 is greater than or equal to num3
    print(c(num2, num1, num3))  
  } else {  
    print(c(num2, num3, num1))   
  }  
} else {       # If neither num1 nor num2 is the largest, then num3 must be the largest.
  if (num1 >= num2) {       # Check if num1 is greater than or equal to num2
    print(c(num3, num1, num2))    
  } else {  
    print(c(num3, num2, num1))   
  }  
}  

# Q-6.3 
# Let’s say the cost of a journey ticket depends not only on the distance travelled but also on the details of the traveller. Distance-wise, the cost is a min. of Rs.
# 100 for the first 100km, Rs. 1.50 for every extra km until 1000km and Rs.2 per km thereafter. On top of that, senior citizens (> 60 years ) get a 25% concession
# and children under 6 years of age get 50% concession. Write a code that takes the journey distance and the traveller’s age as inputs, and prints out the ticket cost.
distance <- as.numeric(readline("Enter the journey distance (in km): "))
age <- as.numeric(readline("Enter the traveller's age: "))

if (is.na(distance)) {
  stop("Invalid! No distance entered.")     # stop() - If the user enters no input or an invalid value, execution stops immediately with an error message.
} else if (distance <= 0) {
  print("Invalid input! Please enter valid no. as distance.")
} else {
  if (distance <= 100) {
    cost <- 100
  } else if (distance <= 1000) {
    cost <- 100 + (distance - 100) * 1.50
  } else {
    cost <- 100 + (900 * 1.50) + (distance - 1000) * 2
  }
}
if (is.na(age)) {
  stop("Invalid! No age entered.")
} else {
  if (age > 60) {
    cost <- cost * 0.75    # 25% concession given
  } else if (age < 6) {
    cost <- cost * 0.50    # 50% concession given
  }
}
print(paste("Ticket cost: Rs.", round(cost, 2)))

# Q.7 (Writing functions)
# Q-7.1 (Write a function to replace all the negative values in a vector by zeros.)
replace_neg_with_zero <- function(vec) {
  vec[vec < 0] <- 0
  return (vec)
}
replace_neg_with_zero(c(2, -1, NA, 55, -32, -8, NA, NA, 3))     # Example (Output: 2  0  NA  55  0  0  NA  NA  3)

# Q-7.2 (Write a function to calculate the factorial of a no. using the Stirling’s approximation.)
factorial_stirling_approx <- function(n) {
  if (n < 0 || !is.numeric(n)){
    stop("Enter non-negative no.")
  } else {
    if (n == 0) {    # 0! = 1
      return (1)
    } else {
      n_fac <- (n**n)*(exp(-n))*(sqrt(2*pi*n))*(1+(1/(12*n))+(1/(288*n**2))-(139/(51840*n**3))-(571/(2488320*n**4)))
    }
  }
  return (n_fac)
}
factorial_stirling_approx(5)     # Example (Output: 120)

# Q-7.3 (Write a function to sum the digits of a number.)
add_digits <- function(num) {      
  num <- abs(num)     # abs() - take absolute value to handle negative numbers.
  sum <- 0
  while (num > 0){
    sum <- sum + (num %% 10)     # Extracts the last digit and add to sum.
    num <- num %/% 10     # Removes the last digit
  }
  return (sum)
}
add_digits(17122002)    # Example (Output: 15)
