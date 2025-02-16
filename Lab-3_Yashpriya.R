### LAB-3 ( DATE: 13.1.25)

# Q-4.1 (Vector assignment and queries)
vec <- c(4, 7, 6, 5, 6, 7)
class(vec)     # Output: "numeric"
length(vec)    # Output: 6
min(vec)       # Output: 4
max(vec)       # Output: 7

# Q-4.2 (Vector creation using keyboard input)
# an extra 'Enter' so as to tell R that the vector is now complete
vec1 <- scan()
4 
7
6
5
6
7
      
print(vec1)     # Output: 4 7 6 5 6 7

# Q-4.3 (Vector subscripts)
vec[4]    # Indexing starts from 1 unlike in Python where it is 0 (Output: 5)

# Q-4.4 (Extracting multiple elements using two ways)
ind <- c(2, 3, 6)
vec[ind]    # Output: 7 6 7 (these are the elements present at index 2, 3 & 6 respectively in the vector)
vec[c(2, 3, 6)]   # Same output as above

# Q-4.5 (Drop elements using minus symbol)
vec[-1]   # will remove the 1st element from vec (Output: 7 6 5 6 7)

# Q-4.6
vec[-length(vec)]   # removes the last element of vec (Output: 4 7 6 5 6)

# Q-4.7 (to remove the largest two and smallest two values from a vector)
trim <- function(x) sort(x) [(c(-1, -2, -(length(x)-1), -length(x)))]
trim(vec)    # Output: 6 6 (These are the remaining values after trimming)

# Q-4.8 (Use sequences to extract elements)
vec[1:3]   # Output: 4 7 6 (first three elements of the vector)
vec[seq(2, length(vec), 2)]    # Output: 7 5 7 (starting from index-2 to the length of vector, it will display every alternate element b/w this range)
vec[1:length(vec)%%2==0]    # Alternative way to get the elements present at even indices

# Q-4.9 (logical subscripts)
x <- 0:10
sum(x[x<5])    # Output: 10 (finding the sum of values of x that are less than 5)
x <- 0:10      # Alternative way, but manual one 
y <- x[x < 5]
z <- y[1] + y[2] + y[3] + y[4] + y[5]
z     # Output: 10    

# Q-4.10 (to find out the sum of three largest values in a vector)
vec <- c(14, -7, 6, 51, 9, 7)
sum(sort(vec, decreasing=TRUE)[1:3])     # Output: 74 (51+14+9=74) - sorts x in descending order, then selects top three values & adds them

# Q-4.11 (Finding index of vector corresponding to max. or min. value)
which.max(x)   # Output: 11
which.min(x)   # Output: 1

# Q-4.12 (Combining vectors as columns or rows)
cbind(1:10, 10:1)     # 10 by 2 matrix (stacks vectors vertically)
rbind(1:10, 10:1)     # 2 by 10 matrix (stacks vectors horizontally)

# Q-4.13.1 (Basic operations with vectors)
X <- c(1:10)
X            # Output: 1  2  3  4  5  6  7  8  9 10
# Q-4.13.2
Y <- c(1:10*5)    # Each element of a vector is multiplied by 5
Y            # Output: 5 10 15 20 25 30 35 40 45 50
# Q-4.13.3
X*Y        # Output: 5  20  45  80 125 180 245 320 405 500 (each element of X vector is multiplied by its corresponding element in Y vector - just like 'zip()' in Python)
# Q-4.13.4
X+Y        # Output: 6 12 18 24 30 36 42 48 54 60 (each element of X vector is added with its corresponding element in Y vector)
# Q-4.13.5
X/Y     # Output: 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 (gives quotient)
# Q-4.13.6
X^Y    # X raise to power Y (element-wise exponentiation)
# Q-4.13.7
log(X)   # Natural logarithm of X
# Q-4.13.8
exp(Y)   # Exponential of Y

# Q-5 (Matrices/Dataframes/Arrays)
# Matrices are 2D arrays containing numbers.
# Dataframes are 2D lists containing potentially a mix of numbers, text or logical variables in different columns.
# First subscript refers to row, and second subscript refers to the column index.
y <- 1:24
dim(y) <- c(2,4,3)   # 2 by 4 by 3 (3D array)
y  
dim(y) <- c(3, 2, 4)   # will be rearranged into 3-D array (3 by 2 by 4)
y

# Q-5.1 (matrix() - converts vectors to matrices)
X <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow=3)    # 3 by 3 identity matrix
print(X)     # [,1] means "all of" row 1

# Q-5.2
# Alternative way - to provide the vector object with two dimensions (rows and columns) using dim()
vector <- c(1, 2, 3, 4, 4, 3, 2, 1)
V <- matrix(vector, byrow=T, nrow=2)   # method-1 (matrix filled row-wise)
dim(vector) <- c(4,2)    # method-2
is.matrix(vector)     # Output: TRUE (checks if vector is a matrix)

# Vector functions - one can evaluate functions over entire vectors without the need to invoke loops and subscripts
# Q-6.1
vec <- c(2, 56, -4, 0, -8, 65)
min(vec)    # Output: -8
max(vec)    # Output: 65
sum(vec)    # Output: 111
range(vec)  # Output: -8 65
sort(vec)   # Output: -8 -4  0  2 56 65

# Q-6.2 (colMeans(M) - evaluate the mean of each column of dataframe/matrix 'M')
M <- matrix(1:9, nrow = 3) 
M     # creates 3 by 3 matrix & it is filled column-wise by default
colMeans(M)    # Output: 2 5 8  

# Q-6.3 (Matrix multiplication)
X <- 1:4
Y <- 1:3
Z <- X %o% Y     # Outer product of X and Y - multiplication of each element in X with each element in Y
Z                # no. of rows = no. of elements in X, and, no. of columns = no. of elements in Y

YoX <- Y[1:3] %o% X[1:4]    # Reverse the order of X and Y in the outer product
YoX       # no. of rows = no. of elements in Y, and, no. of columns = no. of elements in X

t(Z)      # transpose of Z (swaps the rows and columns of a matrix) - Output: 3 by 4 matrix
t(YoX)    # Output: 4 by 3 matrix

X %*% Y     # dot product (Output: Error - non-conformable arguments) - bec; X is a row vector (1 by 4) and Y is also a row vector (1 by 3), but we need X to be row vector & Y to be column vector 
# alternative way for dot product (Warning: longer object length is not a multiple of shorter object length , so, R will recycle the elements of Y to match the length of X, i.e., Y=(1,2,3,1)
sum(X*Y)    # Output: 18 (i.e., (1*1)+(2*2)+(3*3)+(4*1)=18) -  element-wise multiplication & then, sum

crossprod(X[1:4], Z)    # Output: 1 by 3 matrix [30  60  90]

diag(4)   # 4 by 4 size identity matrix

# Q-6.4
class(X)    # Output: "matrix" "array"


### DATE: 27.1.25
# Lists / dataframes -
Xvec <- c("Yash", "Priya", 119, "Biotechnology", 2000, 2003, 2006, 2012, 2017, 2023)        # Create a character vector with mixed elements
Xvec
Mylist <- list(name="YPB",mixedlist = Xvec,numbers=1:10)     # Create a list with named elements: a string, a vector, and a sequence
Mylist
Mylist$numbers    # Access the 'numbers' element of the list using $
Mylist[3]         # Access the third element of the list as a sublist
Mylist[[3]]       # Extract the content of the third element (not as a sublist; but the actual data type of the content)
