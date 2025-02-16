### LAB-6 (7.2.25)
### "BRAIN CANCER" dataset -

data = read.csv("/home/ibab/Downloads/BrainCancer.csv", header=TRUE)   
print(data)

# Q-5 (Manipulating the ‘factors’ in existing data)
# Q-5.1 (Change the current class of the headers in the Brain Cancer data to the class ‘factor’ and check that it is indeed the factor class.)
data$sex <- as.factor(data$sex)
print(is.factor(data$sex))    # Output: TRUE
data$diagnosis <- as.factor(data$diagnosis)
print(is.factor(data$diagnosis))   # Output: TRUE
data$loc <- as.factor(data$loc)
print(is.factor(data$loc))     # Output: TRUE
data$stereo <- as.factor(data$stereo)
print(is.factor(data$stereo))   # Output: TRUE

# Q-5.2 (Print the number of levels in the category ’Sex’ using nlevels() function)
print(nlevels(data$sex))    # Output: 2

# Q-5.3 (Print the levels in the categrory ’Diagnosis’ using the levels() function)
print(levels(data$diagnosis))   # Output: "HG glioma"  "LG glioma"  "Meningioma" "Other"

# Q-6 (Generating factor levels using gl() function)
# Q-6.1 (Generate a new category called 'Temperature' containing the same no. of elements that are no. of rows in the Brain Cancer data with labels 'Hot', 'Cold' & 'Lukewarm'.
Temperature <- gl(3, 29, 88, labels = c("Hot", "Cold", "Lukewarm"))
# 3: represents the no. of unique levels, 29: represents the no. of times each level is repeated consecutively, 88: total no. of elements
print(Temperature)

# Q-6.2 (Add this category of data to the above Brain Cancer dataframe and give it a new dataframe name. Print this new dataframe.)
new_data <- data.frame(data, Temperature)
print(new_data)

# Q-7 (Using the tapply() function.)
# Q-7.1 
# Each unique ki value acts as a category. So, The corresponding value is the mean data$gtv for all rows where data$ki matches that category.
tapply(data$gtv, data$ki, mean)
# Output: 40         60         70        80        90        100 
#         22.870000  15.744000  9.750714  9.093611  5.683333  8.646000 

# Q-7.2 (What does trim option do here?)
# trim=0.1: removes the lowest 10% & highest 10% values from each ki group before calculating the mean.
tapply(data$gtv, data$ki, mean, trim=0.1)  
# Output:  40        60         70        80        90        100 
#         22.870000  15.744000  8.567500  7.988000  4.494348  8.646000

subdata1 <- subset(data$gtv, data$ki==80)   # Alternative way
print(subdata1)
sorted_subdata <- sort(subdata1)
print(sorted_subdata)
mean_subdata_trimmed <- mean(sorted_subdata, trim=0.1)
print(mean_subdata_trimmed)    # Output: 7.988

# Q-8 (Use the pmin() and pmax() functions to find the minimum for each triplet set of 3 vectors data$gtv, data$ki and data$time)
print(pmin(data$gtv, data$ki, data$time))    # Parallel Minimum (finds the minimum value for each row across the three column)
print(pmax(data$gtv, data$ki, data$time))    # Parallel Maximum (finds the maximum value for each row across the three column)

# Q-9 (Difference between rank(), sort() and order().)
# Q-9.1 
# rank(): The lowest value gets rank 1, the second lowest gets rank 2, and so on. If there are ties, it assigns the average rank for those tied values.
# sort(): The original indices are lost, and only the sorted (in ascending order) values are displayed.
# order(): Returns indices that would sort in ascending order.
ranks <- rank(data$gtv)
sorted <- sort(data$gtv)
ordered <- order(data$gtv)
view <- data.frame(gtv=data$gtv, ranks, sorted, ordered)
print(view)

# Q-9.2
diagnosis_ordered <- data$diagnosis[ordered]    # Extracts diagnosis values in the same order as sorted gtv
print(diagnosis_ordered)
ordered_data <- data.frame(ordered_gtv = data$gtv[ordered], ordered_diagnosis = diagnosis_ordered)    # Creates a data frame with gtv and diagnosis, both sorted based on gtv
write.csv(ordered_data, "/home/ibab/Downloads/lab6_ordered_data_BrainCancer.csv")    # The ordered data is saved as a CSV file.
print(ordered_data)

# Q-10 (Converting data frames to matrices.)
# Q-10.1 (Extract the following rows and columns from the Brain cancer data: rows 1-6, columns 3-8.)
filter1 = data[1:6, 3:8]
print(filter1)

# Q-10.2 (Convert the class of this object into a matrix using as.matrix() function & check the attributes to make sure it is a matrix.)
filter1mat = as.matrix(filter1)
print(filter1mat)
print(class(filter1mat))    # Output: "matrix" "array" 
print(attributes(filter1mat))    # Output: $dim - 6 6 ; $dimnames[[1]] - "1" "2" "3" "4" "5" "6" ; $dimnames[[2]] - "diagnosis"  "loc"  "ki"  "gtv"  "stereo"  "status"   

# Q-10.3 (Create a newcol vector/column by adding 3 vectors - data$ki, data$gtv, data$time.)
newcol = data$ki + data$gtv + data$time   # Element-wise addition of three columns
print(newcol)

# Q-10.4 (Create a new dataframe by adding this column to the last, call this new dataframe as newcoladded.)
newcoldadded = data.frame(data, newcol)
print(newcoldadded)
print(colnames(newcoldadded))

# Q-10.5 (Alternative way - using cbind())
newcoladded2 = cbind(data, newcol)
print(colnames(newcoladded2))

# Q-10.6 (Pick rows 26 and 35 from the original data & add these are new rows to the original data using the data.frame() function.)
filter2 = data.frame(data[c(26, 35),])   
newrowadded = rbind(data, filter2)
print(newrowadded)     # Output: Row names as 261 & 351 ; R tries to maintain unique row names, often concatenating or modifying them to avoid duplication.
print(dim(newrowadded))    # Output: 90  9

# Q-11 (Adding row and column names.)
X <- matrix(c(1,0,2,5,3,1,1,3,1,3,3,1,0,2,2,1,0,2,1,0), nrow = 4, byrow = TRUE)
print(X)
print(rownames(X))   # Output: NULL (No row names set)
rownames(X) <- rownames(X, do.NULL=FALSE, prefix='Trial-')   # generates default row names with the given prefix ("Trial-")
print(rownames(X))
print(X)

# Q-11.1
drugs <- c("aspirin","paracetamol","nurofen","hedex","placebo")
colnames(X) <- drugs
print(X)

# Q-11.2 (using the dimnames() function where each column number is prefixed by “drug.”)
dimnames(X) <- list(NULL, paste("drug", 1:5, sep="."))   # Output: drug.1, drug.2, drug.3, drug.4, drug.5
print(X)

# Q-12 (Calculations on rows or columns of the matrix.)
# Q-12.1
print(mean(X[,5]))   # Computes average of the 5th column  ; Output: 2

# Q-12.2
print(var(X[4,]))   # Computes the variance of the 4th row  ; Output: 0.7

# Q-12.3
print(rowSums(X))   # Calculates the sum of elements along each row  ; Output:  11  9  8  4
print(apply(X,1,sum))   # Alternative & faster way  ; Output:  11  9  8  4

# Q-12.4
print(colSums(X))   # Calculates the sum of elements along each column  
print(apply(X,2,sum))    # Alternative & faster way
# Output: drug.1 drug.2 drug.3 drug.4 drug.5 
#         6      2      7      9      8

# Q-12.5
print(rowMeans(X))   # Computes the mean of elements along each row.
print(apply(X,1,mean))    # Alternative way  ; Output: 2.2  1.8  1.6  0.8

# Q-12.6
print(colMeans(X))   # Gives column names alongwith their respective mean
print(apply(X,2,mean))   # Alternative way
# Output: drug.1 drug.2 drug.3 drug.4 drug.5 
#         1.50   0.50   1.75   2.25   2.00 

# Q-12.7 (Sum groups of rows within a column.)
group = c("A","B","B","A")   # Assigns group labels (A or B) to each row
print(rowsum(X, group))      # Aggregates values by groups row-wise while keeping the column structure

print(row(X))    # Generate repeats based on row index
print(col(X))    # Generate repeats based on column index

print(tapply(X, list(group[row(X)], col(X)), sum))   # Alternative ways to rowsum()
print(aggregate(X, list(group), sum))    # Better way than above
# Output:  Group.1 drug.1 drug.2 drug.3 drug.4 drug.5
#          A       2      0      4      6      3
#          B       4      2      3      3      5


### DATE: 10.2.25

# Q-12.8 
print(apply(X,2,sample))   # Shuffles the elements of each column
print(t(apply(X,1,sample)))    # Shuffles the elements of each row; t() restores the matrix's original shape.

# Q-12.9
X <- rbind(X, apply(X,2,mean))    # Adds a row at the bottom of matrix X showing the column means
print(X)
X <- cbind(X, apply(X,1,var))     # Adds a column at the right showing the row variances
print(X)   
headings <- c(paste("drug.", 1:5, sep=""), "var")    # Adds 'var' heading to the last column
dimnames(X) <- list(NULL, headings)    # It is to be noted that dimnames() always takes list as input.
headings <- c(paste("Trial-", 1:4, sep=''), "Mean")   # Adds ' Mean' heading to the last row
rownames(X) <- headings
print(X)

# Q-13 (The sweep() function is used to ‘sweep out’ summaries from vectors, matrices, arrays or data frames.)
# Q-13.1 (Create a data frame containing the columns data$ki, data$gtv and data$time.)
eg_sweep = data.frame(data$ki, data$gtv, data$time)
print(eg_sweep)

# Q-13.2 (Use the apply() function to obtain a vector called cols that contains the mean of each column.)
cols <- apply(eg_sweep,2,mean)
print(cols)

# Q-13.3 (Without using sweep() function)
cols.means <- matrix(rep(cols, rep(dim(eg_sweep)[1], dim(eg_sweep)[2])), nrow=dim(eg_sweep)[1])    # Replicates cols (column means) to match the dimensions of eg_sweep.
print(cols.means)    # Creates a matrix with the same no. of rows as eg_sweep.
eg_sweep_alt <- eg_sweep - cols.means    # Performs element-wise subtraction to center each column by subtracting its mean.
print("Method 1")
print(eg_sweep_alt)

# Q-13.4 (Using sweep() to perform the same thing)
eg_sweep1 <- sweep(eg_sweep,2,cols)   # columns will be departures from relevant col means
print("Method 2")
print(eg_sweep1)

# Q-14 (Using the max.col function)
pg_data <- read.table("/home/ibab/Downloads/pgfull.txt", header=TRUE)    # Read the data present in pgfull.txt
print(pg_data)
species <- pg_data[1:54]   # Subset containing columns 1 to 54 from the read data
print(max.col(species))    # returns the column index of the max value in each row
print(names(species)[max.col(species)])   # using these indices to collect the names of the species
print(table(names(species)[max.col(species)]))   # table() - to build a frequency table of each of the species
# Output: AC AE AO AP CN FR HL HS LH LP TP 
#         26 23 4  2  1  19 3  1  5  1  4 
print(max.col(-species))    # extracts the column indices that give the minimum value along each row (no min.col() function in R).

# Q-15.1 (Lists can handle different lengths of arrays and also mixed data types.)
apples <- c(4, 4.5, 4.2, 5.1, 3.9)
oranges <- c(TRUE, TRUE, FALSE)
chalk <- c("limestone", "marl", "oolite", "CaCO3")
cheese <- c(3.2-4.5i, 12.8+2.2i)
items <- list(apples, oranges, chalk, cheese)
print(items)
print(items[[3]])   # subscripts on lists have double square brackets
# Output: "limestone"   "marl"   "oolite"   "CaCO3"
print(items[[3]][3])   # Output: "oolite"
print(items[3])
# If we try creating a dataframe of the 4 objects given above, then it will throw an error bec. these vectors have different lengths.
# items[[3]] - extracts the actual vector stored in the list at index 3 (which is chalk).
# items[3] - extracts a sublist containing only the third element. So, trying items[3][3] won’t work bec. items[3] is a list, not a vector.

# Q-15.2 (Lists can be indexed using the ‘$’ if the subsets have names associated with them.)
print(names(items))    # Output: NULL
items <- list(first=apples, second=oranges, third=chalk, fourth=cheese)   # provide name to each list
print(names(items))    # Output: "first"  "second" "third"  "fourth"
print(items$fourth)    # Output: 3.2-4.5i 12.8+2.2i
print(class(items))    # Output: "list"

# Q-15.3 (The function lapply() is used to apply functions to lists.)
print(lapply(items, length))    # displays length of each list
print(lapply(items, class))     # displays class of each list as numeric, logical, character, complex
print(lapply(items, mean))      # displays mean (in case of 2nd - boolean list - TRUE: 1, FALSE: 0, and in case of 3rd list - character list - so shows warning - returns NA)

# Q-15.4 (summary() function - outputs the name of the sublists, length & mode of the data present.)
print(summary(items))
print(str(items))   # str() (structure) function is the most useful for lists as it outputs the names, mode, data entries as well as the lengths of each sub-list.
print(attributes(items))    # Output the names - here - "first"  "second" "third"  "fourth"

# Difference: class() - defines how the object behaves (dataframe, factor, matrix), mode() - defines how the object is stored internally (numeric, list, character) & attributes() - stores extra info./metadata (names, labels, dimensions, etc.).
