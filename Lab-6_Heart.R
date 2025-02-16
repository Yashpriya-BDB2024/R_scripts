### LAB-6 (7.2.25)
### "HEART" dataset -

data = read.csv("/home/ibab/Downloads/Heart.csv", header=TRUE)   
print(data)

# Q-5 (Manipulating the ‘factors’ in existing data)
# Q-5.1 (Change the current class of the headers in the Heart data to the class ‘factor’ and check that it is indeed the factor class.)
data$ChestPain <- as.factor(data$ChestPain)
print(is.factor(data$ChestPain))    # Output: TRUE
data$Thal <- as.factor(data$Thal)
print(is.factor(data$Thal))   # Output: TRUE
data$AHD <- as.factor(data$AHD)
print(is.factor(data$AHD))     # Output: TRUE

# Q-5.2 (Print the number of levels in the category 'ChestPain' using nlevels() function)
print(nlevels(data$ChestPain))    # Output: 4

# Q-5.3 (Print the levels in the category 'Thal' using the levels() function)
print(levels(data$Thal))   # Output: "fixed"      "normal"     "reversable" 

# Q-6 (Generating factor levels using gl() function)
# Q-6.1 (Generate a new category called 'Condition' containing the same no. of elements that are no. of rows in the Heart data with labels 'High Risk', 'Moderate Risk' & 'Low Risk'.
Condition <- gl(3, 101, 303, labels = c("High Risk", "Moderate Risk", "Low Risk")) 
# 3: represents the no. of unique levels, 101: represents the no. of times each level is repeated consecutively, 303: total no. of elements
print(Condition)

# Q-6.2 (Add this category of data to the above Heart dataframe and give it a new dataframe name. Print this new dataframe.)
new_data <- data.frame(data, Condition)
print(new_data)

# Q-7 (Using the tapply() function.)
# Q-7.1 
# Each unique RestBP value acts as a category. So, The corresponding value is the mean cholesterol level (data$Chol) for all rows where data$RestBP matches that category.
tapply(data$Chol, data$RestBP, mean)

# Q-7.2 (What does trim option do here?)
# trim=0.1: removes the lowest 10% & highest 10% values from each RestBP group before calculating the mean.
tapply(data$Chol, data$RestBP, mean, trim=0.1)

subdata1 <- subset(data$Chol, data$RestBP == 140)     # Alternative way
print(subdata1)
sorted_subdata <- sort(subdata1)
print(sorted_subdata)
mean_subdata_trimmed <- mean(sorted_subdata, trim=0.1)
print(mean_subdata_trimmed)    # Output: 249.0385 (Matches with that of tapply(....trim=0.1))

# Q-8 (Use the pmin() and pmax() functions to find the minimum for each triplet set of 3 vectors data$RestBP, data$Chol and data$MaxHR)
print(pmin(data$RestBP, data$Chol, data$MaxHR))    # Parallel Minimum (finds the minimum value for each row across the three column)
print(pmax(data$RestBP, data$Chol, data$MaxHR))    # Parallel Maximum (Finds the maximum value for each row across the three column)

# Q-9 (Difference between rank(), sort() and order().)
# Q-9.1 
# rank(): The lowest value gets rank 1, the second lowest gets rank 2, and so on. If there are ties, it assigns the average rank for those tied values.
# sort(): The original indices are lost, and only the sorted (in ascending order) values are displayed.
# order(): Returns indices that would sort in ascending order.
ranks <- rank(data$RestBP)
sorted <- sort(data$RestBP)
ordered <- order(data$RestBP)
view <- data.frame(RestBP=data$RestBP, ranks, sorted, ordered)
print(view)

# Q-9.2
chol_ordered <- data$Chol[ordered]    # Extracts Chol values in the same order as sorted RestBP
print(chol_ordered)
ordered_data <- data.frame(ordered_RestBP = data$RestBP[ordered], ordered_Chol = chol_ordered)     # Creates a data frame with RestBP and Chol, both sorted based on RestBP
write.csv(ordered_data, "/home/ibab/Downloads/lab6_ordered_data_heart.csv")     # The ordered data is saved as a CSV file.
print(ordered_data)

# Q-10 (Converting data frames to matrices.)
# Q-10.1 (Extract the following rows and columns from the Heart data: rows 1-6, columns 3-8.)
filter1 = data[1:6, 3:8]
print(filter1)

# Q-10.2 (Convert the class of this object into a matrix using as.matrix() function & check the attributes to make sure it is a matrix.)
filter1mat = as.matrix(filter1)
print(filter1mat)
print(class(filter1mat))    # Output: "matrix" "array" 
print(attributes(filter1mat))    # Output: $dim - 6 6 ; $dimnames[[1]] -  "1" "2" "3" "4" "5" "6" ; $dimnames[[2]] - "Sex"  "ChestPain" "RestBP"  "Chol"  "Fbs"  "RestECG"  

# Q-10.3 (Create a newcol vector/column by adding 3 vectors - data$RestBP, data$Chol, data$MaxHR.)
newcol = data$RestBP + data$Chol + data$MaxHR     # Element-wise addition of three columns
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
print(tail(newrowadded))   # Output: Row names as 2610 & 351 ; R tries to maintain unique row names, often concatenating or modifying them to avoid duplication.
print(dim(newrowadded))    # Output: 305  15

# Q-11 (Adding row and column names.)
X <- matrix(c(140, 220, 80, 45, 1, 130, 180, 85, 50, 0, 120, 210, 90, 55, 1, 150, 200, 75, 40, 0), nrow = 4, byrow = TRUE)
print(X)
print(rownames(X))    # Output: NULL (No row names set)
rownames(X) <- rownames(X, do.NULL=FALSE, prefix='Case')    # generates default row names with the given prefix ("Case").
print(rownames(X))
print(X)

# Q-11.1
features <- c("BP", "Cholesterol", "HeartRate", "Age", "ECG")
colnames(X) <- features
print(X)

# Q-11.2 (using the dimnames() function where each column number is prefixed by “Attribute.”)
dimnames(X) <- list(NULL, paste("Attribute", 1:5, sep="."))    # Output: Attribute.1, Attribute.2, Attribute.3, Attribute.4, Attribute.5
print(X)

# Q-12 (Calculations on rows or columns of the matrix.)
# Q-12.1
print(mean(X[,5]))   # Computes average of the 5th column  ; Output: 0.5

# Q-12.2
print(var(X[4,]))   # Computes the variance of the 4th row  ; Output: 6620

# Q-12.3
print(rowSums(X))   # Calculates the sum of elements along each row  ; Output: 486 445 476 465
print(apply(X,1,sum))   # Alternative & faster way  ; Output:  486 445 476 465

# Q-12.4
print(colSums(X))   # Calculates the sum of elements along each column 
print(apply(X,2,sum))    # Alternative & faster way
# Output: Attribute.1 Attribute.2 Attribute.3 Attribute.4  Attribute.5 
#         540         810         330         190          2 

# Q-12.5
print(rowMeans(X))   # Computes the mean of elements along each row.
print(apply(X,1,mean))    # Alternative way  ; Output: 97.2 89.0 95.2 93.0

# Q-12.6
print(colMeans(X))   # Computes the mean of elements along each column & algo displays the column names
print(apply(X,2,mean))   # Alternative way  
# Output: Attribute.1 Attribute.2 Attribute.3 Attribute.4 Attribute.5 
#         135.0       202.5       82.5        47.5        0.5 

# Q-12.7 (Sum groups of rows within a column.)
group = c("A","B","B","A")   # Assigns group labels (A or B) to each row
print(rowsum(X, group))   #    Aggregates values by groups row-wise while keeping the column structure

print(row(X))    # Generate repeats based on row index
print(col(X))    # Generate repeats based on column index

print(tapply(X, list(group[row(X)], col(X)), sum))   # Alternative ways to rowsum()
print(aggregate(X, list(group), sum))    # Better way than above
# Output:  Group.1 Attribute.1 Attribute.2 Attribute.3 Attribute.4 Attribute.5
#          A         290         420         155         85         1
#          B         250         390         175         105        1


### DATE: 10.2.25

# Q-12.8 
print(apply(X,2,sample))   # Shuffles the elements of each column
print(t(apply(X,1,sample)))    # Shuffles the elements of each row; t() restores the matrix's original shape.

# Q-12.9
X <- rbind(X, apply(X,2,mean))    # Adds a row at the bottom of matrix X showing the column means
print(X)
X <- cbind(X, apply(X,1,var))     # Adds a column at the right showing the row variances
print(X)   
headings <- c(paste("Attribute.", 1:5, sep=""), "Variance")    # Adds 'Variance' heading to the last column
dimnames(X) <- list(NULL, headings)    # It is to be noted that dimnames() always takes list as input.
headings <- c(paste("Case-", 1:4, sep=''), "Mean")   # Adds ' Mean' heading to the last row
rownames(X) <- headings
print(X)

# Q-13 (The sweep() function is used to ‘sweep out’ summaries from vectors, matrices, arrays or data frames.)
# Q-13.1 (Create a data frame containing the columns data$Chol, data$RestBP and data$MaxHR.)
eg_sweep = data.frame(data$Chol, data$RestBP, data$MaxHR)
print(eg_sweep)

# Q-13.2 (Use the apply() function to obtain a vector called cols that contains the mean of each column.)
cols <- apply(eg_sweep,2,mean)
print(cols)     # Output: 246.6931    131.6898    149.6073 

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

# Q-15.1 (Lists can handle different lengths of arrays and also mixed data types.)
age <- data$Age[1:5]   
cholesterol <- data$Chol[1:3]
thal <- data$Thal[1:4] 
complex_numbers <- c(3.2-4.5i, 12.8+2.2i)
heart_list <- list(age, cholesterol, thal, complex_numbers)
print(heart_list)
print(heart_list[[3]])    # Access third element (thal values)  ; Output: [1] "fixed"  "normal"  "reversable"  "normal" 
print(heart_list[[3]][3])   # Access a specific value from the third element  ; Output: [1] "reversable"
print(heart_list[3])    # Extracting as a sublist                                                  

# Q-15.2 (Lists can be indexed using the ‘$’ if the subsets have names associated with them.)
print(names(heart_list))    # Output: NULL
heart_list <- list(Age=age, Chol=cholesterol, Thal=thal, Complex=complex_numbers)    # Assigning names to the list elements
print(names(heart_list))    # Output:  "Age"     "Chol"    "Thal"    "Complex"
print(heart_list$Complex)   # Access the complex numbers list using $  ; Output: [1]  3.2-4.5i 12.8+2.2i
print(class(heart_list))    # Output: "list"

# Q-15.3 (The function lapply() is used to apply functions to lists.)
print(lapply(heart_list, length))
print(lapply(heart_list, class))    # Output: "integer", "integer", "character", "complex"
print(lapply(heart_list, mean))     # Will show warning - returns NA for Thal (character list) 

# Q-15.4 
print(summary(heart_list))    # summary() function - outputs the name of the sublists, length & mode of the data present.
print(str(heart_list))     # str() (structure) function is the most useful for lists as it outputs the names, mode, data entries as well as the lengths of each sub-list.
print(attributes(heart_list))    # Output the names - here - "Age"     "Chol"    "Thal"    "Complex"

# Difference: class() - defines how the object behaves (dataframe, factor, matrix), mode() - defines how the object is stored internally (numeric, list, character) & attributes() - stores extra info./metadata (names, labels, dimensions, etc.).
