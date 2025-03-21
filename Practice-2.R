###### LAB-5&6 PRACTICE ######

# Read the .csv file
data <- read.csv("C:\\Users\\Yash Priya Baid\\SEM-2\\BS_and_R\\BrainCancer.csv")

# Dimensions of the data 
print(dim(data))    # 88   9

# Column names of the data
print(colnames(data))

# Row names of data
print(rownames(data))    # no row names, so will print row numbers

# Display first 30 lines of data (including header)
print(head(data, 30))
print(head(data))    # By default - 6 lines (excluding header line)

# Frequency table of each column
print(lapply(data, table))

# Frequency table of a specific column
print(table(data$diagnosis))

# To see the categorical columns, mean, median, Q1, Q3, max. & min. value, class, mode
print(summary(data))   

# How many levels does each categorical variable have and what are they?
print(unique(data$stereo))
print(unique(data$loc))
print(unique(data$sex))
print(unique(data$diagnosis))

# Mean of a column
print(mean(data$gtv))

# Median of a column
print(median(data$gtv))   
# If mean > median - right-skewed distri.

# Mode of a column    
install.packages('modeest')
library('modeest')
mode_val <- mlv(data$gtv, method='mfv')
print(mode_val)
# If mean > median > mode - right skewed distri.

# Standard deviation of a column 
print(sd(data$gtv))

# Histogram plot of a column
hist(data$gtv)

# Skewness & kurtosis values of a column
install.packages('moments')
library('moments')
print(skewness(data$gtv))    # skewness > 0: right-skewed 
print(kurtosis(data$gtv))    # kurtosis > 0: leptokurtic; sharp peak, heavy tails

# Boxplot of a column (by default)
boxplot(data$gtv)

# Customised boxplot of a column
boxplot(data$gtv, xlabel='Spread of GTV', ylabel='GTV', range=0.1, horizontal=FALSE, border=c('blue'), col=c('red'))

# For comparison purpose -
boxplot(data$gtv, data$time, data$ki,
        names = c("GTV", "time", "ki"),
        main="Boxplot comparison of GTV, time, ki",
        ylab="Values")

# Build a subset of the GTV data with values > 20. Also print the dimensions of the subset.
filter1 <- subset(data, data$gtv > 20)
print(filter1)
print(dim(filter1))     # 11   9

# Build subset of row numbers 1,3,8,9,13,14,18 and 21
filter2 <- data[c(1,3,8,9,13,14,18, 21),]
print(filter2)

# Obtain the indices of the rows that have only Female sex entries, and using this list of indices build a subset of the data.
filter3_ind <- which(data$sex == 'Female')
print(filter3)
subset_filter3 <- data[filter3_ind,]
print(subset_filter3)

# Build a new column that uses this formula: data$gtv*data$ki/234. Build a new dataframe containing the three columns: GTV, KI and the newly added column.
new_col <- (data$gtv * data$ki)/234
new_df <- data.frame(GTV = data$gtv, KI = data$ki, new_column = new_col)
print(new_df)

# Write the subset data corresponding to ”Female” entries (above) as a dataframe, and write the dataframe to a csv file – call this file as lab5_female_BrainCancer.csv
female_df <- data.frame(subset_filter3)
write.csv(female_df, "C:\\Users\\Yash Priya Baid\\SEM-2\\BS_and_R\\test.csv")

# Change the current class of the headers in the Brain Cancer data to the class ‘factor’ and check that it is indeed the factor class.
data$sex <- as.factor(data$sex)
print(is.factor(data$sex))    # TRUE
data$diagnosis <- as.factor(data$diagnosis)
print(is.factor(data$diagnosis))  # TRUE
data$stereo <- as.factor(data$stereo)
print(is.factor(data$stereo))    # TRUE
data$loc <- as.factor(data$loc)
print(is.factor(data$loc))   # TRUE

# No. of levels present in a column 
print(nlevels(data$sex))

# Levels - print
print(levels(data$sex))

# Generate a new category called 'Temperature' containing the same no. of elements that are no. of rows in the Brain Cancer data with labels 'Hot', 'Cold' & 'Lukewarm'.
Temperature <- gl(3, 29, 88, labels=c('Hot', 'Cold', 'Lukewarm'))    # 88: rows, 3: labels, 29: no. of times each label is repeated
print(Temperature)

# Add this category of data to the above Brain Cancer dataframe and give it a new dataframe name. Print this new dataframe.
temp_df <- data.frame(data, Temperature)
print(temp_df)

print(tapply(data$gtv, data$ki, mean))    # will display the mean of gtv values corresponding to each ki category

# trim option
print(tapply(data$gtv, data$ki, mean, trim=0.1))   # tapply() - applies a function based on grouping factor; trim=0.1 means it removes lowest 10% & highest 10% of values before calculating the mean.

# Use the pmin() and pmax() functions to find the min. & max. for each triplet set of 3 vectors data$gtv, data$ki and data$time.
print(pmin(data$gtv, data$ki, data$time))    # parallel min. - min. in each row across all the 3 columns
print(pmax(data$gtv, data$ki, data$time))    # parallel max.

# rank() - lowest value = rank 1, 2nd lowest value = rank 2
# sort() - in ascending order, original indices are lost
# order() - returns indices that sort in ascending order.
ranked <- rank(data$gtv)
sorted <- sort(data$gtv)
ordered <- order(data$gtv)
view <- data.frame(gtv=data$gtv, ranked, sorted, ordered)
print(view)

# Extracts diagnosis values in the same order as sorted gtv
diag_val <- data$diagnosis[ordered]
print(diag_val)

# Extract the following rows and columns from the Brain cancer data: rows 1-6, columns 3-8.
filter4 <- data[1:6, 3:8]
print(filter4)

# Convert the class of this object into a matrix using as.matrix() function & check the attributes to make sure it is a matrix.
print(class(filter4))    # 'data.frame'
filt4 <- as.matrix(filter4)
print(class(filt4))   # 'matrix'  'array'
print(attributes(filt4))

# Create a newcol vector/column by adding 3 vectors - data$ki, data$gtv, data$time.
newcol <- data$ki + data$gtv + data$time    #element-wise addition
print(newcol)
newcoladded <- data.frame(data, newcol)
print(colnames(newcoladded))
# Alternative way -
newcoladded1 <- cbind(data, newcol)

# Pick rows 26 and 35 from the original data & add these new rows to the original data using the data.frame() function.
filter5 = data.frame(data[c(26, 35),])   
newrowadded = rbind(data, filter5)
print(newrowadded)     # Output: Row names as 261 & 351 ; R tries to maintain unique row names, often concatenating or modifying them to avoid duplication.
print(dim(newrowadded))     # 90   9

# Adding row & column names 
X <- matrix(c(1,0,2,5,3,1,1,3,1,3,3,1,0,2,2,1,0,2,1,0), nrow=4, byrow=TRUE)
rownames(X) <- rownames(X, do.NULL=FALSE, prefix='Trial-')
print(rownames(X))
drugs <- c("aspirin","paracetamol","nurofen","hedex","placebo")
colnames(X) <- drugs
print(colnames(X))

# using the dimnames() function where each column number is prefixed by “drug.”
dimnames(X) <- list(NULL, paste("drug", 1:5, sep="."))
print(X)

# Compute average of the 5th column -
print(mean(X[,5]))

#  Compute the variance of the 4th row -
print(var(X[4,]))

# Calculate the sum of elements along each row & each column -
print(rowSums(X))
print(apply(X, 1, sum))   # Alternative
print(colSums(X))
print(apply(X, 2, sum))    #Alternative

# Row & Column means -
print(rowMeans(X))
print(apply(X, 1, mean))
print(colMeans(X))
print(apply(X, 2, mean))

group = c("A","B","B","A")   # Assigns group labels (A or B) to each row
print(rowsum(X, group))      # Aggregates values by groups row-wise while keeping the column structure
# Alternate way -
print(aggregate(X, list(group), sum))
print(row(X)) 
print(col(X))

# shuffle elements of each row & column -
print(apply(X, 1, sample))
print(t(apply(X, 2, sample)))

# Adds a row at the bottom of matrix X showing the column mean.
X <- rbind(X, apply(X, 2, mean))
print(X)
# Adds a column at the right showing the row variances
Y <- cbind(X, apply(X, 1, var))
print(Y)
# Adds 'var' heading to the last column
headings <- c(paste("drug.", 1:5, sep=""), 'var')
dimnames(X) <- list(NULL, headings)
# Adds ' Mean' heading to the last row
headings1 <- c(paste("Trial-", 1:4, sep=""), 'mean')

# Create a data frame containing the columns data$ki, data$gtv and data$time.
eg_sweep = data.frame(data$ki, data$gtv, data$time)
# Use the apply() function to obtain a vector called cols that contains the mean of each column.
cols <- apply(eg_sweep, 2, mean)
eg_sweep1 <- sweep(eg_sweep, 2, cols)   # col. value - col. mean  
print(eg_sweep1)

# using the max.col() -
pg_data <- read.table("C:\\Users\\Yash Priya Baid\\SEM-2\\BS_and_R\\pgfull.txt", header=TRUE)
# Subset containing columns 1 to 54 from the read data
species <- pg_data[1:54]
# return the column index of the max value in each row
print(max.col(species))
# using these indices to collect the names of the species
print(name(species)[max.col(species)])
# table() - to build a frequency table of each of the species
print(table(name(species)[max.col(species)]))
# extracts the column indices that give the minimum value along each row (no min.col() function in R).
print(max.col(-species))

apples <- c(4, 4.5, 4.2, 5.1, 3.9)
oranges <- c(TRUE, TRUE, FALSE)
chalk <- c("limestone", "marl", "oolite", "CaCO3")
cheese <- c(3.2-4.5i, 12.8+2.2i)
items <- list(apples, oranges, chalk, cheese)
print(items[[3]])    # Output: "limestone"   "marl"   "oolite"   "CaCO3"
print(items[[3]][3])      # Output: "oolite"
print(items[3])

items <- list(first=apples, second=oranges, third=chalk, fourth=cheese)    # assign names to vectors
print(names(items))
print(items$fourth)
print(class(items))   # Output: "list"
# lapply() is used to apply functions to lists.
print(lapply(items, length))
print(lapply(items, class))
print(lapply(items, mean))
print(summary(items))
print(str(items))   # str() (structure) function is the most useful for lists as it outputs the names, mode, data entries as well as the lengths of each sub-list.
print(attributes(items))
print(mode(items))

# Difference: class() - defines how the object behaves (dataframe, factor, matrix), mode() - defines how the object is stored internally (numeric, list, character) & attributes() - stores extra info./metadata (names, labels, dimensions, etc.).


