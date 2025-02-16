### LAB-5 (31.1.25)
### "BRAIN CANCER" dataset -


# Q-1 (Read in the file ”BrainCancer.csv”)
data = read.csv("/home/ibab/Downloads/BrainCancer.csv", header=TRUE)   
print(data)

# Q-2.1 (dimensions of the data)
print(dim(data))      # Output: 88  9

# Q-2.2 (column names of the data)
print(colnames(data))      # Output: "X"   "sex"   "diagnosis"   "loc"   "ki"   "gtv"  "stereo"   "status"   "time" 

# Q-2.3 (row names)
print(rownames(data))   # no row names present, it will just print row numbers

# Q-2.4 (display the first 30 lines of data)
print(head(data, 30))   

# Q-2.5 (data representation as a frequency table)
print(lapply(data, table))  # frequency table of each column
print(table(data$diagnosis))    # frequency table of 'diagnosis' column

# Q-2.6 & 2.7 (Categorical variables)
print(summary(data))    # 4 categorical variables are there - sex, diagnosis, loc, stereo

# Q-2.8 (How many levels does each categorical variable have and what are they?)
unique(data$sex)   # 2 levels are there - Female, Male
unique(data$diagnosis)   # Levels - Meningioma, HG glioma, LG glioma, NA, Other    ; (NA - missing data)
unique(data$loc)      # 2 levels - Infratentorial, Supratentorial
unique(data$stereo)   # 2 levels - SRS, SRT

# Q-3.1 (mean of column describing the gross tumor volume (GTV))
print(mean(data$gtv))     # Output: 8.660795

# Q-3.2 (mean of column describing the time)
print(mean(data$time))     # Output: 27.4575

# Q-3.3 (median of GTV column (how does it compare with the mean?))
# Since, mean > median, it is right-skewed distribution.
print(median(data$gtv))     # Output: 6.51

# Q-3.4 (mode of GTV column (how does it compare with median and mean? Is the distribution symmetric?)
# Since, mean > median > mode, it is right-skewed distribution, and not the symmetric one.
mode_val <- names(which.max(table(data$gtv)))
print(mode_val)
library('modeest')    # Alternative better way
mode_val1 <- mlv(data$gtv, method="mfv")     # mlv - most likely value, mfv - most frequent value
print(mode_val1)      # Output: 2.5

# Q-3.5 (standard deviation of GTV column)
print(sd(data$gtv))    # Output: 8.657576 

# Q-3.6 (statistical summary of GTV column data)
print(summary(data$gtv))
# Output: Min.   1st Qu.  Median  Mean   3rd Qu.  Max. 
#         0.010  2.500    6.510   8.661  12.100   34.640 

# Q-3.7 (histogram plot of GTV data (does the appearance agree with the mean,median,mode order?)
# Yes, bec., the mode is in the left, the median is slightly right of the mode, and the mean is even further right.
hist(data$gtv)

# Q-3.8 (skewness value of GTV data)
# Since, skewness > 0, it is a right-skewed distribution.
library(moments)
print(skewness(data$gtv))   # Output: 1.37448

# Q-3.9 (kurtosis value of GTV data; do the skewness and kurtosis match with your expectations?)
# Since, kurtosis > 0, it is a leptokurtic distribution, that means, sharp peak, heavy tail.
print(kurtosis(data$gtv))   # Output: 4.172248

# Q-3.10 (Box plot of GTV data; comment on the differences)
boxplot(data$gtv)     # default
# Range = 0.1 - shorter whiskers, more values as outliers
boxplot(data$gtv, xlabel="Spread of GTV", ylabel="GTV", range=0.1, horizontal=FALSE, border=c("blue"), col=c("red"))
# Range = 0.2 - longer whiskers compared to 0.1, fewer points as outliers
boxplot(data$gtv, xlabel="Spread of GTV", ylabel="GTV", range=0.2, horizontal=FALSE, border=c("blue"), col=c("red"))
# Range = 0.05 - very short whiskers, large no. of points as outliers
boxplot(data$gtv, xlabel="Spread of GTV", ylabel="GTV", range=0.05, horizontal=FALSE, border=c("blue"), col=c("red"))

# Q-3.11 (Make a boxplot of 3 data sets – GTV, KI, and time. Which of the data has the broadest distribution?)
# 'time' has the broadest distribution bec. of larger IQR & longer whiskers as compared to 'gtv' & 'ki'.
boxplot(data$gtv, data$ki, data$time, 
        names = c("GTV", "ki", "time"), 
        main = "Boxplot Comparison of GTV, ki, and time", 
        ylab = "Values")  

# Q-4.1 (Build subset of the GTV data with values > 20. Also print the dimensions of the subset)
filter1 = subset(data, data$gtv > 20)
print(filter1)
print(dim(filter1))   # Output: 11  9

# Q-4.2 (Build subset of row numbers 1,3,8,9,13,14,18 and 21)
filter2 = data[c(1, 3, 8, 9, 13, 14, 18, 21),]
print(filter2)

# Q-4.3 (Obtain the indices of the rows that have only Female sex entries, and using this list of indices build a subset of the data)
filter3_ind = which(data$sex == 'Female')    # will give the indices
filter3 = data[filter3_ind ,]      # ',' represent column    # 'filter3' is a subset
print(filter3)

# Q-4.4 (build a new column that uses this formula: data$gtv*data$ki/234. Build a new dataframe containing the three columns: GTV, KI and the newly added column)
new_dataframe <- data.frame(GTV = data$gtv, KI = data$ki, new_column = data$gtv*data$ki/234)
print(new_dataframe)

# Q-4.5 (Write the subset data corresponding to ”Female” entries (above) as a dataframe, and write the dataframe to a csv file – call this file as lab5_female_BrainCancer.csv)
female_dataframe <- data.frame(filter3)
write.csv(female_dataframe, "home/ibab/Downloads/lab5_female_BrainCancer.csv")     # Now, this new .csv file will have all the 'Female' entries as dataframe
