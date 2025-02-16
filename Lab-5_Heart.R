### LAB-5 (31.1.25)
### "HEART" dataset -


# Q-1 (Read in the file ”Heart.csv” and store the data as data frame)
data = read.csv("/home/ibab/Downloads/Heart.csv", header=TRUE)    
print(data)

# Q-2.1 (dimensions of the data)
print(dim(data))   # Output: 303  15

# Q-2.2 (column names of the data)
print(colnames(data))    # Output: "X"  "Age"  "Sex"  "ChestPain"  "RestBP"  "Chol"  "Fbs"  "RestECG"  "MaxHR"  "ExAng"  "Oldpeak"  "Slope"  "Ca"  "Thal"  "AHD" 

# Q-2.3 (row names)
print(rownames(data))    # if no row names, then will display row no.(s)

# Q-2.4 (display the first 30 lines of data and header also)
print(head(data, 30))   

# Q-2.5 (data representation as a frequency table)
lapply(data, table)    # Applies table() to each column
print(table(data$Chol))   # Applies table() to only 'Chol' column

# Q-2.6 & 2.7 (Categorical variables)
print(summary(data))   # 3 categorical variables are there - ChestPain, Thal, AHD

# Q-2.8 (How many levels does each categorical variable have and what are they?)
unique(data$ChestPain)   # 4 levels - typical, asymptomatic, nonanginal, nontypical
unique(data$Thal)        # levels - fixed, normal, reversable, NA    ; (NA - missing data) 
unique(data$AHD)         # 2 levels - No, Yes

# Q-3.1 (mean of column describing the cholesterol ('Chol'))
print(mean(data$Chol))     # Output: 246.6931

# Q-3.2 (median of 'Chol' column (how does it compare with the mean?))
# Since, mean > median, it shows the right-skewed (positively-skewed) distribution; means that there might be a few high cholesterol values pulling the mean upward.
print(median(data$Chol))    # Output: 241 

# Q-3.3 (mode of 'Chol' column (how does it compare with median and mean? Is the distribution symmetric?)
# It is a multimodal distribution. Since, mean > median > mode, it is right-skewed distributrion and not the symmetric one.
library('modeest')   
mode_val1 <- mlv(data$Chol, method="mfv")    
print(mode_val1)    # Output: 197  204  234

# Q-3.5 (standard deviation of 'Chol' column)
print(sd(data$Chol))     # Output: 51.77692

# Q-3.6 (statistical summary of 'Chol' column data)
print(summary(data$Chol))
# Output: Min.  1st Qu.  Median  Mean    3rd Qu.  Max. 
#        126.0   211.0   241.0   246.7   275.0    564.0 

# Q-3.7 (histogram plot of 'Chol' data (does the appearance agree with the mean, median, mode order?)
# Yes, bec., the mode is at the peak (left-most highest bar), the median is slightly right of the mode, and the mean is even further right, pulled by high outliers. 
hist(data$Chol)    

# Q-3.8 (skewness value of 'Chol' data)
# Since, skewness > 0, it is a right-skewed distribution.
library(moments)
print(skewness(data$Chol))    # Output: 1.129874

# Q-3.9 (kurtosis value of 'Chol' data; do the skewness and kurtosis match with your expectations?) 
# Since, kurtosis > 0, it is a leptokurtic distribution, that means, sharp peak, heavy tail, outlier prone as can be seen in histogram.
print(kurtosis(data$Chol))    # Output: 7.398208

# Q-3.10 (Box plot of 'Chol' data)
boxplot(data$Chol)   # default
boxplot(data$Chol, xlabel="Spread of Cholesterol", ylabel="Cholesterol", range=0.1, horizontal=FALSE, border=c("blue"), col=c("red"))
# Range = 0.2: Whiskers extend farther from the box.
boxplot(data$Chol, xlabel="Spread of Cholesterol", ylabel="Cholesterol", range=0.2, horizontal=FALSE, border=c("blue"), col=c("red"))
# Range = 0.05: Most sensitive to outliers, with a greater number of points flagged as outliers, and whiskers are the shortest, tightly enclosing the central data.
boxplot(data$Chol, xlabel="Spread of Cholesterol", ylabel="Cholesterol", range=0.05, horizontal=FALSE, border=c("blue"), col=c("red"))

# Q-3.11 (Make a boxplot of 3 data sets – Chol, RestBP, MaxHR. Which of the data has the broadest distribution?)
# 'Chol'has the broadest distribution because of largest IQR, longest whiskers & most outliers.
boxplot(data$Chol, data$RestBP, data$MaxHR, 
        names = c("Chol", "RestBp", "MaxHR"), 
        main = "Boxplot Comparison of Chol, RestBP, and MaxHR", 
        ylab = "Values")

# Q-4.1 (Build subset of the 'Chol' data with values > 250. Also print the dimensions of the subset.)
filter1 = subset(data, data$Chol > 250)
print(filter1)
print(dim(filter1))    # Output: 127  15

# Q-4.2 (Build subset of row numbers 1,3,8,9,13,14,18 and 21)
filter2 = data[c(1, 3, 8, 9, 13, 14, 18, 21),]
print(filter2)

# Q-4.3 (Obtain the indices of the rows that have only asymptomatic ChestPain entries, and using this list of indices build a subset of the data.)
filter3_ind = which(data$ChestPain == 'asymptomatic')    # will give the indices
filter3 = data[filter3_ind ,]    # ',' represent column    # 'filter3' is a subset
print(filter3)

# Q-4.4 (Build a new column that uses this formula: data$Chol * data$RestBP / 234. Build a new dataframe containing the three columns: Chol, RestBP and the newly added column.)
new_dataframe <- data.frame(Chol = data$Chol, RestBP = data$RestBP, new_column = data$Chol * data$RestBP / 234)
print(new_dataframe)

# Q-4.5 (Write the subset data corresponding to 'asymptomatic' entries (above) as a dataframe, and write the dataframe to a csv file – call this file as lab5_asymptomatic_heart.csv)
asymptomatic_dataframe <- data.frame(filter3)
write.csv(asymptomatic_dataframe, "home/ibab/Downloads/lab5_asymptomatic_heart.csv")    # Now, this new .csv file will have all the 'asymptomatic' entries as dataframe
