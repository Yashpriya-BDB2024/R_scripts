##### To be continued ------ #####
##### DATE: 4.4.25 #####

# Section-4 (ROC Curve)

# Q-1 (Read in the white wine data into a dataframe, and create additional columns that classifies the data as good or bad wine based on threshold quality scores of 6, 7, 8, 9 and 10.)
wine_data <- read.csv("/home/ibab/Downloads/winequality-white.csv", header=TRUE, sep=";")   
print(str(wine_data))   # View structure of the data

# Create good (label: 1) or bad (label: 0) wine binary classification columns based on different thresholds -
# If the quality is greater than or equal to the threshold, then it will be labelled as 1, otherwise will be labelled as 0.
wine_data$threshold_6 <- ifelse(wine_data$quality >= 6, 1, 0)
wine_data$threshold_7 <- ifelse(wine_data$quality >= 7, 1, 0)
wine_data$threshold_8 <- ifelse(wine_data$quality >= 8, 1, 0)
wine_data$threshold_9 <- ifelse(wine_data$quality >= 9, 1, 0)
wine_data$threshold_10 <- ifelse(wine_data$quality >= 10, 1, 0)
print(head(wine_data, 20))   # To see the appended new columns of labels

# Q-2 (Use plot.roc() function to plot the ROC curves for each threshold value. Which threshold value brings the ROC curve to the perfect classifier?)
install.packages('pROC')
library('pROC')
plot.roc(wine_data$threshold_6, wine_data$alcohol, main="ROC Curve (threshold >= 6)", legacy.axes=TRUE, ci=TRUE, print.auc=TRUE, identity.lwd=2, print.thres=TRUE)
plot.roc(wine_data$threshold_7, wine_data$alcohol, main="ROC Curve (threshold >= 7)", legacy.axes=TRUE, ci=TRUE, print.auc=TRUE, identity.lwd=2, print.thres=TRUE)
plot.roc(wine_data$threshold_8, wine_data$alcohol, main="ROC Curve (threshold >= 8)", legacy.axes=TRUE, ci=TRUE, print.auc=TRUE, identity.lwd=2, print.thres=TRUE)
plot.roc(wine_data$threshold_9, wine_data$alcohol, main="ROC Curve (threshold >= 9)", legacy.axes=TRUE, ci=TRUE, print.auc=TRUE, identity.lwd=2, print.thres=TRUE)
plot.roc(wine_data$threshold_10, wine_data$alcohol, main="ROC Curve (threshold >= 10)", legacy.axes=TRUE, ci=TRUE, print.auc=TRUE, identity.lwd=2, print.thres=TRUE)
