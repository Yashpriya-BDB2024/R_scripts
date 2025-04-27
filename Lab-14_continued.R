###### LAB-14 - TO BE CONTINUED (DATE: 25-4-25) ######

# Q-3 (One sample proportion test)

x <- 710    # no. of successes
n <- 2600   # total no. of trials
p <- 0.25   # proportion to test against (i.e., hypothesized value) 
alternative <- "greater"   # It is the string value indicating type of null hypothesis as “two-sided”, “less”, “greater”.

# binom.test() function computes the exact binomial probability, and is used when the sample sizes are small.
print(binom.test(x=x, n=n, p=p, alternative=alternative))
# Output:
# Exact binomial test
# data:  x and n
# number of successes = 710, number of trials = 2600, p-value = 0.003766      ; p-value: The probab. of observing 710 or more successes if the true proportion was only 0.25. Since p < 0.05, reject null hypothesis.
# alternative hypothesis: true probability of success is greater than 0.25
# 95 percent confidence interval:
#  0.2587064  1.0000000     ; We are 95% confident that the true success rate lies in this range.
# sample estimates:
#  probability of success 
# 0.2730769     ; Observed proportion (710 / 2600)

# prop.test() function uses a normal approximation to the binomial distribution and can be used when n > 30.
# correct: It is a logical variable indicating whether a correction should be applied for small sample sizes (the default is TRUE).
# Continuity correction adjusts the normal approximation to better match the discrete binomial distribution. 
print(prop.test(x=x, n=n, p=p, alternative=alternative, correct=FALSE))   # Since, n is large, so we need not to apply the continuity correction.
# Output:
# 1-sample proportions test without continuity correction
# data:  x out of n, null probability p
# X-squared = 7.3846, df = 1, p-value = 0.003289    ; p < 0.05, so reject the null hypothesis.
# alternative hypothesis: true p is greater than 0.25
# 95 percent confidence interval:
#  0.258946  1.000000   ; slightly different than binom.test()
# sample estimates:
#  p 
# 0.2730769 

# Interpretation:
# Both test suggests that the sample proportion (0.273) is significantly greater than 0.25. So, there is strong evidence against the null hypothesis.

# Q-4 (One sample variance test)

# Q-4.a: Write a function with the following structure to output the statistical conclusion and properties (p-value and σ^2 value):
# one_sample_variance_test(x,test_sigma,alpha), where the function should compute the Chi_sq test statistic & get appropriate limits by using qchisq() & deciding the conclusion.
one_sample_variance_test <- function(x, test_sigma, alpha) {
  n <- length(x)  # sample size
  samp_var <- var(x)   # sample variance
  hypo_var <- test_sigma^2   # hypothesized population variance
  chi_sq <- ((n-1)*samp_var) / hypo_var
  # To compute confidence limits from Chi-sq distribution -
  df = n-1   # degrees of freedom
  # Assume it as two-tailed test -
  lower_limit <- qchisq(alpha/2, df=df)   # qchisq() - to find the critical values.
  upper_limit <- qchisq(1-(alpha/2), df=df)
  # Two-tailed p-value calculation -
  # The chi-sq distri. is skewed to the right for small degrees of freedom, and it becomes more symmetric as degrees of freedom increase. As chi_sq increases, it moves further to the right tail of the distribution.
  if (chi_sq < df) {   # Observed statistic is closer to the lower tail of chi sq. distri., so we calculate the left tail area using pchisq(...).
    p_value <- 2*pchisq(chi_sq, df)   # Multiplying by 2 bec. its a 2-tailed test.
  } else {
    p_value <- 2*(1 - pchisq(chi_sq, df))   # We calculate the right tail area.
  }
  # Decision -
  if (chi_sq < lower_limit || chi_sq > upper_limit) {
    conclusion <- "Reject null hypothesis (Sample variance is significantly different from hypothesised variance)."
  } else {
    conclusion <- "Fail to reject null hypothesis."
  }
  result <- c(p_value=p_value, samp_var=samp_var, hypo_var=hypo_var, conclusion=conclusion)
  return(result)
}

# Q-4.b: Perform the above test for the data set given below for hypothesized σ = 29 and for 0.05 significance level. 
# The points are from a normal distribution with mean=140 and standard deviation of 20. 
x = c(142.8, 135.0, 157.5, 148.4, 135.9, 153.4, 149.0, 130.2, 156.0, 189.7, 151.6, 156.5, 123.8, 152.9, 118.4, 145.8)
one_sample_variance_test(x, test_sigma = 29, alpha = 0.05)  
# Output: p_value: 0.0143 (Reject null hypothesis.)

# Q-5 (One sample Wilcoxon signed rank test)
# The function wilcox.test() carries out one- and two-sample non-parametric tests. Perform this test for the data set with μ = 160 and confidence level of 95% and H0 as μ >= μ0.

x = c(176.9, 158.3, 152.1, 158.8, 172.4, 169.8, 159.7, 162.7, 156.6, 174.5, 184.4, 165.2, 147.8, 177.8, 160.1, 161.5)
# alternative="less", bec. H0 is mu >= mu0 and H1 is mu < mu0 , conf.int=TRUE calculates the confidence interval for the mu.
output <- wilcox.test(x, mu=160, alternative="less", conf.int = TRUE, conf.level = 0.95)    
print(output)
# Output:
# Wilcoxon signed rank exact test
# data:  x
# V = 99, p-value = 0.9477    ; It tells the difference b/w the ranks of the data & the expected median under the H0. A smaller V would indicate evidence in favor of the alternative hypothesis.
# alternative hypothesis: true location is less than 160
# 95 percent confidence interval:
#  -Inf  169.2
# sample estimates:
#  (pseudo)median 
# 164.85   # estimated population median from the data, wish is greater than 160.

# Interpretation: Since, the p-value is much greater than 0.05, we do not reject the null hypothesis. So, there is no sufficient evidence to conclude that the population median is less than 160. 

####### Section-3 (Two Sample Tests) #######

# Q-1 (Two sample Z-test)

# Q-1.a: Write a function to perform a two sample Z test given data sets x1, x2, their respective standard deviations, significance level and null hypothesis. 
# As above, the function should return the conclusions of the test along with the statistic values.

two_sample_Z_test <- function(x1, x2, sigma_x1, sigma_x2, alpha, null_hypothesis = "equal") {
  x1_bar <- mean(x1)
  x2_bar <- mean(x2)
  n1 <- length(x1)
  n2 <- length(x2)
  # Calculate the Z-statistic based on the chosen null hypothesis -
  if (null_hypothesis == "equal") {
    null_value <- 0
  } else if (null_hypothesis == "greater") {
    null_value <- 0   # Tests if mu1 >= mu2
  } else if (null_hypothesis == "less") {
    null_value <- 0   # Tests if mu1 <= mu2
  } else {
    stop("Invalid null hypothesis. Choose 'equal', 'greater', or 'less'.")
  }
  Z <- (x1_bar - x2_bar - null_value) / sqrt((sigma_x1^2 / n1) + (sigma_x2^2 / n2))
  # Find the critical Z-value based on the significance level and type of test -
  if (null_hypothesis == "equal") {    # Two-tailed test
    Z_critical <- qnorm(1 - alpha / 2)
  } else if (null_hypothesis == "greater") {    # Right-tailed test
    Z_critical <- qnorm(1 - alpha)
  } else if (null_hypothesis == "less") {   # Left-tailed test
    Z_critical <- qnorm(alpha)
  }
  # Calculate p-value for the test -
  if (null_hypothesis == "equal") {
    p_value <- 2 * (1 - pnorm(abs(Z)))   # Two-tailed p-value
  } else if (null_hypothesis == "greater") {
    p_value <- 1 - pnorm(Z)   # Right-tailed p-value
  } else if (null_hypothesis == "less") {
    p_value <- pnorm(Z)   # Left-tailed p-value
  }
  # Conclusion based on the Z-statistic and critical value -
  if (abs(Z) > Z_critical) {
    conclusion <- paste("Reject H0: There is enough evidence to reject the null hypothesis (", null_hypothesis, ").", sep = "")
  } else {
    conclusion <- paste("Fail to reject H0: There is not enough evidence to reject the null hypothesis (", null_hypothesis, ").", sep = "")
  }
  return(list(Z_statistic = Z, Z_critical = Z_critical, p_value = p_value, Conclusion = conclusion))
}

# Q-1.b: Use the data given in two-sample.dat for this problem with σx1 = 24.6 and σx2 = 27.8 with α = 0.05 to test the null hypothesis that the μ1 ≥ μ2.

x1 = c( 258.0, 271.5, 189.1, 216.5, 237.2, 222.0, 231.3, 181.7, 220.0, 179.3, 238.1, 217.7, 246.2, 241.5, 233.8, 222.3, 199.2, 167.9, 216.2, 240.4, 235.3, 187.0, 233.7, 214.7,
        174.6, 246.3, 185.7, 207.0, 244.3, 237.7, 245.2, 228.3, 201.8, 218.3, 242.7, 213.8, 231.9, 257.3, 208.4, 250.7, 198.3, 206.7, 259.7, 253.3, 200.3, 196.6, 210.6, 257.6,
        173.5, 267.5, 167.2, 227.1, 172.1, 197.6, 256.9, 203.7, 195.1, 237.4, 210.2, 208.8, 218.0, 205.1, 241.1, 216.8, 223.6, 191.0, 225.9, 215.1, 233.1, 243.0)

x2 = c( 221.0, 213.0, 199.3, 211.2, 225.2, 229.1, 253.9, 194.6, 243.0, 221.9, 230.9, 221.1, 206.7, 217.2, 215.8, 203.0, 234.0, 196.3, 235.8, 234.3, 244.7, 248.8, 200.5, 232.0,
        233.3, 220.6, 289.2, 244.9, 230.8, 182.9, 199.3, 263.2, 220.6, 266.7, 258.0, 243.9, 178.1, 200.7, 270.2, 224.4, 222.4, 234.6, 296.7, 202.3, 277.9, 204.3, 221.1, 257.0,
        243.4, 239.4, 230.0, 263.5, 241.3, 216.6, 227.9, 230.1, 230.5, 188.6, 289.3, 234.4, 267.5, 256.0, 246.5, 210.5, 270.6, 295.5, 195.8, 235.3, 245.4, 245.4)

sigma_x1 <- 24.6   # Standard deviation for sample 1
sigma_x2 <- 27.8   # Standard deviation for sample 2
alpha <- 0.05      # Significance level

# Perform the two-sample Z test for different null hypotheses -
# Test for equal means (two-tailed) -
result_equal <- two_sample_Z_test(x1, x2, sigma_x1, sigma_x2, alpha, null_hypothesis = "equal")
# Test for greater means (right-tailed) -
result_greater <- two_sample_Z_test(x1, x2, sigma_x1, sigma_x2, alpha, null_hypothesis = "greater")
# Test for less means (left-tailed) -
result_less <- two_sample_Z_test(x1, x2, sigma_x1, sigma_x2, alpha, null_hypothesis = "less")

# cat("Result for Two-Tailed Test (Equal Means): \n")
# cat("Z-statistic:", result_equal$Z_statistic, "\n")
# cat("Critical Z-value:", result_equal$Z_critical, "\n")
# cat("P-value:", result_equal$p_value, "\n")
# cat("Conclusion:", result_equal$Conclusion, "\n\n")
# 
# cat("Result for Right-Tailed Test (Greater Means): \n")
# cat("Z-statistic:", result_greater$Z_statistic, "\n")
# cat("Critical Z-value:", result_greater$Z_critical, "\n")
# cat("P-value:", result_greater$p_value, "\n")
# cat("Conclusion:", result_greater$Conclusion, "\n\n")

cat("Result for Left-Tailed Test (Less Means): \n")
cat("Z-statistic:", result_less$Z_statistic, "\n")   # Output: -2.808937
cat("Critical Z-value:", result_less$Z_critical, "\n")   # Output: -1.644854 
cat("P-value:", result_less$p_value, "\n")   # Output: 0.002485267 
cat("Conclusion:", result_less$Conclusion, "\n")   # Output: Reject H0

# Q-2 (Two sample t-test)

# In R, both one-sample and two-sample t-tests can be performed with the library function t.test().
# t.test(x,y,alternative,mu,paired,var.equal,conf.level) where x,y are the data vectors, alternative is a character string specifying the alternate hypothesis 
# of 3 possible values: two.sided, less and greater. One can also input a vector with all three, in which case all the 3 hypotheses are tested, the default value is two.sided. 
# For one-sample, mu is the hypothesized mean of the population, and for two samples, it is the hypothesized difference of the means. A conf.level=0.95 sets the confidence level at 95%.

# Q-2.a: Welsch’s test: use the data sets to carry out the t-test for equality of means as H0. Print the results summary.

Xvar <- c(4.95,5.37,4.70,4.96,4.72,5.17,5.28,5.12,5.26,5.48)
Yvar <- c(4.65,4.86,4.57,4.56,4.96,4.63,5.04,4.92,5.37,4.58,4.26,4.40)
welsch_test <- t.test(Xvar, Yvar, alternative='two.sided',    # H1: means are not equal
                      mu=0,   # mean difference = 0 (H0); mentioned bec. these are independent samples.
                      var.equal=FALSE,   # In Welsch's test, variances are not assumed equal.
                      conf.level=0.95)
print(welsch_test)
# Interpretation of output:
# p-value = 0.006749 ; Since, p < 0.05, reject the null hypothesis - the means are significantly different.
# 95 percent confidence interval: 0.1138185  0.6215148 ; Since, the C.I. for difference in means doesn't include 0, which supports rejecting the null hypothesis.

# Q-2.b: Use the data sets below to carry out a paired t-test for a significance level of 0.05. In this case, do we need to input μ?

data_before <- c(95,106,79,71,90,79,71,77,103,103,92,63,82,76)
data_after <- c(97,116,82,81,82,86,107,86,94,91,85,98,91,87)
welch_test1 <- t.test(data_before, data_after, alternative='two.sided',   # H0: No difference in means of data_before and data_after (mu=0)
       paired=TRUE,    # It specifies that the two samples are dependent or paired.
       conf.level=0.95   # Since, alpha=0.05 (significance level)
       )
# Since, samples are related or dependent, mu = 0 is assumed as the null hypothesis for paired t-tests.
# No need to input mu in the R function bec. it is automatically assumed that we're testing whether the mean difference is zero (no effect).
print(welch_test1)
# Interpretation of output:
# p-value = 0.101 ; Since p-value > 0.05, so we fail to reject the null hypothesis.
# 95 percent confidence interval: -15.248261  1.533975 ; Since the C.I. includes 0, this suggests that there is no significant difference b/w "before" & "after" measurements at 95% confidence level.
#  mean difference: -6.857143 ; observed difference is not statistically significant.

# Q-3 (Two sample proportion test)

# In R, the function prop.test() can perform proportion tests for one, two or more proportions.
# prop.test(x,n,p=NULL,alternative="two.sided",correct=TRUE) where x is a 2x2 matrix containing the values of the contingency table under different categories for Fisher’s test 
# & a data vector of counts of successes for the prop.test(), n is a data vector containing no. of trials in which x successes were observed, 
# p is a vector of probabilites of successes. The alternative can be two.sided, less or more.

# Q-3.a: Perform a prop.test() test for the following problem with H0 that the proportions are equal. In a health survey, 520 out of 600 men and 550 out of 600 women
# questioned said they use antibiotics whenever fever continues for more than 2 days. We want to test whether there is a significant difference in the fraction of men and
# women who start taking antibotics after 2 days of fever.

x <- c(520, 550)
n <- c(600, 600)
result <- prop.test(x, n, alternative='two.sided', correct=FALSE)    # H0: p1=p2 ; no need to specify p bec. it will automatically test for equality b/w two sample proportions.
# When the sample size is large, the continuity correction becomes unnecessary bec. the normal approximation to the binomial is already accurate. 
print(result)
# Interpretation of output:
# p-value = 0.005329  ; p < 0.05, so we reject the null hypothesis.
# 95 percent confidence interval: -0.08505583   -0.01494417
# There is a statistically significant difference in the proportion of men and women who take antibiotics after 2 days of fever.

# Q-3.b: Perform a Fisher’s exact test for the following data to find whether the patients from higher income group indulge in tobacco abuse 
# in a significantly different proportion than the patients from the lower income group.

# Fisher's exact test checks for association between two categorical variables. It is accurate for small sample sizes. It gives an exact p-value instead of an approximation.

data <- matrix(c(11, 17, 42, 39), nrow=2, byrow=TRUE)    # 2 by 2 matrix containing the values of the contingency table under different categories. 
fisher <- fisher.test(data, alternative = 'two.sided', conf.int=TRUE, conf.level=0.95)   # two.sided bec. we're checking for any difference in abuse rates b/w groups.
print(fisher)
# Interpretation of output: 
# p-value = 0.2797 ; since p > 0.05, we fail to reject the null hypothesis. There's no statistically significant difference in tobacco abuse between higher-income & lower-income groups.
# The odds ratio < 1 suggests lower odds of abuse in the higher-income group, but the result is not significant.
# The 95% confidence interval shows the range where the true odds ratio is likely to fall. Since 1 lies within (0.225, 1.564), the result is not statistically significant.

# Q-4 (Two sample variance test)

# F-test: compares the variances of two independent samples to test whether they are significantly different.

# Q-4.a: Write a function of the form two_sample_variance_test(x,y,alpha) that outputs the statistical conclusion along with the statistical values of F and p-values.
two_sample_variance_test <- function(x, y, alpha) {
  n1 <- length(x)
  n2 <- length(y)
  var_x <- var(x)
  var_y <- var(y)
  # Calculate F-statistic (larger variance divided by smaller variance)
  if (var_x > var_y) {
    F_stat <- var_x / var_y
    df1 <- n1 - 1
    df2 <- n2 - 1
  } else {
    F_stat <- var_y / var_x
    df1 <- n2 - 1
    df2 <- n1 - 1
  }
  # This determines whether we want the lower or upper tail of the F-distribution. In a two-tailed test, we use both the tails to calculate the p-value.
  p_value <- 2*min(pf(F_stat, df1, df2, lower.tail = FALSE), pf(F_stat, df1, df2, lower.tail = TRUE))
  result = c(F_statistic=F_stat, p_value=p_value)
  lower_critical <- qf(alpha/2, df1, df2)
  upper_critical <- qf(1-(alpha/2), df1, df2)
  if (F_stat < lower_critical || F_stat > upper_critical) {
    conclusion <- "Reject null hypothesis: The variances are significantly different."
  } else {
    conclusion <- "Fail to reject null hypothesis. No significant difference in variances."
  }
  cat("Statistical Conclusion:", conclusion, "\n")
  return(result)
}

# Q-4.b:
x = c(1067.7, 984.3,998.8,1025.9,1060.9,959.1,1013.8,1047.0,987.8,1051.0,885.2,1049.5,1098.2,1001.5,1011.1,991.6)
y = c(957.6, 981.8, 1096.5, 984.4, 1074.3, 929.4, 1056.0,1012.3, 1040.7, 1099.5,1006.1, 1064.3, 865.6, 944.4, 1091.8, 952.1)
two_sample_variance_test(x, y, alpha = 0.05)
# Output: p_value = 0.2337848 (Fail to reject null hypothesis.)

# Q-5: Wilcoxon signed rank test for two dependent samples: This is carried out using wilcox.test() function in R again, and the parameters are already described above in the one sample
# tests. Carry out this test with conf.level=0.95 for the null hypothesis that the mean for the paired sample is greater than 0, i.e. the two samples have different means.

wilcoxon_signed_rank_test <- function(pre, post, alpha = 0.05) {
  result <- wilcox.test(pre, post, alternative = "greater", paired = TRUE, conf.level = 1 - alpha,
                        exact = FALSE)  # Use normal approximation for p-value when ties are present
  cat("Wilcoxon Signed-Rank Test Result:\n")
  cat("Test Statistic (V):", result$statistic, "\n")
  cat("p-value:", result$p.value, "\n")
  cat("Significance level (alpha):", alpha, "\n")
  if (result$p.value < alpha) {
    cat("Conclusion: Reject H0 - Post_therapy values are significantly lower than Pre_therapy.\n")
  } else {
    cat("Conclusion: Fail to reject H0 - No significant difference detected.\n")
  }
}
Pre_therapy <- c(74, 72, 62, 58, 59, 65, 54, 63, 80, 66, 65, 64, 79, 60)
Post_therapy <- c(79, 55, 53, 53, 74, 55, 64, 55, 39, 44, 37, 68, 54, 54)
wilcoxon_signed_rank_test(Pre_therapy, Post_therapy)
# Output: p-value: 0.02222418, so reject H0.

# Q-6: Wilcoxon rank sum test for unpaired samples and Mann-Whitney test: Use the wilcox.test() function to carry out the Wilcoxon rank sum test for two independent samples 
# given below with the alternate hypothesis that the placebo population has a smaller mean than that exposed to the drug. Use a confidence level of 95%. 

wilcoxon_rank_sum_test <- function(group1, group2, alpha = 0.05) {
  result <- wilcox.test(group2, group1, alternative = "less", paired = FALSE, conf.level = 1 - alpha,
                        exact = FALSE)   # handles ties if present
  cat("Wilcoxon Rank Sum Test (Mann-Whitney Test):\n")
  cat("Test Statistic (W):", result$statistic, "\n")
  cat("p-value:", result$p.value, "\n")
  cat("Significance level (alpha):", alpha, "\n")
  if (result$p.value < alpha) {
    cat("Conclusion: Reject H0 - Placebo group has significantly smaller mean than Drug group.\n")
  } else {
    cat("Conclusion: Fail to reject H0 - No significant difference in means between groups.\n")
  }
}
drug <- c(31.7, 75.0, 101.1, 60.5, 62.8, 59.3, 58.9, 91.3, 99.1, 52.0, 39.1)
placebo <- c(59.3, 72.7, 100.5, 64.7, 69.0, 72.7, 69.6, 97.4, 100.6, 65.1, 65.7)
wilcoxon_rank_sum_test(drug, placebo)
# Output: p-value: 0.9303359, so fail to reject H0.

# Q-7: Kruskal Wallis test: In R, this test is performed by kruskal.test() function. Reform the data into a (x,y) form where x stands for the value and y is the category of the 
# group (use rep() function to label each data point according to the group), then use the above R function with arguments x and y. Print the results output by the function.

kruskal_wallis_test <- function(x, y, alpha = 0.05) {
  result <- kruskal.test(x ~ y) 
  cat("Kruskal-Wallis Test Result:\n")
  cat("Test Statistic (H):", result$statistic, "\n")
  cat("p-value:", result$p.value, "\n")
  cat("Significance level (alpha):", alpha, "\n")
  if (result$p.value < alpha) {
    cat("Conclusion: Reject H0 - Significant difference between the groups.\n")
  } else {
    cat("Conclusion: Fail to reject H0 - No significant difference between the groups.\n")
  }
}
group_1 <- c(220, 214, 203, 184, 186, 200, 165)
group_2 <- c(262, 193, 225, 200, 164, 266, 179)
group_3 <- c(272, 192, 190, 208, 231, 235, 141)
group_4 <- c(190, 255, 247, 278, 230, 269, 289)
# Create the corresponding labels for each group -
x_values <- c(group_1, group_2, group_3, group_4)
y_labels <- rep(1:4, times = c(length(group_1), length(group_2), length(group_3), length(group_4)))
kruskal_wallis_test(x_values, y_labels)
# Output: p-value: 0.06898404, so fail to reject H0.

# Q-8: Chi-square GoF test: Based on what we learnt in class, write a function to perform the GoF test based on input data of expected and observed values. 
# We will use qchisq() function to get the critical value and pchisq() to get the p-value.

chi_square_gof_test <- function(observed, expected, alpha = 0.05) {
  chi_square_statistic <- sum((observed - expected)^2 / expected)   # Calculate the Chi-sq. statistic.
  df <- length(observed) - 1   # Degrees of freedom = no. of categories - 1
  critical_value <- qchisq(1 - alpha, df)
  p_value <- 1-pchisq(chi_square_statistic, df)
  cat("Chi-Square Goodness of Fit Test Result:\n")
  cat("Chi-Square Statistic:", chi_square_statistic, "\n")
  cat("Critical Value:", critical_value, "\n")
  cat("p-value:", p_value, "\n")
  if (p_value < alpha) {
    cat("Conclusion: Reject H0- Significant difference between observed and expected frequencies.\n")
  } else {
    cat("Conclusion: Fail to reject H0 - No significant difference between observed and expected frequencies.\n")
  }
}
# Data: Observed and Expected frequencies -
observed <- c(32, 82, 77, 49)
expected <- c(40, 80, 80, 40)
chi_square_gof_test(observed, expected)
# Output: p-value: 0.2853434, so fail to reject H0.

####### Section-4 (One-way ANOVA - multiple sample tests) ######## 

# Q-1 (ANOVA test on people on the Titanic ship)

# Q-1.a: Read in the data file called titanic.csv. Make histogram plots of groups of people marked as ‘1st’, ‘2nd’ and ‘3rd’ (use about 30 bins) to check whether 
# the three samples have approximately the same variance and are looking approximately normal. Then we are justified in using ANOVA. Make the plots in a 3x1 grid. 
# Our null hypothesis is that the mean age is same among the 3 groups of people.

library(ggplot2)
library(dplyr)

titanic_data <- read.csv("/home/ibab/Downloads/titanic.csv")
titanic_data$passenger_class <- as.factor(titanicData$passenger_class)   # # Ensure passenger_class is treated as a factor
print(levels(titanic_data$passenger_class))   # To check levels are 1st, 2nd, 3rd
par(mfrow = c(3,1))    # 3 rows, 1 column

# Plot the histogram for 1st class
hist(titanic_data$age[titanic_data$passenger_class == "1st"], breaks=30, main="Age Distribution - 1st Class", xlab="Age", col="red", border="black")
# Plot histogram for 2nd class
hist(titanic_data$age[titanic_data$passenger_class == "2nd"], breaks=30, main="Age Distribution - 2nd Class", xlab="Age", col="blue", border="black")
# Plot histogram for 3rd class
hist(titanic_data$age[titanic_data$passenger_class == "3rd"], breaks=30, main="Age Distribution - 3rd Class", xlab="Age", col="yellow", border="black")
par(mfrow = c(1,1))   # Reset plotting area back to default (optional)

# Interpretation: Hard to infer anything about normality or equality of variance.

# Q-1.b: To quantitatively verify the equal variance assumption, we are going to determine the mean and standard deviations from each group. Load the package dplyr, we will use
# two functions group_by() and summarise(). What do you find? Are the standard deviations similar between the groups? Print a statement showing the conclusion of this comparison.

titanic_by_passenger_class <- group_by(titanic_data, passenger_class)   # Group the data by passenger class and calculate mean and standard deviation
# Summarize mean and standard deviation of age by group
summary_stats <- summarise(titanic_by_passenger_class, group_mean = mean(age, na.rm=TRUE), group_sd = sd(age, na.rm=TRUE))
print(summary_stats)
cat("The standard deviations across passenger classes (14.9, 13.0, 11.3) are reasonably similar.\n")
cat("Thus, the assumption of equal variance needed for ANOVA is satisfied.\n")

# Q-1.c: We fit the ANOVA model to the data using lm() function. This function takes a formula and data frame as arguments. A model formula takes the form of a response variable 
# followed by a tilde( ) and then at least one explanatory variable. Here we will give age~passenger_class which tells R to ‘fit’ a model in which age of passengers are grouped 
# by the variable passenger_class. The anova() returns the ANOVA table. What is statistical inference/decision from the table, & therefore what is the statistical conclusion?

lmresults <- lm(age ~ passenger_class, data=titanic_data)   # Fit the ANOVA model
anova_results <- anova(lmresults)   # Perform ANOVA
print(anova_results)

# Conclusion:
# The F-value (75.903) is large, indicating that there is a significant difference between the passenger classes in terms of age.
# The p-value (< 2.2e-16) is extremely small, confirming that the difference is statistically significant.
# Thus, we reject the null hypothesis and conclude that there are significant differences in age among the passenger classes.

# '*' (p <= 0.001)**: Very highly significant, strong evidence against the null hypothesis.
# '' (0.001 < p <= 0.01)**: Significant, strong evidence against the null hypothesis.
# '*' (0.01 < p <= 0.05): Marginally significant, some evidence against the null hypothesis.
# '.' (0.05 < p <= 0.1): Borderline significant, weak evidence against the null hypothesis.
# ' ' (p > 0.1): Not significant, not enough evidence to reject the null hypothesis.

# Q-1.d: The ANOVA tells us that at least one group has a mean different from the others, but does not tell us which group means are actually different. A Tukey-Kramer’s
# test tests the null hypothesis that there is no difference between the population means of all pairs of groups. This is invoked in R by using TukeyHSD() function.
# Look at the columns labeled ‘diff’ and ‘p adj’. The p-values are calculated using a 95% confidence interval, and ‘lwr’ and ‘upr’ denote lower and upper bounds of the
# interval. From the output you should be able to see that the CIs do not include zero, and since the p-value is less than 0.05 in all the cases, the H0 is rejected for all
# pairs, and we will conclude that the means of the three populations are significantly different from each other.

tukey_results <- TukeyHSD(aov(lmresults))    # Perform Tukey's HSD test
print(tukey_results)

# Interpretation:
# The p-values for all comparisons (2nd vs 1st, 3rd vs 1st, 3rd vs 2nd) are very small (0.0000000 and 0.0116695), which means that all pairwise comparisons are statistically significant. 
# This confirms that the mean ages of the passengers in different classes are significantly different from each other.
# 2nd vs 1st: Mean age of 2nd class is significantly lower than 1st class.
# 3rd vs 1st: Mean age of 3rd class is significantly lower than 1st class.
# 3rd vs 2nd: Mean age of 3rd class is significantly lower than 2nd class

# Q-1.e: Let us also perform a Kruskal-Wallis test for the above data since the test does not need us to assume normal distribution like ANOVA. 
# Check whether the p value leads to the same conclusion as the parametric test above.

# The Kruskal-Wallis test is a non-parametric alternative to ANOVA that doesn't assume normality. 
# It tests whether there are significant differences between the medians of the groups.
kruskal_results <- kruskal.test(age ~ passenger_class, data=titanic_data)    # Perform Kruskal-Wallis test
print(kruskal_results)

# Interpretation:
# Kruskal-Wallis chi-squared = 116.08, df = 2, p-value < 2.2e-16:
# The p-value is extremely small (< 2.2e-16), indicating that there is a significant difference between the passenger classes in terms of age.
# Since the p-value is much smaller than the significance level (usually 0.05), we reject the null hypothesis that the median ages of the groups are equal. 
# This suggests that the ages of passengers from different classes are significantly different.

# Conclusion: The Chi-Square Goodness of Fit test suggests that there is no significant difference b/w the observed and expected frequencies, while both the 
# Tukey HSD and Kruskal-Wallis tests suggest significant differences in the mean or median ages of passengers in different classes.

# Q-2 (Cuckoo egg size problem)

# Q-2.a: The European cuckoo does not look after its own eggs, but instead lays them in the nests of birds of other species. Previous studies showed that cuckoos sometimes
# have evolved to lay eggs that are colored similarly to the host bird species’ eggs. Is the same true of egg size – do cuckoos lay eggs similar in size to the size of the
# eggs of their hosts? The data file “cuckooeggs.csv” contains data on the lengths of cuckoo eggs laid in the nests of a variety of host species. Here we compare the
# mean size of cuckoo eggs found in the nests of different host species. Plot a multiple histogram showing cuckoo egg lengths by host species.

cuckoo_data <- read.csv("/home/ibab/Downloads/cuckooeggs.csv")   # Load the .csv file
str(cuckoo_data)   # Structure
dim(cuckoo_data)   # Dimensions
head(cuckoo_data)   # First few lines
len_of_unique <- length(unique(cuckoo_data$host_species))
# Plot histograms for each host species
par(mfrow = c(len_of_unique/2, 2))   # Set layout
for (species in unique(cuckoo_data$host_species)) {
  hist(cuckoo_data$egg_length[cuckoo_data$host_species==species], main=species, xlab="Egg Length", col="red",breaks=10)
}
par(mfrow = c(1,1))   # Optional

# Q-2.b: Calculate a table that shows the mean and standard deviation of length of cuckoo eggs for each host species. Look at the graph and the table. For these data, would
# ANOVA be a valid method to test for differences between host species in the lengths of cuckoo eggs in their nests?

group_cuckoo_byspecies <- group_by(cuckoo_data, host_species)   # Group the data by host species.
# Summarize mean and standard deviation by species
summary_stats <- summarise(group_cuckoo_byspecies, group_mean = mean(egg_length, na.rm = TRUE), group_sd = sd(egg_length, na.rm = TRUE))
print(summary_stats)
cat("The standard deviations across host species classes are similar.\n")
cat("Thus, the assumption of equal variance needed for ANOVA is satisfied.\n")

# Q-2.c: Use ANOVA to test for a difference between host species in the mean size of the cuckoo eggs in their nests. What is your conclusion?

anova_model <- aov(egg_length ~ host_species, data = cuckoo_data)    # Fit ANOVA model
summary(anova_model)

# Conclusion: If p-value < 0.05, then means are different across species.

# Q-2.d: Assuming that ANOVA rejected the null hypotheses of no mean differences, use a Tukey-Kramer test to decide which pairs of host species are significantly 
# different from each other in cuckoo egg mean length. What is your conclusion?

tukey_result <- TukeyHSD(anova_model)   # Perform Tukey's HSD (Honestly Significant Difference) test
print(tukey_result)

# Conclusion:
# Significant (p < 0.05):
# Meadow Pipit vs Hedge Sparrow (p = 0.043): Significant
# Wren vs Hedge Sparrow (p ≈ 0.0000006): Highly Significant
# Tree Pipit vs Meadow Pipit (p ≈ 0.047): Significant
# Wren vs Meadow Pipit (p ≈ 0.000486): Highly Significant
# Wren vs Pied Wagtail (p ≈ 0.0000070): Highly Significant
# Wren vs Robin (p ≈ 0.000318): Highly Significant
# Wren vs Tree Pipit (p ≈ 0.0000006): Highly Significant

# Not significant (p > 0.05):
# Pied Wagtail vs Hedge Sparrow
# Robin vs Hedge Sparrow
# Tree Pipit vs Hedge Sparrow
# Pied Wagtail vs Meadow Pipit
# Robin vs Meadow Pipit
# Robin vs Pied Wagtail
# Tree Pipit vs Pied Wagtail
# Tree Pipit vs Robin

# Q-3 (Maize and malaria problem)

# Q-3.a: The pollen of the maize (corn) plant is a source of food to larval mosquitoes of the species Anopheles arabiensis, the main vector of malaria in Ethiopia. The
# production of maize has increased substantially in certain areas of Ethiopia recently, and over the same time period, malaria has entered in to new areas where it was
# previously rare. This raises the question, is the increase of maize cultivation partly responsible for the increase in malaria? One line of evidence is to look for an 
# association b/w maize production and malaria incidence at different geographically dispersed sites (Kebede et al. 2005). The data set “malaria vs maize.csv” contains info. on 
# several high-altitude sites in Ethiopia, with information about the level of cultivation of maize (low, medium or high in the variable maize yield) and the rate of malaria
# per 10,000 people (incidence rate per ten thousand). Plot a multiple histogram to show the relationship between level of maize production & the incidence of malaria.

malaria_data <- read.csv("/home/ibab/Downloads/malaria vs maize.csv")
print(head(malaria_data))

# Subset data
low <- malaria_data$incidence_rate_per_ten_thousand[malaria_data$maize_yield == "Low"]
medium <- malaria_data$incidence_rate_per_ten_thousand[malaria_data$maize_yield == "Medium"]
high <- malaria_data$incidence_rate_per_ten_thousand[malaria_data$maize_yield == "High"]

par(mfrow = c(1, 3))   # 1 row, 3 columns
hist(low, main = "Low Maize Yield", xlab = "Malaria Incidence Rate", col = "red", breaks = 10)
hist(medium, main = "Medium Maize Yield", xlab = "Malaria Incidence Rate", col = "blue", breaks = 10)
hist(high, main = "High Maize Yield", xlab = "Malaria Incidence Rate", col = "yellow", breaks = 10)
par(mfrow = c(1,1))   # Reset layout

# Q-3.b: ANOVA is a logical choice of method to test differences in the mean rate of malaria b/w sites differing in level of maize production. Calculate the standard deviation 
# of the incidence rate for each level of maize yield. Do these data seem to conform to the assumptions of ANOVA? Describe any violations of assumptions you identify.

sd_low <- sd(low)
sd_medium <- sd(medium)
sd_high <- sd(high)
cat("Standard deviations (original data):\n")
cat("Low:", sd_low, "\n")
cat("Medium:", sd_medium, "\n")
cat("High:", sd_high, "\n")

# Conclusion: If standard deviations are very different, then the variance homogeneity assumption is violated.

# Q-3.c: Compute the log of the incidence rate and redraw the multiple histograms for different levels of maize yield. Calculate the standard deviation of the log incidence
# rate for each level of maize yield. Does the log-transformed data better meet the assumptions of ANOVA than did the un-transformed data?

malaria_data$log_incidence <- log(malaria_data$incidence_rate_per_ten_thousand)   # Log-transform incidence rates
# Subset log data
low_log <- malaria_data$log_incidence[malaria_data$maize_yield == "Low"]
medium_log <- malaria_data$log_incidence[malaria_data$maize_yield == "Medium"]
high_log <- malaria_data$log_incidence[malaria_data$maize_yield == "High"]
# Plot histograms for log-transformed data
par(mfrow = c(1, 3))
hist(low_log, main = "Low Maize Yield (Log)", xlab = "Log Malaria Incidence Rate", col = "darkgreen", breaks = 10)
hist(medium_log, main = "Medium Maize Yield (Log)", xlab = "Log Malaria Incidence Rate", col = "maroon", breaks = 10)
hist(high_log, main = "High Maize Yield (Log)", xlab = "Log Malaria Incidence Rate", col = "orange", breaks = 10)
par(mfrow = c(1,1))   # Reset layout

# Calculate standard deviations of log data.
sd_low_log <- sd(low_log)
sd_medium_log <- sd(medium_log)
sd_high_log <- sd(high_log)
cat("Standard deviations (log-transformed data):\n")
cat("Low (log):", sd_low_log, "\n")
cat("Medium (log):", sd_medium_log, "\n")
cat("High (log):", sd_high_log, "\n")

# Q-3.d: Test for an association between maize yield and malaria incidence.

anova_result <- aov(log_incidence ~ maize_yield, data = malaria_data)   # ANOVA on log-transformed data
summary(anova_result)

# Q-4 (Circadian rhythms of diseased animals)

# Q-4.a: Animals that are infected with a pathogen often have disturbed circadian rhythms.(A circadian rhythm is an endogenous daily cycle in a behavior or physiological
# trait that persists in the absence of time cues.) Shirasu-Hiza et al. (2007) wanted to know whether it was possible that the circadian timing mechanism itself could have an 
# effect on disease. To test this idea they sampled from three groups of fruit flies: one “normal”, one with a mutation in the timing gene tim01, and one group that had the 
# tim01 mutant in a heterozygous state. They exposed these flies to a dangerous bacteria, Streptococcus pneumoniae, and measured how long the flies lived afterwards, in days. 
# The date file “circadian mutant health.csv” shows some of their data. Plot a histogram of each of the three groups. Do these data match the assumptions of an ANOVA?

circadian_data <- read.csv("/home/ibab/Downloads/circadian mutant health.csv")   # Load the data
# Subset data for each genotype -
tim01_data <- circadian_data$days_to_death[circadian_data$genotype == "tim01"]
rescued_data <- circadian_data$days_to_death[circadian_data$genotype == "tim01 (rescued)"]
wildtype_data <- circadian_data$days_to_death[circadian_data$genotype == "wild type"]

par(mfrow = c(1, 3))   # Set up 1 row 3 columns layout for plots.
hist(tim01_data, main = "tim01 Mutant", xlab = "Days to Death", col = "red", breaks = 10, xlim = c(0, 22))
hist(rescued_data, main = "tim01 Rescued", xlab = "Days to Death", col = "blue", breaks = 10, xlim = c(0, 22))
hist(wildtype_data, main = "Wild Type", xlab = "Days to Death", col = "green", breaks = 10, xlim = c(0, 22))
par(mfrow = c(1,1))   # Reset layout

# Q-4.b: Use a Kruskal-Wallis test to ask whether lifespan differs between the three groups of flies.
kruskal.test(days_to_death ~ genotype, data = circadian_data)
# If the p-value is < 0.05, it means there is a statistically significant difference in median survival times between at least two groups.








