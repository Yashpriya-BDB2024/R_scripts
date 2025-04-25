###### LAB-15 (DATE: 25-4-25) ######

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

###### Section-3 (Two sample tests) ######

# Q-1 (Two sample Z test)







