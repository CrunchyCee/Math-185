### HW 2

## Problem 1

# Part A
# The sign vector of length n, where each element has two outcomes, has 2^n
# possible outcomes. We count each sign vector for which the sample mean is greater
# than or equal to the the original sample mean, then set this over the
# number of possible outcomes 2^n to find the p-value.

# Part B
# one-sided test
# null hypothesis: distribution is symmetric about 0
# rejects the null if the distribution is skewed to the right
flipSignTest1 = function(x, B=999) {
  n = length(x)
  xbar = mean(x)
  flipbar = numeric(B)
  for (b in 1:B) {
    # generates length n sequence of ones and negative ones
    epsilon = sample(seq(-1, 1, by = 2), size = n, replace = TRUE)
    flip = epsilon*x
    flipbar[b] = mean(flip)
  }
  pval = (sum(flipbar >= xbar) + 1)/(B + 1)
  # the function outputs the p-value
  pval
}

# Part C
# two-sided test
# null hypothesis: distribution is symmetric about 0
# rejects the null if the distribution is not symmetric
flipSignTest2 = function(x, B=999) {
  n = length(x)
  xbar = mean(x)
  flipbar = numeric(B)
  for (b in 1:B) {
    # generates length n sequence of ones and negative ones
    epsilon = sample(seq(-1, 1, by = 2), size = n, replace = TRUE)
    flip = epsilon*x
    flipbar[b] = mean(flip)
  }
  # sum instances where flipbar is greater than or less than xbar
  # which is equivalent to being equal to xbar
  pval = (sum(flipbar == xbar) + 1)/(B + 1)
  # the function outputs the p-value
  pval
}

## Problem 2
require(UsingR)
dat = father.son
X = dat$fheight
Y = dat$sheight

# Part A
boxplot(X,Y,horizontal=TRUE,names=c("Father's Height","Son's Height"))
# from this informal boxplot it appears that sons are usually taller than fathers
# though it's not yet known whether the difference is statistically significant

#Part B
Z = Y - X
flipSignTest1(Z)
# The result of 0.001 shows us that the father-son height difference is not
# symmetric around 0. It is likely skewed to the right, based on the boxplot.
# This test is a permutation test since it requires one to test every
# permutation of the length n sequence of signs

## Problem 3
B = 2000
out = numeric(B)
for (b in 1:B) {
  x = sample(Z, n, TRUE) # bootstrap sample
  out[b] = mean(x)
}
alpha = 0.10
I = quantile(out, alpha)
# 90% of the time, the mean difference in father-son height is greater than
# or equal to 0.88 inches, so we can assume that the sons are generally
# taller than the fathers. We are drawing an inference about the total
# population of fathers and sons from this sample