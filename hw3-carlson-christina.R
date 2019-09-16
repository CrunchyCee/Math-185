### HW 3

## Problem 1

# Part A
m = 1000
n = 3000
B = 10000
out = numeric(B)
meandif = rnorm(B, 0, (1/m) + (1/n))

for (b in 1:B) {
  X = rnorm(m) 
  Y = rnorm(n, 0, 5)
  X = sample(X)
  Y = sample(Y)
  out[b] = mean(X) - mean(Y)
}

summary(out)
sd(out)
summary(meandif)
sd(meandif)
# the distribution before permutation is less spread out than
# the distribution after permutation
  
# Part B
m = 2000
n = 2000
B = 10000
out = numeric(B)
meandif = rnorm(B, 0, (1/m) + (1/n))

for (b in 1:B) {
  X = rnorm(m) 
  Y = rnorm(n, 0, 5)
  X = sample(X)
  Y = sample(Y)
  out[b] = mean(X) - mean(Y)
}

summary(out)
sd(out)
summary(meandif)
sd(meandif)

## Problem 2
load('cloudseeding.rda')
dat = cloudseeding # shorthand

# Part A
# For number of runs, if the data are identically distributed there is a high
# number of runs, but if the data are not iid there is a low number of runs
# H0: The distributions of the two samples in cloudseeding are identical
# H1: The distributions are different
# We choose a p-value of 0.05

# Part B
wilcox.test(dat$unseeded, dat$seeded)

# Part C
nb.runs.test = function(x, y, B=999) {
  W_test = qwilcox(0.5/2,length(x),length(y))
  W = numeric(B)
  for(b in 1:B) {
    x = sample(x,replace = TRUE)
    y = sample(y,replace = TRUE)
    info = x - y
    info.mat = matrix(0, length(x), 2)
    info.mat[,1] = abs(info)
    info.mat[,2] = sign(info)
    info.mat = info.mat[order(info.mat[,1]),]
    R = rank(info.mat[,1])*info.mat[,2]
    W[b] = abs(sum(R))
  }
  pval = (sum(W < W_test) + 1)/(B + 1)
  pval
}
# the p-value is the proportion of times that we accept the null
# based on the test statistic
# if this proportion is very low we reject the null hypothesis
# this p-value is close to exact since it is a Monte Carlo test


# Part D

## Problem 3
nb.runs.sym.test = function(x, B=999) {
  x[!is.element(x,0)]
  x = x[x < 0]
  y = x[x > 0]
  W_test = qwilcox(0.5/2,length(x),length(y))
  W = numeric(B)
  for(b in 1:B) {
    x = sample(x,replace = TRUE)
    y = sample(y,replace = TRUE)
    info = x - y
    info.mat = matrix(0, length(x), 2)
    info.mat[,1] = abs(info)
    info.mat[,2] = sign(info)
    info.mat = info.mat[order(info.mat[,1]),]
    R = rank(info.mat[,1])*info.mat[,2]
    W[b] = abs(sum(R))
  }
  pval = (sum(W < W_test) + 1)/(B + 1)
  pval
}