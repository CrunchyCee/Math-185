setwd('C:/Path/')

### Final

## Problem 1

# Part 1
B = 200
alpha = 0.05
n = c(20,50,100) #sample sizes
mu = c(0.01,0.1,1,10,100) #mu values
#declare matrices of power determined by n vs mu
test.vals.wilcox = matrix(0,length(mu),length(n))
test.vals.ks = matrix(0,length(mu),length(n))
for (i in 1:length(n)){
  for (j in 1:length(mu)) {
    pwr.wilcox = numeric(B)
    pwr.ks = numeric(B)
    for (b in 1:B){
      #sample from distributions
      x = rnorm(n[i],0,1)
      y = rnorm(n[i],mu[j],1)
      #store test results
      pwr.wilcox[b] = wilcox.test(x,y)$p.value
      pwr.ks[b] = ks.test(x,y)$p.value
    }
    # store probability of rejection
    test.vals.wilcox[j,i] = sum(pwr.wilcox <= alpha)/B
    test.vals.ks[j,i] = sum(pwr.ks <= alpha)/B
  }
}
# format matrices
colnames(test.vals.wilcox) = n
rownames(test.vals.wilcox) = mu
colnames(test.vals.ks) = n
rownames(test.vals.ks) = mu

# plot mu vs power
par(mfrow=c(1,2))
boxplot(t(test.vals.wilcox),main="Mann-Whitney U-test",xlab="mu",ylab="power")
boxplot(t(test.vals.ks),main="Kolmogorov-Smirnov test",xlab="mu",ylab="power")

#results: both tests have similar power
# (all mu > 0) power is low for mu < 1 but it is high for large mu

# Part 2
B = 200
alpha = 0.05
n = c(20,50,100) #sample sizes
sd = c(0.01,0.1,1,10,100) #standard dev values
#declare matrices of power determined by n vs sd
test.vals.wilcox2 = matrix(0,length(sd),length(n))
test.vals.ks2 = matrix(0,length(sd),length(n))
for (i in 1:length(n)){
  for (j in 1:length(sd)) {
    pwr.wilcox = numeric(B)
    pwr.ks = numeric(B)
    for (b in 1:B){
      #sample from distributions
      x = rnorm(n[i],0,1)
      y = rnorm(n[i],0,sd[j])
      #store test results
      pwr.wilcox[b] = wilcox.test(x,y)$p.value
      pwr.ks[b] = ks.test(x,y)$p.value
    }
    test.vals.wilcox2[j,i] = sum(pwr.wilcox <= alpha)/B
    test.vals.ks2[j,i] = sum(pwr.ks <= alpha)/B
  }
}
#format matrices
colnames(test.vals.wilcox2) = n
rownames(test.vals.wilcox2) = sd
colnames(test.vals.ks2) = n
rownames(test.vals.ks2) = sd

#plot sd vs power
boxplot(t(test.vals.wilcox2),main="Mann-Whitney U-test",xlab="standard deviation",ylab="power")
boxplot(t(test.vals.ks2),main="Kolmogorov-Smirnov test",xlab="standard deviation",ylab="power")

#results: power is generally low for the Mann-Whitney U-test
# for the KS test, power is generally high (except when sd = 1, as expected)

par(mfrow=c(1,1)) #reset plot layout

## Problem 2
#Ho: The groups have the same distribution.
#Ha: The groups do not have the same distribution.
#significance level = 0.05
load('smokers.rda')
dat = smokers
boot.combined.test = function(dat,B=999){
  num.groups = ncol(dat) #number of groups
  group.len = nrow(dat) #length of groups
  concat = dat[[1]] #combining data: step 1
  SST.b = numeric(B) #variable to store bootstrap treatment sum of squares
  #combining the rest of the data
  for (i in 2:num.groups){
    concat = c(concat,dat[[i]])
  }
  #mean over all groups
  grand.mean = mean(concat)
  #find the observed treatment sum of squares
  SST.treat.obs = numeric(num.groups)
  for (j in 1:num.groups){
    SST.treat.obs[j] = num.groups*(mean(dat[[j]])-grand.mean)^2
  }
  SST.obs = sum(SST.treat.obs)
  #calculate and store bootstrap sum of squares
  for (b in 1:B){
    dat.new = dat
    for(s in 1:num.groups){
      #create new groups by sampling from combined data
      dat.new[[s]] = sample(concat,group.len,replace=TRUE)
    }
    SST.treat.b = numeric(num.groups)
    for (t in 1:num.groups){
      SST.treat.b[t] = num.groups*(mean(dat.new[[t]])-grand.mean)^2
    }
    SST.b[b] = sum(SST.treat.b)
  }
  #p-value
  pval = (sum(SST.b >= SST.obs) + 1)/(B+1)
  return(pval)
}

#conduct 100 tests on smokers data
#(loop takes about 12 seconds to complete)
results = numeric(100)
for(a in 1:100){
  results[a] = boot.combined.test(dat)
}
#plot results
boxplot(results,main="p-value")

#results: the groups in the smokers data do not have the same distribution
  
## Problem 3
# x and y are two vector samples whose distributions are being compared,
# both taking values in k. They may have different sizes.
# k is the vector of factors 1:length(k)
# alpha is the significance level of the test
# the function computes the chi-squared statistic, storing the spread
# of each factor. Factors that contribute a large proportion of spread
# are recorded as significantly different and returned
multiple.categorical.test = function(x,y,k,alpha=0.05){
  k.len = length(k)
  x.len = length(x)
  y.len = length(y)
  tot = x.len + y.len #total sample size
  D = numeric(k.len) #vector to store elements of chisq test stat
  index = NaN #index of factors with a significant diff between x and y
  #put x and y into a table
  counts = matrix(NaN, 2, k.len)
  counts[1,] = table(factor(x, levels=k))
  counts[2,] = table(factor(y, levels=k))
  #calculates and stores each factor's contributed value to test statistic
  for(j in 1:k.len){
    exp.x = sum(counts[1,])*sum(counts[,j])/tot
    exp.y = sum(counts[2,])*sum(counts[,j])/tot
    D[j] = (counts[1,j] - exp.x)^2/exp.x + (counts[2,j] - exp.y)^2/exp.y
    #adds factor to index if it contributes a large proportion to the test statistic
    if(D[j] >= qchisq(1-alpha,k.len-1)/k.len || is.nan(D[j])){
      index = c(index,j)
    }
  }
  #if the result is numeric(0) there are no significantly different factors
  return(index[!is.na(index)])
}

#testing for samples with same (uniform) distribution
m = 30
n = 20
k = 1:10
x = sample(k,m,TRUE)
y = sample(k,n,TRUE)
counts = matrix(NaN, 2, k.len)
counts[1,] = table(factor(x, levels=k))
counts[2,] = table(factor(y, levels=k))
chisq.test(counts) 
multiple.categorical.test(x,y,k)
#results: we do not reject Ho, so the distributions are the same
#thus there are very few factors that are significantly different

#testing for samples with different distributions
m = 30
n = 20
k = 1:10
x = sample(k,m,TRUE)
y = round(rnorm(n,n/2))
counts = matrix(NaN, 2, k.len)
counts[1,] = table(factor(x, levels=k))
counts[2,] = table(factor(y, levels=k))
chisq.test(counts)
multiple.categorical.test(x,y,k)
#results: we reject Ho, so the distributions are different
#thus there are many factors that are significantly different

## Problem 4
require(splines)

smooth.spline.CV = function(x,y,K=10){
  #length of each group in K groups
  interval = floor(length(x)/K)
  #split x and y into sets
  x.fold = matrix(x,interval,K)
  y.fold = matrix(y,interval,K)
  #initialize for future variable storage
  x.test = matrix(NaN,interval,K-1)
  y.test = matrix(NaN,interval,K-1)
  lambda = numeric(K)
  df = numeric(K)
  for(k in 1:K){
    #split into training sets
    x.test = x.fold[,-k]
    y.test = y.fold[,-k]
    x.t = c(x.test[,1:K-1])
    y.t = c(y.test[,1:K-1])
    #find lambda and df of each training set
    lambda[k] = smooth.spline(x.t,y.t)$lambda
    df[k] = smooth.spline(x.t,y.t)$df
  }
  #return average lambda and df (tuning parameters)
  print(c("lambda","df"))
  return(c(mean(lambda),mean(df)))
}

# data set
f = function(x) (1 + 10*x - 5*x^2)*sin(10*x)
n = 1000
x = runif(n)
x = sort(x)
y = f(x) + rnorm(n)

#test
test = smooth.spline.CV(x,y)

#fitting with tuning paramater found by smooth.spline.CV
plot(x,y)
fit = smooth.spline(x,y,df=test[2])
lines(fit,col="red",lwd=2)
