### HW 5

## Problem 1
n = 10000
B = 999
pvalPearson = numeric(B)
pvalSpearman = numeric(B)
pvalKendall = numeric(B)
x = runif(n,-1,1)
y = x^2
for (b in 369:B){
  x = sample(x,n)
  y = sample(y,n)
  pvalPearson[b] = cor.test(x,y,alternative="two.sided",method="pearson")$p.value
  pvalSpearman[b] = cor.test(x,y,alternative="two.sided",method="spearman")$p.value
  pvalKendall[b] = cor.test(x,y,alternative="two.sided",method="kendall")$p.value
}
boxplot(pvalPearson,pvalSpearman,pvalKendall)
# The three boxplots appear almost identical, indicating that the tests reject the
# null hypothesis at about the same proportion. 

## Problem 2
library(Hmisc)
hoeff.test = function(z,B=999){
  pvalHoeff = numeric(B)
  for (b in 1:B){
    z[,1] = sample(z[,1],n)
    z[,2] = sample(z[,2],n)
    pvalHoeff[b] = hoeffd(z)$P[1,2]
  }
  return(pvalHoeff)
}
x = runif(n,-1,1)
y = x^2
z = matrix(c(x,y), n, 2)
pvalList = hoeff.test(z)
boxplot(pvalList)
boxplot(pvalPearson,pvalSpearman,pvalKendall,pvalList)
# Compared to the previous tests, the p-values of Hoeffding's test are
# generally lower, so this test is more likely to reject the null and
# thus has less power.


## Problem 3
dat = read.table(file="C:/Path/sea_ice_data.txt", header=TRUE)

# Part A
fit1 = lm(dat[[2]]~dat[[1]])
plot(dat[[1]], dat[[2]], xlab="Year",ylab = "September Arctic Sea Ice Extent (1,000,000) sq km", main = "Fitted models",pch = 16)
abline(fit1, col='red', lwd = 2)

# Part B
R0 = numeric(4)
for (d in 1:4) {
  fit2 = lm(dat[[2]] ~ poly(dat[[1]], d, raw = TRUE))
  val = predict(fit2)
  lines(dat[[1]],val, col=rainbow(4)[d], lwd = 2)
}
# Part C
require(splines)
Knots = quantile(dat[[1]], seq(0, 1, len = 6), type=1)
fit3 = lm(dat[[2]] ~ bs(dat[[1]], degree=1, knots=Knots))
val = predict(fit3)
lines(dat[[1]], val, col="green", lwd = 2)

# Part D
legend(1980,5.5,legend = c("Linear fit","Poly deg 2","Poly deg 4","Splines fit"),col=c("red",rainbow(4)[3],rainbow(4)[4],"green"),lwd=2)

# From the fitted plots, it appears that the data sets have a negative correlation
# As the years go on the extent of the ice decreases

## Problem 4
library(boot)
boot.regression = function(x,y,conf=0.95,residual=FALSE,B=999){
  #will be used to store t values
  tintercept = numeric(B)
  tslope = numeric(B)
  n = length(x) #length of predictor/response vars
  fit = lm(y~x)
  val = predict(fit) #response vars estimated by regression
  interceptEst = fit$coefficients[[1]] #estimated intercept
  slopeEst = fit$coefficients[[2]] #estimated slope
  resids = y - val #residuals
  SD = sqrt((1/(n-2))*sum(resids^2)) #unbiased estimate of SD
  sumsquaresX = sum((x-mean(x))^2) #sum of squares of X
  SEintercept = SD*((1/n)+(mean(x)^2/sumsquaresX)) #standard error of intercept
  SEslope = SD*sqrt(1/sumsquaresX) #standard error of slope
  for (b in 1:B){
    if(residual==TRUE){ #boostrap residuals
      resids1 = sample(resids,n,replace=TRUE)
      x1 = x
      y1 = val + resids1
    } else { #bootstrap observations
      x1 = sample(x,n,replace=TRUE)
      y1 = sample(y,n,replace=TRUE)
    }
    #find estimates of bootstrapped sample (as before)
    fit1 = lm(y1~x1)
    val1 = predict(fit1)
    resids1 = y1-val1
    interceptEst1 = fit1$coefficients[[1]]
    slopeEst1 = fit1$coefficients[[2]]
    SD1 = sqrt((1/(n-2))*sum(resids1^2))
    sumsquaresX1 = sum((x1-mean(x1))^2)
    SEintercept1 = SD1*((1/n)+(mean(x1)^2/sumsquaresX1))
    SEslope1 = SD1*sqrt(1/sumsquaresX1)
    #find t values
    tintercept[b] = (interceptEst1 - interceptEst)/SEintercept1
    tslope[b] = (slopeEst1 - slopeEst)/SEslope1
  }
  #confidence interval for intercept
  interceptCI = c(interceptEst - quantile(tintercept,1-((1-conf)/2))[[1]],interceptEst + quantile(tintercept,(1-conf)/2)[[1]])
  #confidence interval for slope
  slopeCI = c(slopeEst - quantile(tslope,1-((1-conf)/2))[[1]],slopeEst + quantile(tslope,(1-conf)/2)[[1]])
  #print results
  print(interceptCI)
  print(slopeCI)
}
boot.regression(x,y)
