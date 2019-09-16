## Problem 1
load('earthquakes-2014.rda')

# Part A
# focus on events of magnitude at least 2
dat2 = subset.data.frame(dat, magnitude >= 2)
# table counting events per month
tab = table(dat2$month)
tab
# plot
barplot(tab, xlab="Months", ylab="Counts", main='BARPLOT', col=rainbow(12))

# Part B
# We test whether earthquakes are more prevalent in some months of 2014
# H0: the frequency of earthquakes is the same across all months (uniform distribution)
# H1: the frequency of earthquakes is not the same across all months
# set the significance level to p = 0.05

# chi-squared test against the uniform distribution
chisq.test(tab)
# the test results have a very low p-value (close to 0), so we should reject the null hypothesis
# this indicates that the frequency of earthquakes is not uniformly distributed,
# so earthquakes are more prevalent in some months and less so in others

## Problem 2

# Part A
# the data is sufficient, so we can test the data to see if male and female applicants
# have the same admission rate

# Part B
# H0: male and female applicants have the same admission rate
# H1: male and female applicants do not have the same admission rate
# set the significance level to p = 0.05

# initialize values
mtotal = 8442
myes = 3738
mno = mtotal - myes
ftotal = 4321
fyes = 1494
fno = ftotal - fyes

# estimated proportions and expectations
yesrate = (myes + fyes)/(mtotal + ftotal)
norate = (mno + fno)/(mtotal + ftotal)
expmyes = mtotal*yesrate
expmno = mtotal*norate
expfyes = ftotal*yesrate
expfno = ftotal*norate

# calculate chi-squared value
chisqr = (((myes - expmyes)^2/expmyes) + ((fyes - expfyes)^2/expfyes)) + 
  (((mno - expmno)^2/expmno) + ((fno - expfno)^2/expfno))

# p-value, df = (2-1)(2-1)
pval = 1 - pchisq(chisqr,1)
pval
# the p-value is low, so we can reject the null and accept that
# male and female applicants do not have the same admission rate

## Problem 3

# Part A
adm = UCBAdmissions # shorthand
adm = adm[,,"A"] + adm[,,"B"] + adm[,,"C"] + adm[,,"D"] + adm[,,"E"] + adm[,,"F"]
adm = as.table(adm)
summary(adm)
# the p-value is low, so we can reject the null from Part A
# and accept that male and female applicants do not have the same admission rate

# Part B
adm2 = UCBAdmissions #shorthand
mydata = data.frame(adm2[,,"A"],adm2[,,"B"],adm2[,,"C"],adm2[,,"D"],adm2[,,"E"],adm2[,,"F"])
barplot(as.matrix(mydata), ylab="Counts", main='BARPLOT', col = c("gray30","gray80"))
legend(9,800, legend = c("Admitted","Rejected"), fill = c("gray30","gray80"))
# this graphic is overall consistent with previous findings in that
# number of male applicants is generally higher, and number of admissions
# is generally lower than number of rejections
