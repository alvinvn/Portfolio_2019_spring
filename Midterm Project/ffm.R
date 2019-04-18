#========================================
# multifactor model
#========================================
rm(list=ls())
setwd("~/")
#library(fPortfolio)
#library(fAssets)
#This data set contains monthly returns
#from January 1, 1987, to December 1, 1987, on 16 equities. There are 18
#columns. The ﬁrst column is the date and the last is the risk-free rate.

library("fEcofin")
data(berndtInvest)
class(berndtInvest)

#retdata = read.csv("berndt.csv")
t = dim(berndtInvest)[1]
retdata<-berndtInvest[,-1]
market = retdata[,10]

riskfree = retdata[,17]
market = market - riskfree

retdata1 = retdata[,c(-10, -17)]
retdata1 = as.matrix(retdata1)
n = dim(retdata1)[2]
riskfree_mtx = matrix(rep(riskfree,n), ncol=n)
retdata1<-retdata1 - riskfree_mtx
ones = rep(1,t)
X = cbind(ones,market)
b_hat = solve(t(X)%*%X)%*%t(X)%*%retdata1
E_hat = retdata1 - X%*%b_hat
diagD_hat = diag(t(E_hat)%*%E_hat)/(t-2)

#R-square
retvar = apply(retdata1,2,var) 
R_2 = 1 - diag(t(E_hat)%*%E_hat)/((t-1)*retvar)
res_std = sqrt(diagD_hat)
cov_factor = var(market)*t(b_hat)%*%b_hat + diag(diagD_hat) 
sd = sqrt(diag(cov_factor))
cor_factor = cov_factor/(sd%*%t(sd))
# sample variance and correlation matrix
cov_sample = cov(retdata1);
cor_sample = cor(retdata1);
# use factor covariance matrix to compute global minimum variance portfolio
one.vec = rep(1,15)
a = solve(cov_factor)%*%one.vec
b = t(one.vec)%*%a
mvp.w =a / as.numeric(b)
mvp.w

# use historical covariance matrix to compute global minimum variance portfolio
a1 = solve(cov_sample)%*%one.vec
b1 = t(one.vec)%*%a1
mvp.w1 =a1 / as.numeric(b1)
mvp.w1

par(mfrow=c(2,1))
barplot(mvp.w[, 1], main="MVP weights by single.index", col="blue", cex.names = 0.75, las=1)
barplot(mvp.w1[,1], main="MVP weights by historical variance", col="blue", cex.names = 0.75, las=1)
par(mfrow=c(1,1))