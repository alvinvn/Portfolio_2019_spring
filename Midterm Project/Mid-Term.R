library(fPortfolio)
library(quantmod)

# 1. Download ETF daily data from yahoo with ticker names of SPY, QQQ, EEM, IWM, EFA, TLT, IYR, GLD from 2010 to current date. 
tickers = c("SPY", "QQQ", "EEM", "IWM", "EFA", "TLT", "IYR", "GLD")
data <- new.env()
getSymbols(tickers, src = "yahoo", from = "2010-01-01", env = data, auto.assign = T)
names(data)
head(data$SPY)
multi<-data$SPY[,6]
multi<-merge(multi, data$QQQ[,6])
multi<-merge(multi, data$EEM[,6])
multi<-merge(multi, data$IWM[,6])
multi<-merge(multi, data$EFA[,6])
multi<-merge(multi, data$TLT[,6])
multi<-merge(multi, data$IYR[,6])
multi<-merge(multi, data$GLD[,6])
head(multi)
multi<-multi[,-1]
head(multi)
tail(multi)

#2. Use year 2010 data to calculate daily returns and their covariance matrix, and compute weights of minimum variance portfolio (MVP).  
multi.ret<-multi/lag(multi)-1
head(multi.ret)
multi.ret<-multi.ret[-1,]
head(multi.ret)
class(multi.ret)
lppAssets <- as.timeSeries(multi.ret)
class(lppAssets)
lppData <- portfolioData(data = lppAssets, spec = portfolioSpec())
print(lppData) 
Data <- portfolioData(lppData)
getStatistics(Data)
tgPortfolio <- tangencyPortfolio(lppAssets)
str(tgPortfolio)
print(tgPortfolio)
mvPortfolio <- minvariancePortfolio(lppAssets)
print(mvPortfolio)
globminSpec <- portfolioSpec()
shortConstraints <- c("minW[1:3]=-999", "maxW[1:3]=+999")
mvPortfolio.short <- minvariancePortfolio(lppAssets, spec = globminSpec, constraints = shortConstraints )
print(mvPortfolio.short)
weightsPie(mvPortfolio.short, radius = 0.7)
text <- "Global Minimum Risk Portfolio"
mtext(text, side = 3, line = 1.5, font = 2, cex = 0.7, adj = 0)
weightedReturnsPie(mvPortfolio.short, radius = 0.8, legend = FALSE)
covRiskBudgetsPie(mvPortfolio.short, radius = 0.9, legend = FALSE)
Spec<-portfolioSpec()
setTargetReturn(Spec)<- 0.007
efficientPortfolio.short <- efficientPortfolio(lppAssets, spec = Spec, constraints = shortConstraints )
print(efficientPortfolio.short)

#3. now use year 2010 data to calculate weekly returns and their covariance matrix, and compute weights of minimum variance portfolio (MVP).
weekly.end <- endpoints(multi, on = "weeks")
weekly <- period.apply(multi, INDEX = weekly.end, FUN = mean)
head(weekly)
weekly.ret<-weekly/lag(weekly)-1
head(weekly.ret)
weekly.ret<-weekly.ret[-1,]
head(weekly.ret)
class(weekly.ret)
lppAssets1 <- as.timeSeries(weekly.ret)
class(lppAssets1)
lppData1 <- portfolioData(data = lppAssets1, spec = portfolioSpec())
print(lppData1) 
Data2 <- portfolioData(lppData1)
getStatistics(Data1)
tgPortfolio1 <- tangencyPortfolio(lppAssets1)
str(tgPortfolio1)
print(tgPortfolio1)
mvPortfolio1 <- minvariancePortfolio(lppAssets1)
print(mvPortfolio1)
globminSpec1 <- portfolioSpec()
shortConstraints1 <- c("minW[1:3]=-999", "maxW[1:3]=+999")
mvPortfolio.short1 <- minvariancePortfolio(lppAssets1, spec = globminSpec, constraints = shortConstraints1 )
print(mvPortfolio.short1)
weightsPie(mvPortfolio.short1, radius = 0.7)
text1 <- "Global Minimum Risk Portfolio"
mtext(text1, side = 3, line = 1.5, font = 2, cex = 0.7, adj = 0)
weightedReturnsPie(mvPortfolio.short1, radius = 0.8, legend = FALSE)
covRiskBudgetsPie(mvPortfolio.short1, radius = 0.9, legend = FALSE)
Spec1<-portfolioSpec()
setTargetReturn(Spec)<- 0.007
efficientPortfolio.short1 <- efficientPortfolio(lppAssets1, spec = Spec, constraints = shortConstraints1 )
print(efficientPortfolio.short1)

#4.  By 3, now use year 2010 data to calculate monthly returns and their covariance matrix, and compute weights of minimum variance portfolio (MVP).
month.end <- endpoints(weekly, on = "months")
monthly <- period.apply(weekly, INDEX = month.end, FUN = mean)
head(monthly)
monthly.ret<-monthly/lag(monthly)-1
head(monthly.ret)
monthly.ret<-monthly.ret[-1,]
head(monthly.ret)
class(monthly.ret)
lppAssets2 <- as.timeSeries(monthly.ret)
class(lppAssets2)
lppData2 <- portfolioData(data = lppAssets2, spec = portfolioSpec())
print(lppData2) 
Data2 <- portfolioData(lppData2)
getStatistics(Data2)
tgPortfolio2 <- tangencyPortfolio(lppAssets2)
str(tgPortfolio2)
print(tgPortfolio2)
mvPortfolio2 <- minvariancePortfolio(lppAssets2)
print(mvPortfolio2)
globminSpec2 <- portfolioSpec()
shortConstraints2 <- c("minW[1:3]=-999", "maxW[1:3]=+999")
mvPortfolio.short2 <- minvariancePortfolio(lppAssets2, spec = globminSpec, constraints = shortConstraints2 )
print(mvPortfolio.short2)
weightsPie(mvPortfolio.short2, radius = 0.7)
text2 <- "Global Minimum Risk Portfolio"
mtext(text2, side = 3, line = 1.5, font = 2, cex = 0.7, adj = 0)
weightedReturnsPie(mvPortfolio.short2, radius = 0.8, legend = FALSE)
covRiskBudgetsPie(mvPortfolio.short2, radius = 0.9, legend = FALSE)
Spec2<-portfolioSpec()
setTargetReturn(Spec)<- 0.007
efficientPortfolio.short2 <- efficientPortfolio(lppAssets2, spec = Spec, constraints = shortConstraints2 )
print(efficientPortfolio.short2)

#5. Download Fama/French 3 factors models monthly data and redo question 
options(digits=5)
FF_data = read.csv("~/F-F_Research_Data_Factors.CSV")
head(FF_data)
names(FF_data)
attach(FF_data)
library("Ecdat")
library("robust")
stocks=cbind(N=Mkt.RF,SMB,HML)
fit = lm(cbind(Mkt.RF,SMB,HML)~RF)
options(digits=3)
SMB
pairs(cbind(Mkt.RF,SMB,HML))
cor(fit$residuals)
covRob(fit$residuals,cor=F)
cor.test(fit$residuals[,1], fit$residuals[,2])
cor.test(fit$residuals[,1], fit$residuals[,3])
cor.test(fit$residuals[,2], fit$residuals[,3])
pairs(fit$residuals)
n=dim(FF_data)[1]
sigF = as.matrix(var(cbind(Mkt.RF,SMB,HML)))
sigF
bbeta = as.matrix(fit$coef)
bbeta = t( bbeta[-1,])
bbeta
resig2 = apply((fit$resid)^2, 2, sum)/(n-3-1)
resig2 = diag(resig2)
cov_ff3 = bbeta%*%sigF%*%t(bbeta)+resig2
cov_ff3
cov2cor(cov_ff3)
cov_hist = cov(stocks)
cov2cor(cov_hist)
one.vec = rep(1,3)
a = solve(cov_ff3)%*%one.vec
b = t(one.vec)%*%a
mvp.w =a / as.numeric(b)
mvp.w
a1 = solve(cov_hist)%*%one.vec
b1 = t(one.vec)%*%a1
mvp.w1 =a1 / as.numeric(b1)
mvp.w1
