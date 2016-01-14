## calculate beta of a stock
#Packages required
require(PerformanceAnalytics)
require(quantmod)
library(pbkrtest)
require(car)

startdate = "2014-01-01"
enddate = "2015-01-12"

#Here we get the symbols for the SP500 (GSPC), FI, and 5yr Treasuries (GS5)
getSymbols("^GSPC", src = "yahoo", from = as.Date(startdate), to = as.Date(enddate))
getSymbols("FI", src = "yahoo", from = as.Date(startdate), to = as.Date(enddate))
getSymbols("GS5", src = "FRED", from = as.Date(startdate), to = as.Date(enddate))

#Market risk R_m is the arithmetic mean of SP500 from 2009 through 2011
#Riskfree rate is arithmetic mean of 5yr treasuries
marketRisk<- mean(yearlyReturn(GSPC['2014::2015']))
riskFree <- mean(GS5['2014::2015'])

#My professor advised us to use weekly returns taken on wednesday
#so I take a subset of wednesdays and use the quantmod function
#weeklyReturn()
FI.weekly <- subset(FI,weekdays(time(FI))=='Wednesday')
FI.weekly <- weeklyReturn(FI['2014::2015'])
GSPC.weekly <- subset(GSPC,weekdays(time(GSPC))=='Wednesday')
GSPC.weekly <- weeklyReturn(GSPC['2014::2015'])

#Here I use PerformanceAnalytics functions for alpha+beta
#Then we calculate Cost of equity using our calculated figures
FI.beta <- CAPM.beta(FI.weekly,GSPC.weekly)
FI.alpha <- CAPM.alpha(FI.weekly,GSPC.weekly)
FI.expectedReturn <- riskFree + FI.beta * (marketRisk-riskFree)

#For my graph, I want to show R^2, so we get it from the
#lm object FI.reg
FI.reg<-lm(FI.weekly~GSPC.weekly)
FI.rsquared<-summary(FI.reg)$r.squared

FI.WACC = (0.3)*(0.05) + (0.7)*FI.expectedReturn

#Lastly, we graph the returns and fit line, along with info
plot(100*as.vector(GSPC.weekly),
            100*as.vector(FI.weekly), 
            smooth=FALSE, 
            main='FI vs. S&P 500 2014-2015',
            xlab='S&P500 Returns', 
            ylab='FI Returns',boxplots=FALSE)
abline(FI.reg)

text(1,-7,paste('y = ',
                 signif(FI.alpha,digits=4),
                 ' + ',
                 signif(FI.beta,digits=5),
                 'x \n R^2 = ',
                 signif(FI.rsquared,digits=6),
                 '\nn=',
                 length(as.vector(FI.weekly)),
                '\nWACC = ', FI.WACC,
                 sep=''),font=2)

