library(quantstrat)
library(tseries)
library(IKTrading) #had to get this through GITHUB at https://rdrr.io/github/pdrano/IKTrading/
library(lattice)
library(quantmod)
library(zoo)
library(urca)

#.blotter <- new.env()
#.strategy <- new.env()

#Step 1: initialize currency and instruments, and load historic data.(As ref. R Clenow and R 007d and "blotter and quantstrat" slides)
currency("USD")
stock(symbol, 'USD', 1)
stock(symbols, 'USD', 1)
Sys.setenv(TZ="UTC")

symbol <- c('IBM','MSFT') #IBM and Microsoft between 1996 and 2007

initDate <- '1996-01-03' #yes it's not much data but it was necessary to get the necessary correlation to proceed with cointegration
startDate <- '1996-01-05'
endDate <- '2007-07-01'

getSymbols(Symbols = symbol, from=startDate, to=endDate, periodicity="daily", adjust=TRUE, index.class="POSIXct", env=".GlobalEnv")

#when we short the spread we are selling SCHR and buying SPTI, this is because the spread is higher than it's normal and we believe it will decrease. When we go long on the spread we are buying SCHR and selling SPTI because we believe the spread is lower than normal and that it will increase.

spread <- OHLC(IBM) - OHLC(MSFT)
colnames(spread) <- c("open", "high", "low", "close")

symbols <- c("spread")

chart_Series(spread)
add_TA(EMA(Cl(spread), n =20),on=1,col="blue",lwd = 1.5) #adds a 20-day moving average of the spread


#Step 2: Initialize portfolio, account, orders, strategy. Couldn't get anything to initialize so I updated some R packages, restarted R, and magically it finally worked!!

quick <- 20 #this is the number of days in our moving average.
numShares <- 10 #dunno about this one.
initEq <- 100000   #100K

suppressWarnings(rm.strat(stratName)) #remove strategy etc if this is a re-run

portfName <- "PairStrat"
acctName <- portfName

initPortf(name = portfName, symbols = symbols, initDate = initDate, currency = 'USD')

initAcct(name=acctName, portfolios=portfName, initDate=initDate, initEq=initEq)

initOrders(portfolio = portfName, initDate = initDate)

stratName <- portfName  

#saves the strategy
strategy(name = stratName, store=TRUE)

#ls(.blotter)
#ls(.strategy)


#Indicators:
#Z-score

#function to calculate the log ratio between the 2 symbols
PairRatio <- function(x) {#returns the ratio of close prices for 2 symbols
  
  x1 <- get(x[1])
  x2 <- get(x[2])
  logratio <- log10(Cl(x1)/Cl(x2)) #ratio is based off of the close
  colnames(logratio) <- "Price.Ratio" ###****why log?
  logratio
  
}

Price.Ratio <- PairRatio(c(symbol[1],symbol[2])) #so this ends up returning, for example: log(SCHR/SPTI)

#function to calculate the moving average of the spread
MARatio <- function(x){
  
  MA <- rollapply(x,quick,FUN=mean) #rollapply is a "generic function for applying a function to rolling margins of an array".
  colnames(MA) <- "Price.Ratio.MA"
  MA
}

Price.Ratio.MA <- MARatio(Price.Ratio)

#function to calculate the standard deviation of the spread
Sd <- function(x){
  
  SD <- rollapply(x,quick,FUN=sd)
  colnames(SD) <- "Price.Ratio.SD"
  SD
}

Price.Ratio.SD <- Sd(Price.Ratio)

#calculate the Z-Score of our spread in-order to use it as an indicator
ZScore <- function(x){
  
  a <- x$Price.Ratio
  b <- x$Price.Ratio.MA
  c <- x$Price.Ratio.SD
  
  z <- (a-b)/c
  
  colnames(z) <- "Z.Score"
  z
}


#this creates the Z-score indicator
add.indicator(strategy = stratName, name = "ZScore", arguments = list(x=merge(Price.Ratio, Price.Ratio.MA, Price.Ratio.SD)) ,label="Z-Score")


alpha = 1 #this is the significane level for the ADF test. Since it's at 100%, then the p-value of the ADF test will always be below the signficance level and the strategy will not require the pair to be cointegrated.


#here we plot the Z-score
Z.Score <- ZScore(x=merge(Price.Ratio,Price.Ratio.MA,Price.Ratio.SD))
plot(main = "Z-Score Time Series", xlab = "Date" , ylab = "Z-Score",Z.Score, type = "l" )
abline(h = 2, col = 2, lwd = 3 ,lty = 2)
abline(h = -2, col = 3, lwd = 3 ,lty = 2)


#Z-Score entry and exit thresholds:

buyThresh = -2
sellThresh = -buyThresh
exitlong = -1
exitshort = 1 




#Signals:
#this signal is to enter the market when the Z-score of the pair ratio is crosses the -2 Z-score threshold
add.signal(stratName, name="sigThreshold", arguments=list(column="Z.Score", threshold=buyThresh, relationship="lt", cross=FALSE),label="longEntryZ")


#this signal says to exit the long when Z-score = -1
add.signal(stratName, name="sigThreshold",arguments=list(column="Z.Score", threshold= exitlong,
                                                         relationship="gt", cross=FALSE),label="longExit")

#this signal says to short the spread when Z-score = 2
add.signal(stratName, name="sigThreshold",arguments=list(column="Z.Score", threshold=sellThresh,
                                                         relationship="gt", cross=FALSE),label="shortEntryZ")


# this signal says to exit the short when Z-score = 1
add.signal(stratName, name="sigThreshold",arguments=list(column="Z.Score", threshold = exitshort,
                                                         relationship="lt", cross=FALSE),label="shortExit")

#adds the limits of positions
addPosLimit(portfolio = stratName, symbol = 'spread',timestamp = initDate,maxpos = 3000,longlevels = 1,minpos = -3000)


#go long on the spread
add.rule(stratName, name='ruleSignal',arguments = list(sigcol="longEntryZ",
                                                       sigval=TRUE, orderqty=3000,  osFUN = osMaxPos, replace = FALSE, ordertype='market',
                                                       orderside='long', prefer = "open"), type='enter' )

#go short on the spread
add.rule(stratName, name='ruleSignal', arguments = list(sigcol="shortEntryZ",
                                                        sigval=TRUE, orderqty=-3000,  osFUN = osMaxPos, replace = FALSE,ordertype='market',
                                                        orderside='short', prefer = "open"), type='enter')

#sell the spread (exit the long)
add.rule(stratName, name='ruleSignal', arguments = list(sigcol="longExit",
                                                        sigval=TRUE, orderqty= 'all', ordertype='market', orderside='short', prefer = "open"), type='exit')

#buy back the spread (exit the short)
add.rule(stratName, name='ruleSignal', arguments = list(sigcol="shortExit",
                                                        sigval=TRUE, orderqty= 'all' , ordertype='market', orderside='long', prefer = "open"), type='exit')


summary(get.strategy(stratName))


applyStrategy(strategy = stratName, portfolios = stratName, mktdata = spread)

tns <-getTxns(Portfolio=stratName, Symbol= symbols)

#Update portfolio, account, equity
updatePortf(Portfolio = stratName)

updateAcct(name = acctName)

updateEndEq(Account = acctName)


chart.Posn(Portfolio=portfName, TA="add_EMA(n=quick,col='red')")

tstats <- tradeStats(Portfolios=portfName)
View(t(tstats))

chart.ME(Portfolio=portfName, Symbol=symbols, type='MAE', scale='percent')





