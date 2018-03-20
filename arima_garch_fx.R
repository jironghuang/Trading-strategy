#Create an arima garch function that returns data from windowlength+1 onwards
#Best to use apply or mcmapply function to return the information. Read this for more on parallelization-->http://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/parallel.html

#Input 
# windowlength: NUmber of days used to predict
# d,<start_index of predicted value - 1> i.e if you input 0, you are predicting windowlength+1 index.... 


# Import the necessary libraries
library(quantmod)
library(lattice)
library(timeSeries)
library(rugarch)
library(parallel)

#Create a function to get the ticker?
ticker = "^STI"   #this could be changed to a command line argument
stock = get(getSymbols(ticker, from="1950-01-01"))
stock = subset(stock,!is.na(stock[,6]))

spReturns = diff(log(Cl(stock)))
spReturns[as.character(head(index(Cl(stock)),1))] = 0   #Change the first returns to NA

#Start of function to preoduce returns
arima_garch = function(d,windowLength){
  #to see the freaking progress
  # print(d)
  
  # Obtain the S&P500 rolling window for this day
  #Using 500 as data points as inputs-->May not make sense to use 1st data point as input 
  spReturnsOffset = spReturns[(1+d):(windowLength+d)]
  
  # Fit the ARIMA model
  final.aic <- Inf
  final.order <- c(0,0,0)
  for (p in 0:5) for (q in 0:5) {
    
    if ( p == 0 && q == 0) {
      next
    }
    
    arimaFit = tryCatch( arima(spReturnsOffset, order=c(p, 0, q)),
                         error=function( err ) FALSE,
                         warning=function( err ) FALSE )
    
    if( !is.logical( arimaFit ) ) {
      current.aic <- AIC(arimaFit)
      if (current.aic < final.aic) {
        final.aic <- current.aic
        final.order <- c(p, 0, q)
        final.arima <- arima(spReturnsOffset, order=final.order)
      }
    } else {
      next
    }
  }
  
  # Specify and fit the GARCH model
  spec = ugarchspec(
    variance.model=list(garchOrder=c(1,1)),
    mean.model=list(armaOrder=c(final.order[1], final.order[3]), include.mean=T),
    distribution.model="sged"
  )
  
  fit = tryCatch(
    ugarchfit(
      spec, spReturnsOffset, solver = 'hybrid'
    ), error=function(e) e, warning=function(w) w
  )
  
  # If the GARCH model does not converge, set the direction to "long" else
  # choose the correct forecast direction based on the returns prediction
  # Output the results to the screen and the forecasts vector
  if(is(fit, "warning")) {
    #This is the non-converged option. Probably follow the 
    # forecasts[d+1] = paste(index(spReturnsOffset[windowLength]), 1, sep=",")   #edit this part to remain with previous forecast
    # print(paste(index(spReturnsOffset[windowLength]), 1, sep=","))
    return("warning")
  } else {
    fore = ugarchforecast(fit, n.ahead=1)     #1 step look ahead forecast
    ind = fore@forecast$seriesFor
    # forecasts[d+1] = paste(colnames(ind), ifelse(ind[1] < 0, -1, 1), sep=",")  #at d=0, 500th point. d = 1 is 501st pt 
    # print(paste(colnames(ind), ifelse(ind[1] < 0, -1, 1), sep=","))
    return(ind[1])
  }
  
}

#forecasts for windowlength+1th row to last_row-windowlength to last row-1
#e.g. uses 1st row to 400th row for 401th row 
#arima_garch(0,400)
# wl+1th row onwards
# a = mapply(arima_garch,d = 0:nrow(GLD)-wl,windowLength = wl)   #to return the whole series. from wl+1 to nrow(stock)-wl
# a = mapply(arima_garch,d = 0:100,windowLength = wl)
wl = 400
a = mcmapply(arima_garch,d = 0:10,windowLength = wl)   #parallelizing the computation

