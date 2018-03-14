#https://www.quantinsti.com/blog/an-example-of-a-trading-strategy-coded-in-r/

library(quantmod)
library("PerformanceAnalytics")

#Clear space
rm(list = ls())

#Get USO data
ticker = getSymbols('GREK')
stock = get(ticker)

chartSeries(stock, TA=NULL)
data=stock[,4]    #Closing price

#Plot MACD -->Differentiate companies with high beta and low beta-->High beta use more reactive MAs?
macd = MACD(data, nFast=12, nSlow=26,nSig=9,maType=SMA,percent = FALSE)
chartSeries(data, TA='addMACD()')

#Assign mcad to data
# data$macd = macd$macd
# data$signal = macd$signal


#Long or short
data$signal = Lag(ifelse(macd$macd < macd$signal, -1, 1))
data$macd = Lag(macd$macd)
data$fast = Lag(macd$signal)
data$limit = SMA(data[,1],50)

#Modify this to include trailing limit

#obtain returns
data$ROC = ROC(data[,1])
data$returns = data$ROC*data$signal
# returns = returns['2007-02-22/2018-03-14']
data = subset(data,!is.na(data$returns))
data = subset(data,!is.na(data$limit))

#Initialise adjusted returns
data$returns_adj = NA
data$returns_chlimit = NA

#Initialise states
data$states = NA

# > head(ROC(data))
# GSPC.Close
# 2007-01-03            NA
# 2007-01-04  0.0012275323
# 2007-01-05 -0.0061031679
# 2007-01-08  0.0022178572
# 2007-01-09 -0.0005168099
# 2007-01-10  0.0019384723


#Implement the FSA, Finite State Automata
#1 - initial
#2 - hold_1
#3 - short_1
#4 - limit_buy (after sell to cover)
#5 - limit_sell (after buy to cover)

state = 1

for(i in 1:nrow(data)){

#Transitioning to limit state
if((state == 2) & (data[i,1] < data[i,5])){
  state = 5
  data$returns_chlimit[i] = data$returns[i]
}else if((state == 3) & (data[i,1] > data[i,5])){
  state = 4
  data$returns_chlimit[i] = data$returns[i]
}

#Running condition to change state t-1 to state t

if(state == 1){
  if(data$signal[i] == 1){
    state = 2
  }else if(data$signal[i] == -1){
    state = 3
  }
  
}else if(state == 2){
  if(data$signal[i] == 1){
    state = 2
  }else if(data$signal[i] == -1){
    state = 1
  }  
  
}else if(state == 3){
  if(data$signal[i] == 1){
    state = 1
  }else if(data$signal[i] == -1){
    state = 3
  }

}else if(state == 4){
  if(data$signal[i] == 1){
    state = 2
  }else if(data$signal[i] == -1){
    #think of changing the returns. Have to absorb loss first time round
    state = 4
  }

}else if(state == 5){  
  if(data$signal[i] == 1){
    state = 5
  }else if(data$signal[i] == -1){
    #think of changing the returns. Have to absorb loss first time round
    state = 3
  }  
}

#Given state, change the returns
if(state == 1){
  data$returns_adj[i] = 0
}else if(state == 2){
  data$returns_adj[i] = data$returns[i]
}else if(state == 3){
  data$returns_adj[i] = data$returns[i]
}else if(state == 4){
  data$returns_adj[i] = 0
}else if(state == 5){
  data$returns_adj[i] = 0    
}  

  data$states[i] = state    
  
}#end loop across rows


#include limit returns
data$returns_chlimit = ifelse(is.na(data$returns_chlimit),0,data$returns_chlimit)
data$returns_adj = data$returns_adj + data$returns_chlimit
  
#Look at portfolio performance
data$portfolio = exp(cumsum(data$returns_adj))

#Strategy performance
plot(data$portfolio)
table.Drawdowns(data$returns_adj, top=10)
table.DownsideRisk(data$returns_adj)
charts.PerformanceSummary(data$returns_adj)
