library(TTR)

library(tidyverse)

library(formattable)

library(psych)

options(qwraps2_markup = "markdown")
daydata_ETH <- read_csv("daydata_ETH.csv")
daydata_DOGE<- read_csv("daydata_DOGE.csv")
daydata_BTC<- read_csv("daydata_BTC.csv")
daydata_XLM<- read_csv("daydata_XLM.csv")
daydata_XRP<- read_csv("daydata_XRP.csv")




multp<-left_join(daydata_BTC,daydata_DOGE,by="datetime",suffix = c(".BTC", ".DOGE")) %>% left_join(.,daydata_XLM,by="datetime",suffix = c("", ".XLM")) %>%
  left_join(.,daydata_ETH,by="datetime",suffix = c(".XLM", ".ETH")) %>% left_join(.,daydata_XRP,by="datetime") 
  

Prices<-data_frame(multp[c(5,11,17,23,29)])
colnames(Prices)[5]<-"close.XRP"
priceslog<-as.data.frame(sapply(Prices, function(x) log10(x)))


mov20<-as.data.frame(sapply(Prices, function(x) SMA(x,n=20) )) 

mov50<-as.data.frame(sapply(Prices, function(x) SMA(x,n=50) )) 

mov20log<-as.data.frame(sapply(priceslog, function(x) SMA(x,n=20) ))


#BTC 20 Day moving average
buy<-FALSE
BTCearning<-0
vectorBTCearning<-vector()
buypricevectorBTC<-vector()
sellpricevectorBTC<-vector()
USD100invBTC<-vector()
datebuyBTC<-vector()
datesellBTC<-vector()
nBTCbought<-vector()

for (i in 20:length(mov20$close.BTC)) {
  if((Prices$close.BTC[i] > mov20$close.BTC[i]) & !buy) {
    buy<- TRUE
    buyindex<-i
    buypricevectorBTC<-append(buypricevectorBTC,Prices$close.BTC[i])
    datebuyBTC<-append(datebuyBTC,daydata_BTC$datetime[i])
    if(length(buypricevectorBTC)==1) {
      ncryptobought<-100/Prices$close.BTC[i]
    } else {
      ncryptobought<-USDvalue/Prices$close.BTC[i]
    }
    nBTCbought<-append(nBTCbought,ncryptobought)
  }
  
  if(buy) {
    if(Prices$close.BTC[i]<mov20$close.BTC[i] | i==length(Prices$close.BTC)) {
      buy<- FALSE
      datesellBTC<-append(datesellBTC,daydata_BTC$datetime[i])
      sellpricevectorBTC<-append(sellpricevectorBTC,Prices$close.BTC[i])
      BTCearning<- (Prices$close.BTC[i]-Prices$close.BTC[buyindex])/Prices$close.BTC[buyindex]
      vectorBTCearning<-append(vectorBTCearning,BTCearning)
      USDvalue<-ncryptobought*Prices$close.BTC[i]
      USD100invBTC<-append(USD100invBTC,((ncryptobought*Prices$close.BTC[i])-ncryptobought*Prices$close.BTC[buyindex]) )
    }
  }
  
}

signalsBTC<-data_frame(datebuyBTC,buypricevectorBTC,nBTCbought,datesellBTC,sellpricevectorBTC,USD100invBTC,vectorBTCearning)
avgBTCearning20<-mean(vectorBTCearning)
mean(signalsBTC$USD100invBTC)
signalsBTC$accumearnings<- cumsum(signalsBTC$USD100invBTC)
describe(signalsBTC)

#BTC 50 Day moving average
buy<-FALSE
BTCearning50<-0
vectorBTCearning50<-vector()
buypricevectorBTC50<-vector()
sellpricevectorBTC50<-vector()
USD100invBTC50<-vector()
datebuyBTC50<-vector()
datesellBTC50<-vector()
nBTCbought50<-vector()

for (i in 50:length(mov50$close.BTC)) {
  if((Prices$close.BTC[i] > mov50$close.BTC[i]) & !buy) {
    buy<- TRUE
    buyindex<-i
    buypricevectorBTC50<-append(buypricevectorBTC50,Prices$close.BTC[i])
    datebuyBTC50<-append(datebuyBTC50,daydata_BTC$datetime[i])
    if(length(buypricevectorBTC50)==1) {
      ncryptobought50<-100/Prices$close.BTC[i]
    } else {
      ncryptobought50<-USDvalue/Prices$close.BTC[i]
    }
    nBTCbought50<-append(nBTCbought50,ncryptobought50)
  }
  
  if(buy) {
    if(Prices$close.BTC[i]<mov50$close.BTC[i] | i==length(Prices$close.BTC)) {
      buy<- FALSE
      datesellBTC50<-append(datesellBTC50,daydata_BTC$datetime[i])
      sellpricevectorBTC50<-append(sellpricevectorBTC50,Prices$close.BTC[i])
      BTCearning50<- (Prices$close.BTC[i]-Prices$close.BTC[buyindex])/Prices$close.BTC[buyindex]
      vectorBTCearning50<-append(vectorBTCearning50,BTCearning50)
      USDvalue<-ncryptobought*Prices$close.BTC[i]
      USD100invBTC50<-append(USD100invBTC50,((ncryptobought*Prices$close.BTC[i])-ncryptobought*Prices$close.BTC[buyindex]) )
    }
  }
  
}

signalsBTC50<-data_frame(datebuyBTC50,buypricevectorBTC50,nBTCbought50,datesellBTC50,sellpricevectorBTC50,USD100invBTC50,vectorBTCearning50)
signalsBTC50$accumearnings<- cumsum(signalsBTC50$USD100invBTC50)
avgBTCearning50<-mean(vectorBTCearning)
mean(signalsBTC50$USD100invBTC50)
t.test(vectorBTCearning50, mu=0,alternative = "greater")

describe(signalsBTC50[-1])

##ETH 20 Day Moving Average

  
  buy<-FALSE
  ETHearning<-0
  vectorETHearning<-vector()
  buypricevectorETH<-vector()
  sellpricevectorETH<-vector()
  USD100invETH<-vector()
  datebuyETH<-vector()
  datesellETH<-vector()
  nETHbought<-vector()
  
  for (i in 20:length(mov20$close.ETH)) {
    if((Prices$close.ETH[i] > mov20$close.ETH[i]) & !buy) {
      buy<- TRUE
      buyindex<-i
      buypricevectorETH<-append(buypricevectorETH,Prices$close.ETH[i])
      datebuyETH<-append(datebuyETH,daydata_ETH$datetime[i])
      if(length(buypricevectorETH)==1) {
        ncryptobought<-100/Prices$close.ETH[i]
      } else {
        ncryptobought<-USDvalue/Prices$close.ETH[i]
      }
      nETHbought<-append(nETHbought,ncryptobought)
    }
    
    if(buy) {
      if(Prices$close.ETH[i]<mov20$close.ETH[i] | i==length(Prices$close.BTC)) {
        buy<- FALSE
        datesellETH<-append(datesellETH,daydata_ETH$datetime[i])
        sellpricevectorETH<-append(sellpricevectorETH,Prices$close.ETH[i])
        ETHearning<- (Prices$close.ETH[i]-Prices$close.ETH[buyindex])/Prices$close.ETH[buyindex]
        vectorETHearning<-append(vectorETHearning,ETHearning)
        USDvalue<-ncryptobought*Prices$close.ETH[i]
        USD100invETH<-append(USD100invETH,((ncryptobought*Prices$close.ETH[i])-ncryptobought*Prices$close.ETH[buyindex]) )
      }
    }
    
  }
  
  signalsETH<-data_frame(datebuyETH,buypricevectorETH,nETHbought,datesellETH,sellpricevectorETH,USD100invETH,vectorETHearning)
  signalsETH$accumearnings<- cumsum(signalsETH$USD100invETH)
  avgETHearning20<-mean(vectorETHearning)
  mean(signalsETH$USD100invETH)
  
  t.test(vectorETHearning, mu=0,alternative = "greater")
  
  describe(signalsETH[-1])
  
  #ETH 50 Day moving average
  
  buy<-FALSE
  ETHearning50<-0
  vectorETHearning50<-vector()
  buypricevectorETH50<-vector()
  sellpricevectorETH50<-vector()
  USD100invETH50<-vector()
  datebuyETH50<-vector()
  datesellETH50<-vector()
  nETHbought50<-vector()
  
  for (i in 50:length(mov50$close.ETH)) {
    if((Prices$close.ETH[i] > mov50$close.ETH[i]) & !buy) {
      buy<- TRUE
      buyindex<-i
      buypricevectorETH50<-append(buypricevectorETH50,Prices$close.ETH[i])
      datebuyETH50<-append(datebuyETH50,daydata_ETH$datetime[i])
      if(length(buypricevectorETH50)==1) {
        ncryptobought50<-100/Prices$close.ETH[i]
      } else {
        ncryptobought50<-USDvalue/Prices$close.ETH[i]
      }
      nETHbought50<-append(nETHbought50,ncryptobought50)
    }
    
    if(buy) {
      if(Prices$close.ETH[i]<mov50$close.ETH[i] | i==length(Prices$close.ETH)) {
        buy<- FALSE
        datesellETH50<-append(datesellETH50,daydata_ETH$datetime[i])
        sellpricevectorETH50<-append(sellpricevectorETH50,Prices$close.ETH[i])
        ETHearning50<- (Prices$close.ETH[i]-Prices$close.ETH[buyindex])/Prices$close.ETH[buyindex]
        vectorETHearning50<-append(vectorETHearning50,ETHearning50)
        USDvalue<-ncryptobought*Prices$close.ETH[i]
        USD100invETH50<-append(USD100invETH50,((ncryptobought*Prices$close.ETH[i])-ncryptobought*Prices$close.ETH[buyindex]) )
      }
    }
    
  }
  
  signalsETH50<-data_frame(datebuyETH50,buypricevectorETH50,nETHbought50,datesellETH50,sellpricevectorETH50,USD100invETH50,vectorETHearning50)
  signalsETH50$accumearnings<- cumsum(signalsETH50$USD100invETH50)
  avgETHearning50<-mean(vectorETHearning)
  mean(signalsETH50$USD100invETH50)
  t.test(vectorETHearning50, mu=0,alternative = "greater")
  
  describe(round(signalsETH50[-1],3))
  ?round
  #DOGE
  
  #moving 20
  
  buy<-FALSE
  DOGEearning<-0
  vectorDOGEearning<-vector()
  buypricevectorDOGE<-vector()
  sellpricevectorDOGE<-vector()
  USD100invDOGE<-vector()
  datebuyDOGE<-vector()
  datesellDOGE<-vector()
  nDOGEbought<-vector()
  
  for (i in 20:length(mov20$close.DOGE)) {
    if((Prices$close.DOGE[i] > mov20$close.DOGE[i]) & !buy) {
      buy<- TRUE
      buyindex<-i
      buypricevectorDOGE<-append(buypricevectorDOGE,Prices$close.DOGE[i])
      datebuyDOGE<-append(datebuyDOGE,daydata_DOGE$datetime[i])
      if(length(buypricevectorDOGE)==1) {
        ncryptobought<-100/Prices$close.DOGE[i]
      } else {
        ncryptobought<-USDvalue/Prices$close.DOGE[i]
      }
      nDOGEbought<-append(nDOGEbought,ncryptobought)
    }
    
    if(buy) {
      if(Prices$close.DOGE[i]<mov20$close.DOGE[i] | i==length(Prices$close.BTC)) {
        buy<- FALSE
        datesellDOGE<-append(datesellDOGE,daydata_DOGE$datetime[i])
        sellpricevectorDOGE<-append(sellpricevectorDOGE,Prices$close.DOGE[i])
        DOGEearning<- (Prices$close.DOGE[i]-Prices$close.DOGE[buyindex])/Prices$close.DOGE[buyindex]
        vectorDOGEearning<-append(vectorDOGEearning,DOGEearning)
        USDvalue<-ncryptobought*Prices$close.DOGE[i]
        USD100invDOGE<-append(USD100invDOGE,((ncryptobought*Prices$close.DOGE[i])-ncryptobought*Prices$close.DOGE[buyindex]) )
      }
    }
    
  }
  
  signalsDOGE<-data_frame(datebuyDOGE,buypricevectorDOGE,nDOGEbought,datesellDOGE,sellpricevectorDOGE,USD100invDOGE,vectorDOGEearning)
  signalsDOGE$accumearnings<- cumsum(signalsDOGE$USD100invDOGE)
  avgDOGEearning20<-mean(vectorDOGEearning)
  mean(signalsDOGE$USD100invDOGE)
  
  t.test(vectorDOGEearning, mu=0,alternative = "greater")

  
  #Doge 50
  buy<-FALSE
  DOGEearning50<-0
  vectorDOGEearning50<-vector()
  buypricevectorDOGE50<-vector()
  sellpricevectorDOGE50<-vector()
  USD100invDOGE50<-vector()
  datebuyDOGE50<-vector()
  datesellDOGE50<-vector()
  nDOGEbought50<-vector()
  
  for (i in 50:length(mov50$close.DOGE)) {
    if((Prices$close.DOGE[i] > mov50$close.DOGE[i]) & !buy) {
      buy<- TRUE
      buyindex<-i
      buypricevectorDOGE50<-append(buypricevectorDOGE50,Prices$close.DOGE[i])
      datebuyDOGE50<-append(datebuyDOGE50,daydata_DOGE$datetime[i])
      if(length(buypricevectorDOGE50)==1) {
        ncryptobought50<-100/Prices$close.DOGE[i]
      } else {
        ncryptobought50<-USDvalue/Prices$close.DOGE[i]
      }
      nDOGEbought50<-append(nDOGEbought50,ncryptobought50)
    }
    
    if(buy) {
      if(Prices$close.DOGE[i]<mov50$close.DOGE[i] | i==length(Prices$close.DOGE)) {
        buy<- FALSE
        datesellDOGE50<-append(datesellDOGE50,daydata_DOGE$datetime[i])
        sellpricevectorDOGE50<-append(sellpricevectorDOGE50,Prices$close.DOGE[i])
        DOGEearning50<- (Prices$close.DOGE[i]-Prices$close.DOGE[buyindex])/Prices$close.DOGE[buyindex]
        vectorDOGEearning50<-append(vectorDOGEearning50,DOGEearning50)
        USDvalue<-ncryptobought*Prices$close.DOGE[i]
        USD100invDOGE50<-append(USD100invDOGE50,((ncryptobought*Prices$close.DOGE[i])-ncryptobought*Prices$close.DOGE[buyindex]) )
      }
    }
    
  }
  
  signalsDOGE50<-data_frame(datebuyDOGE50,buypricevectorDOGE50,nDOGEbought50,datesellDOGE50,sellpricevectorDOGE50,USD100invDOGE50,vectorDOGEearning50)
  signalsDOGE50$accumearnings<- cumsum(signalsDOGE50$USD100invDOGE50)
  avgDOGEearning50<-mean(vectorDOGEearning)
  mean(signalsDOGE50$USD100invDOGE50)
  t.test(vectorDOGEearning50, mu=0,alternative = "greater")
  
  
  
  buy<-FALSE
  XRPearning<-0
  vectorXRPearning<-vector()
  buypricevectorXRP<-vector()
  sellpricevectorXRP<-vector()
  USD100invXRP<-vector()
  datebuyXRP<-vector()
  datesellXRP<-vector()
  nXRPbought<-vector()
  
  for (i in 20:length(mov20$close.XRP)) {
    if((Prices$close.XRP[i] > mov20$close.XRP[i]) & !buy) {
      buy<- TRUE
      buyindex<-i
      buypricevectorXRP<-append(buypricevectorXRP,Prices$close.XRP[i])
      datebuyXRP<-append(datebuyXRP,daydata_XRP$datetime[i])
      if(length(buypricevectorXRP)==1) {
        ncryptobought<-100/Prices$close.XRP[i]
      } else {
        ncryptobought<-USDvalue/Prices$close.XRP[i]
      }
      nXRPbought<-append(nXRPbought,ncryptobought)
    }
    
    if(buy) {
      if(Prices$close.XRP[i]<mov20$close.XRP[i] | i==length(Prices$close.BTC)) {
        buy<- FALSE
        datesellXRP<-append(datesellXRP,daydata_XRP$datetime[i])
        sellpricevectorXRP<-append(sellpricevectorXRP,Prices$close.XRP[i])
        XRPearning<- (Prices$close.XRP[i]-Prices$close.XRP[buyindex])/Prices$close.XRP[buyindex]
        vectorXRPearning<-append(vectorXRPearning,XRPearning)
        USDvalue<-ncryptobought*Prices$close.XRP[i]
        USD100invXRP<-append(USD100invXRP,((ncryptobought*Prices$close.XRP[i])-ncryptobought*Prices$close.XRP[buyindex]) )
      }
    }
    
  }
  
  signalsXRP<-data_frame(datebuyXRP,buypricevectorXRP,nXRPbought,datesellXRP,sellpricevectorXRP,USD100invXRP,vectorXRPearning)
  signalsXRP$accumearnings<- cumsum(signalsXRP$USD100invXRP)
  avgXRPearning20<-mean(vectorXRPearning)
  mean(signalsXRP$USD100invXRP)
  
  t.test(vectorXRPearning, mu=0,alternative = "greater")
 
  buy<-FALSE
  XRPearning50<-0
  vectorXRPearning50<-vector()
  buypricevectorXRP50<-vector()
  sellpricevectorXRP50<-vector()
  USD100invXRP50<-vector()
  datebuyXRP50<-vector()
  datesellXRP50<-vector()
  nXRPbought50<-vector()
  
  for (i in 50:length(mov50$close.XRP)) {
    if((Prices$close.XRP[i] > mov50$close.XRP[i]) & !buy) {
      buy<- TRUE
      buyindex<-i
      buypricevectorXRP50<-append(buypricevectorXRP50,Prices$close.XRP[i])
      datebuyXRP50<-append(datebuyXRP50,daydata_XRP$datetime[i])
      if(length(buypricevectorXRP50)==1) {
        ncryptobought50<-100/Prices$close.XRP[i]
      } else {
        ncryptobought50<-USDvalue/Prices$close.XRP[i]
      }
      nXRPbought50<-append(nXRPbought50,ncryptobought50)
    }
    
    if(buy) {
      if(Prices$close.XRP[i]<mov50$close.XRP[i] | i==length(Prices$close.XRP)) {
        buy<- FALSE
        datesellXRP50<-append(datesellXRP50,daydata_XRP$datetime[i])
        sellpricevectorXRP50<-append(sellpricevectorXRP50,Prices$close.XRP[i])
        XRPearning50<- (Prices$close.XRP[i]-Prices$close.XRP[buyindex])/Prices$close.XRP[buyindex]
        vectorXRPearning50<-append(vectorXRPearning50,XRPearning50)
        USDvalue<-ncryptobought*Prices$close.XRP[i]
        USD100invXRP50<-append(USD100invXRP50,((ncryptobought*Prices$close.XRP[i])-ncryptobought*Prices$close.XRP[buyindex]) )
      }
    }
    
  }
  
  signalsXRP50<-data_frame(datebuyXRP50,buypricevectorXRP50,nXRPbought50,datesellXRP50,sellpricevectorXRP50,USD100invXRP50,vectorXRPearning50)
  signalsXRP50$accumearnings<- cumsum(signalsXRP50$USD100invXRP50)
  avgXRPearning50<-mean(vectorXRPearning)
  mean(signalsXRP50$USD100invXRP50)
  t.test(vectorXRPearning50, mu=0,alternative = "greater")
  
  
  
  
      
