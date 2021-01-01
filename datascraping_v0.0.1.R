#daily scraping financial data
#set to run automatically
#Data source:


#--------------------------------------------------------------------------#
#----------------------Initial Setup------------------------------------
if (!require(quantmod)) install.packages('quantmod')
if (!require(BatchGetSymbols)) install.packages('BatchGetSymbols')
library(quantmod)
library(BatchGetSymbols)
library(readr)
library(dplyr)
library(tidyr)
library(kableExtra)
library(ggplot2)


#functions
##SMA, 
mySMA <- function (price,n){
  sma <- c()
  sma[1:(n-1)] <- NA
  for (i in n:length(price)){
    sma[i]<-mean(price[(i-n+1):i])
  }
  sma <- reclass(sma,price)
  return(sma)
}

##EMA
myEMA <- function (price,n){
  ema <- c()
  ema[1:(n-1)] <- NA
  ema[n]<- mean(price[1:n])
  beta <- 2/(n+1)
  for (i in (n+1):length(price)){
    ema[i]<-beta * price[i] + 
      (1-beta) * ema[i-1]
  }
  ema <- reclass(ema,price)
  return(ema)
}

myDayminusN <- function (price,n){
  dayMinusN <- c()
  dayMinusN[1:n] <- NA
  for (i in (n+1):length(price)){
    dayMinusN[i]<-price[i-n]
  }
  return(dayMinusN)
}

#set the time window for the data
first.date <- Sys.Date()-365-120
#get the last working day
last.date <- Sys.Date()
daysToKeep <- 200

#---------------------------------------------------------------------
#---------------- SP500 Stocks----------------------------------------
#---------------------------------------------------------------------
#cleaned data downloads data/raw

#In order to improve the efficiency in data retrieval, we will get one 

#get the SP500 TICKERS
df.SP500 <- GetSP500Stocks()
tickers <- df.SP500$Tickers

#get the stock info of SP500 for the specified period
l2.out <- BatchGetSymbols(tickers = tickers,
                          first.date = first.date,
                          last.date = last.date)
#check data download status
print(l2.out$df.control)
#check tickerz
print(l2.out$df.tickers)

SP500marketBreadthRaw<-df.SP500 %>%right_join(l2.out$df.tickers,by=c("Tickers"="ticker"))

#remove duplicated dates (the last date)
SP500marketBreadthRaw<-SP500marketBreadthRaw %>%
  group_by(Tickers,ref.date)%>%
  filter(row_number(price.close) == 1)

#A few stocks were missed because of the ticker name has ".".

#output the file.


#Processing the dataset for Breadth
SP500marketBreadthRaw <- SP500marketBreadthRaw%>%group_by(Tickers)%>% 
  mutate(SMA20 = SMA(price.close,20))%>%
  mutate(SMA60 = SMA(price.close,60))%>%
  mutate(SMA120 = SMA(price.close, 120))%>%
  mutate(EMA20 = EMA(price.close, 20))%>%
  mutate(EMA60 = EMA(price.close,60))%>%
  mutate(EMA120 = EMA(price.close,120))%>%
  mutate(AboveSMA20 = ifelse(price.close>SMA20,1,0))%>%
  mutate(AboveSMA60 = ifelse(price.close>SMA60,1,0))%>%
  mutate(AboveSMA120 = ifelse(price.close>SMA120,1,0))%>%
  mutate(AboveEMA20 = ifelse(price.close>EMA20,1,0))%>%
  mutate(AboveEMA60 = ifelse(price.close>EMA60,1,0))%>%
  mutate(AboveEMA120 = ifelse(price.close>EMA120,1,0))%>%
  mutate(DMinus1 = myDayminusN(price.close,1))%>%
  mutate(Dminus5 = myDayminusN(price.close,5))%>%
  mutate(Dminus20 = myDayminusN(price.close,20))%>%
  mutate(Dminus60 = myDayminusN(price.close,60))%>%
  mutate(Dminus120 = myDayminusN(price.close,120))%>%
  mutate(Rel1D = round((price.close-DMinus1)/DMinus1*100,2))%>%
  mutate(Rel5D = round((price.close-Dminus5)/Dminus5*100,2))%>%
  mutate(Rel20D = round((price.close-Dminus20)/Dminus20*100,2))%>%
  mutate(C_Rel_EMA20 = round((price.close-EMA20)/EMA20*100,2))%>%
  mutate(EMA20_Rel_EMA60 = round((EMA20 - EMA60)/EMA60*100,2))%>%
  mutate(EMA60_Rel_EMA120 = round((EMA60 - EMA120)/EMA120*100,2))%>%
  filter(ref.date>(Sys.Date()-daysToKeep))


file_end <- paste0(Sys.Date(),".csv")
raw_folder <-"./data/raw/"
rawfilePath <-paste0(raw_folder,paste0("SP500marketBreadthRaw",file_end))
write.csv(SP500marketBreadthRaw,rawfilePath)


#processing the raw data into tables for visuals
SP500marketBreadthScoresRaw <-SP500marketBreadthRaw %>%filter(ref.date>(Sys.Date()-daysToKeep))%>%
  group_by(GICS.Sector,ref.date)%>%summarize(N=n(),n=sum(AboveSMA20),Percent = round(n/N*100))%>%
  select(ref.date,GICS.Sector,Percent) 

SP500marketBreadthScores<-SP500marketBreadthScoresRaw%>%spread(GICS.Sector,Percent)%>%
  mutate(Breadth = rowSums(.[2:12]))%>% arrange(desc(ref.date))
#write the process file for visualization
processed_folder <-"./data/processed/"
processfilePath <-paste0(processed_folder,paste0("SP500marketBreadthScores",file_end))
write.csv(SP500marketBreadthScores,processfilePath)

#----------------------------------------------------------
#---------------Market Dashboard Data----------------------
#---------------Load the tickers from csv file ------------
#----------------------------------------------------------

MarketDashboardTickers <- read_csv("./data/MarketDashboardTickers.csv")
# set tickers
tickers <- MarketDashboardTickers$Symbols

l.out <- BatchGetSymbols(tickers = tickers, 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = "daily") # cache in tempdir()


MarketDashboardRaw<-MarketDashboardTickers %>%right_join(l.out$df.tickers,by=c("Symbols"="ticker"))%>%
  select(-"ret.adjusted.prices",-"ret.closing.prices")%>%rename(Tickers = Symbols)

#clean for duplicated ref.dates
MarketDashboardRaw <-MarketDashboardRaw %>%
  group_by(Tickers,ref.date)%>%
  filter(row_number(price.close) == 1)


#add more indicators,only save the last 120 days
MarketDashboardProcessed<-MarketDashboardRaw%>%group_by(Tickers)%>% 
  mutate(SMA20 = SMA(price.close,20))%>%
  mutate(SMA60 = SMA(price.close,60))%>%
  mutate(SMA120 = SMA(price.close, 120))%>%
  mutate(EMA20 = EMA(price.close, 20))%>%
  mutate(EMA60 = EMA(price.close,60))%>%
  mutate(EMA120 = EMA(price.close,120))%>%
  mutate(AboveSMA20 = ifelse(price.close>SMA20,1,0))%>%
  mutate(AboveSMA60 = ifelse(price.close>SMA60,1,0))%>%
  mutate(AboveSMA120 = ifelse(price.close>SMA120,1,0))%>%
  mutate(AboveEMA20 = ifelse(price.close>EMA20,1,0))%>%
  mutate(AboveEMA60 = ifelse(price.close>EMA60,1,0))%>%
  mutate(AboveEMA120 = ifelse(price.close>EMA120,1,0))%>%
  mutate(DMinus1 = myDayminusN(price.close,1))%>%
  mutate(Dminus5 = myDayminusN(price.close,5))%>%
  mutate(Dminus20 = myDayminusN(price.close,20))%>%
  mutate(Dminus60 = myDayminusN(price.close,60))%>%
  mutate(Dminus120 = myDayminusN(price.close,120))%>%
  mutate(Rel1D = round((price.close-DMinus1)/DMinus1*100,2))%>%
  mutate(Rel5D = round((price.close-Dminus5)/Dminus5*100,2))%>%
  mutate(Rel20D = round((price.close-Dminus20)/Dminus20*100,2))%>%
  mutate(C_Rel_EMA20 = round((price.close-EMA20)/EMA20*100,2))%>%
  mutate(EMA20_Rel_EMA60 = round((EMA20 - EMA60)/EMA60*100,2))%>%
  mutate(EMA60_Rel_EMA120 = round((EMA60 - EMA120)/EMA120*100,2))%>%
  filter(ref.date>(Sys.Date()-daysToKeep))%>%
  left_join(SP500marketBreadthScoresRaw,by=c("Sector"="GICS.Sector","ref.date"="ref.date"))

#write the process file to folder
processed_folder <-"./data/processed/"
processdashfilePath <-paste0(processed_folder,paste0("MarketDashboardProcessed",file_end))
write.csv(MarketDashboardProcessed,processdashfilePath)


#----------------------------------------------------------
#------------------My Holding List-------------------------
#----------------------------------------------------------
#----------------------------------------------------------
myListTickers <- read_csv("./data/MyList.csv")
# set tickers
mytickers <- myListTickers$Symbols

l3.out <- BatchGetSymbols(tickers = mytickers, 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = "daily") # cache in tempdir()


MyListRaw<-myListTickers %>%right_join(l3.out$df.tickers,by=c("Symbols"="ticker"))%>%
  select(-"ret.adjusted.prices",-"ret.closing.prices")%>%rename(Tickers = Symbols)

#clean for duplicated ref.dates
MyListRaw <-MyListRaw %>%
  group_by(Tickers,ref.date)%>%
  filter(row_number(price.close) == 1)


#add more indicators,only save the last 120 days
MyListProcessed<-MyListRaw%>%group_by(Tickers)%>% 
  mutate(SMA20 = SMA(price.close,20))%>%
  mutate(SMA60 = SMA(price.close,60))%>%
  mutate(SMA120 = SMA(price.close, 120))%>%
  mutate(EMA20 = EMA(price.close, 20))%>%
  mutate(EMA60 = EMA(price.close,60))%>%
  mutate(EMA120 = EMA(price.close,120))%>%
  mutate(AboveSMA20 = ifelse(price.close>SMA20,1,0))%>%
  mutate(AboveSMA60 = ifelse(price.close>SMA60,1,0))%>%
  mutate(AboveSMA120 = ifelse(price.close>SMA120,1,0))%>%
  mutate(AboveEMA20 = ifelse(price.close>EMA20,1,0))%>%
  mutate(AboveEMA60 = ifelse(price.close>EMA60,1,0))%>%
  mutate(AboveEMA120 = ifelse(price.close>EMA120,1,0))%>%
  mutate(DMinus1 = myDayminusN(price.close,1))%>%
  mutate(Dminus5 = myDayminusN(price.close,5))%>%
  mutate(Dminus20 = myDayminusN(price.close,20))%>%
  mutate(Dminus60 = myDayminusN(price.close,60))%>%
  mutate(Dminus120 = myDayminusN(price.close,120))%>%
  mutate(Rel1D = round((price.close-DMinus1)/DMinus1*100,2))%>%
  mutate(Rel5D = round((price.close-Dminus5)/Dminus5*100,2))%>%
  mutate(Rel20D = round((price.close-Dminus20)/Dminus20*100,2))%>%
  mutate(C_Rel_EMA20 = round((price.close-EMA20)/EMA20*100,2))%>%
  mutate(EMA20_Rel_EMA60 = round((EMA20 - EMA60)/EMA60*100,2))%>%
  mutate(EMA60_Rel_EMA120 = round((EMA60 - EMA120)/EMA120*100,2))%>%
  filter(ref.date>(Sys.Date()-daysToKeep))

#write the process file to folder
processed_folder <-"./data/processed/"
processdashfilePath <-paste0(processed_folder,paste0("MyListProcessed",file_end))
write.csv(MyListProcessed,processdashfilePath)


#----------------------------------------------------------
#------------------My Watch List-------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# BatchGetSymbols(tickers = c("YMAB"), 
#                 first.date = "2020-12-30",
#                 last.date = last.date, 
#                 freq.data = "daily")$df.ticker # cache in tempdir()
# 
# # chart_Series(YMAB)

getQuote("AAPL")
getQuote("QQQQ;SPY;^VXN",what=yahooQF(c("Bid","Ask")))
standardQuote()

