# Remove everything to start from scratch
rm(list=ls(all=TRUE))

library(quantmod)
# Basic R code for the Tactical Asset Allocation System by Mebane Faber
#
# For info on the system see here:
# http://papers.ssrn.com/sol3/papers.cfm?abstract_id=962461
#
# Copyright (c) Peter Werner 2012

#work out the log return between 2 periods
calc_ret <- function(s, idx, per) {
  front <- as.numeric(Cl(s[idx]))
  back <- as.numeric(Cl(s[(idx-per)]))  
  return(log(front/back))
}

#calculate the average return and if it is over its 10 SMA
sym_gtaa_data <- function(strsym, prev_month=FALSE) {	
  
  nm <- strsym #this symbol
  s <- get(strsym) #read from env (put there by getSymbols())
  m <- s #aggregate to monthly
  idx <- nrow(m) #get the last row id
  
  if (prev_month) { #if we want to go back 1 month
    idx <- idx # - 1
    
  }
  
  #calc 5, 20 and 50 day returns
  ret90d <- calc_ret(m, idx, 90)
  ret10d <- calc_ret(m, idx, 10)
  ret20d <- calc_ret(m, idx, 20)
  ret30d <- calc_ret(m, idx, 30)
  
  #then calc their average
  avg_ret <- as.numeric(Cl(m[idx])) - as.numeric(Cl(m[idx - 1]))



  
  #are we over the 7 day sma?
  m$SMA20 <- SMA(Cl(m), 20)
  gt_smaf <- as.logical(Cl(m[idx]) > m[idx]$SMA20)

  #are we over the 15 day sma?
  m$SMA50 <- SMA(Cl(m), 50)
  gt_smas <- as.logical(Cl(m[idx]) > m[idx]$SMA50)

  #are we over the 50 day sma?
  m$SMA200 <- SMA(Cl(m), 200)
  gt_smavs <- as.logical(Cl(m[idx]) > m[idx]$SMA200)

  hhh <- as.numeric(Hi(m[idx]))
  lll <- as.numeric(Lo(m[idx]))

  close  <- as.numeric(Cl(m[idx]))
  close1 <- as.numeric(Cl(m[idx - 1]))
  close10 <- as.numeric(Cl(m[idx - 10]))
  close30 <- as.numeric(Cl(m[idx - 30]))
  close90 <- as.numeric(Cl(m[idx - 90]))

  rng <- (close - lll)/(hhh - lll)
  roc90 <- (close - close90)/close90
  
  RET <- (ret30d + ret90d * 2) / 3

  #return a list with appropriate names
  res <- list(Sym=nm, Close=close, Prev=close1, Range=rng, Change=avg_ret,  R10d=ret10d, R30d=ret30d, R90d=ret90d, RoC=RET,  OverMA20=gt_smaf, OverMA50=gt_smas, OverMA200=gt_smavs)
  return(res)
}

prev_month <- TRUE
#our list of symbols

#syms <- c('SPY','XLE','XLV','XLI','XLU','XLP','IYZ','XLK','XLY','XLF','XLB','GLD','SLV','EFA','EEM','FXA','FXE','FXY','HYG','LQD')
#syms <- c('LQD','SHY','IWF','IWB','IWD','IWO','IWM','IWN','IWP','IWR','IWS','IYM','IYK','IYE','IYF','IYJ','IYH','IYR')
syms <- c( 'SPY','XLE','XLV','XLI','XLU','XLP','IYZ','XLK','XLY','XLF','XLB','GLD','SLV','EFA','EEM','FXA','FXE','FXY','HYG','LQD','IJT', 'IJJ', 'IJH', 'IWO', 'IWM', 'IJK', 'IJR', 'IWN', 'IWS', 'IJS', 'IWD', 'IVE', 'IWV', 'IWB', 'IVV', 'IWF', 'IVW', 'IWW') 
#syms <- c('IJT', 'IWO', 'IJJ', 'XLF', 'IWW', 'EWC', 'EPOL')

#get the data from yahoo
getSymbols(syms)
#the data frame where we will store the info
df <- data.frame()
for (s in syms) {
  row <- sym_gtaa_data(s, prev_month)
  if (nrow(df) == 0) {
    df <- data.frame(row, stringsAsFactors=F)
  } else {
    df <- rbind(df, row)
  }
}
#display the data, ordered by the average return
df[with(df, order(RoC, decreasing=T)),]