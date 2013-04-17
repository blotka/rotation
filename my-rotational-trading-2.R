# Remove everything to start from scratch
rm(list=ls(all=TRUE))
# Load Systematic Investor Toolbox (SIT)
setInternet2(TRUE)
con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
source(con)
close(con)

#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod')	
#tickers = spl('LQD,SHY,IWF,IWB,IWD,IWO,IWM,IWN,IWP,IWR,IWS,IYM,IYK,IYE,IYF,IYJ,IYH,IYR')
tickers = spl('SPY,XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU,IWB,IWD,IWF,IWM,IWN,IWO,IWP,IWR,IWS,IWV,IWW,IWZ,IVW,IVE,IJK,IJH,IJJ,IJT,IJR,IJS')
#tickers = spl('SPY, DIA, XLF, QQQ, IWM, SDS, FAS, EWC, EWW, TNA, SSO, QLD, DDM, DIG, ERX, AGQ, TLT, EWW, EWC, EPOL')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '2012-08-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)		
bt.prep(data, align='remove.na', dates='2012::2013')

#*****************************************************************
# Code Strategies : weekly rebalancing
#****************************************************************** 
prices = data$prices  
n = len(tickers)  

# find week ends
week.ends = endpoints(prices, 'weeks')
week.ends = week.ends[week.ends > 0]		


# Rank on ROC 200
position.score = prices / mlag(prices, 100)	
position.score.ma = position.score		
buy.rule = T

# Select Top 2 funds daily
data$weight[] = NA
data$weight[] = ntop(position.score, 2)	
capital = 80000
data$weight[] = (capital / prices) * bt.exrem(data$weight)				
top2.d = bt.run(data, type='share', trade.summary=T, capital=capital)

# Select Top 2 funds weekly
data$weight[] = NA
data$weight[week.ends,] = ntop(position.score[week.ends,], 2)	
capital = 80000
data$weight[] = (capital / prices) * bt.exrem(data$weight)		
top2.w = bt.run(data, type='share', trade.summary=T, capital=capital)

# Plot Strategy Metrics Side by Side
plotbt.strategy.sidebyside(top2.d, top2.w, perfromance.fn = 'engineering.returns.kpi')	

######################################

#*****************************************************************
# Code Strategies : different entry/exit rank
#****************************************************************** 

# Select Top 2 funds, Keep till they are in 4/6 rank

# Select Top 4 funds weekly
data$weight[] = NA
data$weight[week.ends,] = ntop(position.score[week.ends,], 4)  
capital = 120000
data$weight[] = (capital / prices) * bt.exrem(data$weight)		
top4.w = bt.run(data, type='share', trade.summary=T, capital=capital)

data$weight[] = NA
data$weight[] = ntop.keep(position.score, 2, 4)	
capital = 120000
data$weight[] = (capital / prices) * bt.exrem(data$weight)		
top2.d.keep4 = bt.run(data, type='share', trade.summary=T, capital=capital)

data$weight[] = NA
data$weight[] = ntop.keep(position.score, 4, 6)	
capital = 120000
data$weight[] = (capital / prices) * bt.exrem(data$weight)		
top4.d.keep6 = bt.run(data, type='share', trade.summary=T, capital=capital)

# Select Top 6 funds daily
data$weight[] = NA
data$weight[] = ntop.keep(position.score, 6, 8)  
capital = 120000
data$weight[] = (capital / prices) * bt.exrem(data$weight)		
top6.d.keep8 = bt.run(data, type='share', trade.summary=T, capital=capital)

# Select Top 4 funds weekly
data$weight[] = NA
data$weight[week.ends,] = ntop(position.score[week.ends,], 2, 4)  
capital = 120000
data$weight[] = (capital / prices) * bt.exrem(data$weight)		
top2.w.keep4 = bt.run(data, type='share', trade.summary=T, capital=capital)

# Select Top 4 funds weekly
data$weight[] = NA
data$weight[week.ends,] = ntop(position.score[week.ends,], 4, 6)  
capital = 120000
data$weight[] = (capital / prices) * bt.exrem(data$weight)  	
top4.w.keep6 = bt.run(data, type='share', trade.summary=T, capital=capital)

# Select Top 4 funds weekly
data$weight[] = NA
data$weight[week.ends,] = ntop(position.score[week.ends,], 4, 8)  
capital = 120000
data$weight[] = (capital / prices) * bt.exrem(data$weight)  	
top4.w.keep8 = bt.run(data, type='share', trade.summary=T, capital=capital)

# Select Top 4 funds weekly
data$weight[] = NA
data$weight[week.ends,] = ntop(position.score[week.ends,], 6, 8)  
capital = 120000
data$weight[] = (capital / prices) * bt.exrem(data$weight)  	
top6.w.keep8 = bt.run(data, type='share', trade.summary=T, capital=capital)

# Plot Strategy Metrics Side by Side
plotbt.strategy.sidebyside(top2.w.keep4, top4.w.keep6, top4.w.keep8, top6.w.keep8, perfromance.fn = 'engineering.returns.kpi')

################################
print(top2.w.keep4$trade, order("entry.date", decreasing=T))

################################
print(top4.w.keep6$trade, order("entry.date", decreasing=T))

################################
print(top4.w.keep8$trade, order("entry.date", decreasing=T))

################################
print(top6.w.keep8$trade, order("entry.date", decreasing=T))

################################
#print(tail(top6.d.keep8$share))

################################
#print(top4.w$trade, order("entry.date", decreasing=T))