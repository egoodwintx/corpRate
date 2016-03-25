# library(quantmod)
# library(forecast)
#
# stockdat = read.csv("data/EMR_data.csv",header = TRUE, stringsAsFactors = FALSE)
#
# stockdat = stockdat[complete.cases(stockdat),]
# names(stockdat) = c("Date", "Capex", "GrossMargin", "OpMargin", "Price", "Revenue")
# stockdat$Date = strptime(stockdat$Date, format="%Y-%m-%d %H:%M:%S")
# # ts(stockdat)
# # stockdat/lag(stockdat, -1) -1
