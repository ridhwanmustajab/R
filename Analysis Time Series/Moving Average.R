install.packages("TTR")
library(TTR)
install.packages("quantmod")
library(quantmod)

getSymbols(Symbols = "TLK", scr = "yahoo", from = "2019-09-01", 
           to = "2020-09-01")
View(TLK)

y = Ad(TLK)
sma10 = SMA(y, n = 10)
datasma10 = data.frame(y, sma10)
View(datasma10)
plot(time(y), y, col = "blue", main = "PT. Telekomunikasi Indonesia", type = "b", pch = 21, 
     cex = 0.5)
lines(time(sma10), sma10, col = "red", lwd = "2", type = "b", pch = 21,
      cex = 0.5)

wma10 = WMA(y, n = 10, wts = 1:10)
datawma10 = data.frame(y, wma10)
View(datawma10)
lines(time(wma10), wma10, col = "green", lwd = "2", type = "b", pch = 21,
      cex = 0.5)

ema10 = EMA(y, n = 10)
dataema10 = data.frame(y, sma10, ema10)
View(dataema10)
lines(time(ema10), ema10, col = "yellow", lwd = "2", type = "b", pch = 21,
      cex = 0.5)
legend("bottomleft", legend = c("AKTUAL", "SMA10","WMA10","EMA10"), col = c("blue","red","green","yellow"),
       pch = c(21,12), lty = 1, cex = 0.5, inset = 0.05, title = "Keterangan:")

mse.sma10 = mean((y-sma10)^2, na.rm = TRUE)
mse.wma10 = mean((y-wma10)^2, na.rm = TRUE)
mse.ema10 = mean((y-ema10)^2, na.rm = TRUE)
mse = cbind(mse.sma10,mse.wma10,mse.ema10)
colnames(mse)=c("SMA","WMA","EMA")
mse
