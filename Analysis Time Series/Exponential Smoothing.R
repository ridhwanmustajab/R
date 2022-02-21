gas = read.csv("D:/Tugas/Semester 5/ARW/ekspor migas.csv", sep = ";")
View(gas)

gas.rev = gas[order(nrow(gas):1),]
gas.rev

gas.ts = ts(gas$migas, start = c(2017,1), frequency = 12)
gas.ts

plot.ts(gas.ts, col = "blue", lwd = 2, type ="b", pch = 7,
        main = "Ekspor Migas")

sesb.gas = HoltWinters(gas.ts, alpha = 0.5, beta = FALSE, gamma = FALSE)
sesb.gas

sesb.gas = HoltWinters(gas.ts, alpha = NULL, beta = FALSE, gamma = FALSE)
sesb.gas

sesb.gas$fitted
?fitted
#SSE, MSE, RMSE, MAPE
sse = sesb.gas$SSE
mse = sesb.gas$SSE/NROW(sesb.gas$fitted)
rmse = sqrt(mse)
mape = mean(abs(gas.ts-sesb.gas$fitted[,1]/gas.ts), na.rm = TRUE)*100
sse
mse
rmse
mape

pred.sesb = predict(sesb.gas, 5)
pred.sesb

#Plot
plot(gas.ts, main="Natural Gas", lwd = 2, col = "blue", xlim =
       c(2017,2021), type = "o", pch = 15)
limitDate = end(gas.ts)[1]+(end(gas.ts)[2]-1)/frequency(gas.ts)
abline(v=limitDate, lty=4)
lines(sesb.gas$fitted[,1], lwd = 2, col = "red", type = "o", pch = 12)
lines(pred.sesb, col = "green", type = "o", pch = 10)

legend("bottomleft", legend = c("Data aktual", "Fitted Value","Peramalan"), 
       col = c("blue", "red", "green"), lty = 1, 
       pch = c(15, 12, 10), cex = 0.8, inset = 0.02)

#Forecast
install.packages("forecast")
library(forecast)
?ses
ses.gas = ses(gas.ts, h = 5, alpha = NULL)
ses.gas

ses.gas$model
ses.gas$fitted

mse = mean(ses.gas$residuals^2)
rmse = sqrt(mse)
mape = mean (abs(ses.gas$residuals)/gas.ts, na.rm = TRUE)*100
mse
rmse
mape

plot(ses.gas, col = "blue")
lines(ses.gas$fitted, col = "red")
legend("bottomleft", legend = c("Data aktual", "Fitted Value"), col =
         c("blue", "red"), lty = 1, cex = 0.8, inset = 0.02)

