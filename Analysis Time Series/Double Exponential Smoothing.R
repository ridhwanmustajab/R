listrik = read.csv("D:/Tugas/Semester 5/ARW/Modul 3-DES/electricity revenue.csv", sep = ";")
View(listrik)

listrik.ts = ts(listrik$Revenue, start = c(2014,1), frequency = 12)
listrik.ts

plot.ts(listrik.ts)

holtb.listrik=HoltWinters(listrik.ts,alpha = NULL, beta = NULL, gamma = FALSE)
holtb.listrik

holtb.listrik$fitted

#SSE, MSE, RMSE, MAPE
sse = holtb.listrik$SSE
mse = holtb.listrik$SSE/NROW(holtb.listrik$fitted)
rmse = sqrt(mse)
mape = mean(abs(listrik.ts-holtb.listrik$fitted[,1]/listrik.ts), na.rm = TRUE)*100
sse
mse
rmse
mape

pred.holtb = predict(holtb.listrik, 5)
pred.holtb

#Plot data aktual dan hasil peramalan
plot(listrik.ts, main="Electricity Revenue", lwd = 2, col = "blue", xlim =
       c(2014,2020), type = "o", pch = 15)
limitDate = end(listrik.ts)[1]+(end(listrik.ts)[2]-1)/frequency(listrik.ts)
abline(v=limitDate, lty=4)
lines(holtb.listrik$fitted[,1], lwd = 2, col = "red", type = "o", pch = 12)
lines(pred.holtb, col = "green", type = "o", pch = 10)

legend("topleft", legend = c("Data aktual", "Fitted Value","Peramalan"), 
       col = c("blue", "red", "green"), lty = 1, 
       pch = c(15, 12, 10), cex = 0.8, inset = 0.02)

#parameter damped
library(forecast)

holt.listrik = holt(listrik.ts, h = 5,damped = TRUE, alpha = NULL, beta = NULL,
                    phi = NULL)
holt.listrik$model

holt.listrik

holt.listrik$fitted

mse = mean(holt.listrik$residuals^2)
rmse = sqrt(mse)
mape = mean(abs(holt.listrik$residuals)/listrik.ts, na.rm = TRUE)*100
mse
rmse
mape

plot(holt.listrik, main = "electricity revenue", col = "purple", lwd = 2)
lines(holt.listrik$fitted, col = "green")
legend("bottomleft", legend = c("Data aktual", "Fitted Value"), col =
         c("purple", "green"), lty = 1, cex = 0.8, inset = 0.02)

