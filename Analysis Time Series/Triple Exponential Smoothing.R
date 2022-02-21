penumpang = read.csv("D:/Tugas/Semester 5/ARW/Modul 4-TES/penumpang domestik Ngurah Rai.csv", 
                     sep = ";")
View(penumpang)

#Mengubah menjadi kolom time series
penumpang.ts = ts(penumpang$Ngurah.Rai, start = c(2014,1), frequency = 12)
penumpang.ts

plot.ts(penumpang.ts)

#Peramalan Holt winters additive
penumpang.add=HoltWinters(penumpang.ts,alpha = NULL, beta = NULL, gamma = NULL,
                          seasonal = "additive")
penumpang.add

#Peramalan Holt winters multiplicative
penumpang.multi=HoltWinters(penumpang.ts,alpha = NULL, beta = NULL, gamma = NULL,
                          seasonal = "multiplicative")
penumpang.multi

#Menghitung pengukuran kesalahan model additive
mse.add = penumpang.add$SSE/frequency(penumpang.add$fitted)
rmse.add = sqrt(mse.add)
mape.add = mean(abs(penumpang.ts-penumpang.add$fitted[,1])/penumpang.ts)*100
mse.add
rmse.add
mape.add

#Menghitung pengukuran kesalahan model multiplicative
mse.multi = penumpang.multi$SSE/frequency(penumpang.multi$fitted)
rmse.multi = sqrt(mse.multi)
mape.multi = mean(abs(penumpang.ts-penumpang.multi$fitted[,1])/penumpang.ts)*100
mse.multi
rmse.multi
mape.multi

#Menghitung prediksi
pred.penumpang = predict(penumpang.add, 6)
pred.penumpang

#Plot data aktual dan hasil peramalan
plot(penumpang.ts, main="Volume Penumpang Domestik Ngurah Rai", lwd = 2, col = "blue", xlim =
       c(2014,2020), type = "o", pch = 15)
limitDate = end(penumpang.ts)[1]+(end(penumpang.ts)[2]-1)/frequency(penumpang.ts)
abline(v=limitDate, lty=4)
lines(penumpang.add$fitted[,1], lwd = 2, col = "red", type = "o", pch = 12)
lines(pred.penumpang, col = "green", type = "o", pch = 10)

legend("topleft", legend = c("Data aktual", "Fitted Value","Peramalan"), 
       col = c("blue", "red", "green"), lty = 1, 
       pch = c(15, 12, 10), cex = 0.8, inset = 0.02)

#parameter damped
library(forecast)

#Menentukan damped, beta, alpha optimum dan memprediksi 6 periode kedepan
hw.penumpang.add = hw(penumpang.ts, h = 6, seasonal = "additive", damped = TRUE, 
                        alpha = NULL, beta = NULL, gamma = NULL, phi = NULL)
hw.penumpang.add

hw.penumpang.multi = hw(penumpang.ts, h = 6, seasonal = "multiplicative", damped = TRUE, 
                        alpha = NULL, beta = NULL, gamma = NULL, phi = NULL)
hw.penumpang.multi

#Menampilkan model
hw.penumpang.add$model
hw.penumpang.multi$model

#Menghitung pengukuran kesalahan
mse.add = mean(hw.penumpang.add$residuals^2)
rmse.add = sqrt(mse.add)
mape.add= mean(abs(hw.penumpang.add$residuals)/penumpang.ts, na.rm = TRUE)*100
mse.add
rmse.add
mape.add

#Menghitung pengukuran kesalahan
mse.multi = mean(hw.penumpang.multi$residuals^2)
rmse.multi = sqrt(mse.multi)
mape.multi = mean(abs(hw.penumpang.multi$residuals)/penumpang.ts, na.rm = TRUE)*100
mse.multi
rmse.multi
mape.multi

#Menggambarkan plot data aktual dan fitted value
autoplot(penumpang.ts, main = "Volume Penumpang Domestik Ngurah Rai") +
  autolayer(fitted(hw.penumpang.add), series = "Fitted hw add") +
  autolayer(fitted(hw.penumpang.multi), series = "Fitted hw multi")

#Menggambarkan plot data aktual dan hasil peramalan
autoplot(penumpang.ts, main = "Volume Penumpang Domestik Ngurah Rai") +
  autolayer(hw.penumpang.add, series = "Frc hw add", PI = FALSE) +
  autolayer(hw.penumpang.multi, series = "Frc hw multi", PI = FALSE)
