library(forecast)
library(tseries)

#Input Data
oil <- read.csv("D:/Tugas/Semester 5/ARW/Modul 6-ARIMA/Crude oil.csv")
View(oil)

#Mengubah data menjadi runtun waktu
oil = ts(oil$Crude.oil..WTI., start = c(2007,1), freq = 12)
oil

#Membuat Plot
ts.plot(oil, main = "TS: Crude Oil")

#Uji ADF untuk menguji stasioneritas
adf.test(oil)

#Uji Acf dan Pacf untuk menguji stasioneritas
par(mfrow = c(1,2))
Acf(oil, lag.max = 24)
Pacf(oil, lag.max = 24)

#Diferensiasi agar data stasioner
par(mfrow = c(1,1))
oil.diff1 = diff(oil, difference = 1)
ts.plot(oil.diff1, main = "TS: Crude Oil (Diferensi Orde 1)")

#Uji ADF untuk menguji stasioneritas
adf.test(oil.diff1)

#Uji Acf dan Pacf untuk menguji stasioneritas
par(mfrow = c(1,2))
Acf(oil.diff1, lag.max = 24)
Pacf(oil.diff1, lag.max = 24)

#model (p,d,q)
#p = batas keluar dari PACF
#q = batas keluar dari ACF
#d = jumlah diferensi

#estimasi parameter ARIMA(1,1,2), ARIMA(1,1,0), ARIMA(0,1,2)
model1 = Arima(oil.diff1, order = c(1,1,2))
model1

model2 = Arima(oil.diff1, order = c(1,1,0))
model2

model3 = Arima(oil.diff1, order = c(0,1,2))
model3

#Untuk melihat signifikasi dari koefisien model
printstatarima <- function (x, digits = 4,se=T,...){
  if (length(x$coef) > 0) {
    cat("\nCoefficients:\n")
    coef <- round(x$coef, digits = digits)
    if (se && nrow(x$var.coef)) {
      ses <- rep(0, length(coef))
      ses[x$mask] <- round(sqrt(diag(x$var.coef)), digits = digits)
      coef <- matrix(coef, 1, dimnames = list(NULL, names(coef)))
      coef <- rbind(coef, s.e. = ses)
      statt <- coef[1,]/ses
      pval <- 2*pt(abs(statt), df=length(x$residuals)-1, lower.tail =
                     F)
      coef <- rbind(coef, t=round(statt,digits=digits),sign.=round(pval,
                                                                   digits=digits))
      coef <- t(coef)
    }
    print.default(coef, print.gap = 2)
  }
}
printstatarima(model1)
printstatarima(model2)
printstatarima(model3)

#Uji diagnostik yaitu dilakukan uji autokorelasi
tsdiag(model2)
tsdiag(model3)

#peramalan model 3 (0,1,2)
pred.oil = forecast(model3, h = 4)
pred.oil
fitted(model3) #memunculkan nilai fitting model
par(mrow = c(1,1))
plot(pred.oil)

