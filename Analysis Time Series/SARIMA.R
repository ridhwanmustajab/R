#-----------SARIMA-----------#
domestik <- read.csv("D:/Tugas/Semester 5/ARW/Modul 7-SARIMA/penumpang domestik Ngurah Rai.csv")
View(domestik)

domestik.ts=ts(domestik$Ngurah.Rai, start=c(2014,1), frequency=12)
domestik.ts
ts.plot(domestik.ts, col="blue", main="TS: Domestik Ngurah Rai", lwd=2)
summary(domestik.ts)

library(tseries)                
library(forecast)             

Acf(domestik.ts, lag.max=36)

#Jika data mengandung musiman maka, dilakukan differnsi musiman
domestik.dslog=diff(log(domestik.ts), diffrences=1, lag=12)
domestik.dslog
ts.plot(domestik.dslog, col="red", main="TS: domestik.dslog")

#Ui stasioneritas data differencing musiman
adf.test(domestik.dslog)
Acf(domestik.dslog, lag.max=37)


#Karena data belum stasioner, maka dilakukan diferencing 
#non-musiman order 1 utk menstasionerkannya
domestik.ddslog=diff(domestik.dslog, diffrences=1)
adf.test(domestik.ddslog)
Acf(domestik.ddslog, lag.max=37)

#Estimasi model
par(mfrow=c(2,1))
Pacf(domestik.ddslog, lag.max=37)
Acf(domestik.ddslog, lag.max=37)

#Untuk melihat sig. dari koefisien model
printstatarima <- function (x, digits = 4,se=TRUE,...){
  if (length(x$coef) > 0) {
    cat("\nCoefficients:\n")
    coef <- round(x$coef, digits = digits)
    if (se && nrow(x$var.coef)) {
      ses <- rep(0, length(coef))
      ses[x$mask] <- round(sqrt(diag(x$var.coef)), digits = digits)
      coef <- matrix(coef, 1, dimnames = list(NULL, names(coef)))
      coef <- rbind(coef, s.e. = ses)
      statt <- coef[1,]/ses
      pval  <- 2*pt(abs(statt), df=length(x$residuals)-1, lower.tail = FALSE)
      coef <- rbind(coef, t=round(statt,digits=digits),sign.=round(pval,digits=digits))
      coef <- t(coef)
    }
    print.default(coef, print.gap = 2)
  }
}

#pdq (1,1,1) PDQ (0,1,0)

# Fitting Model Seasonal ARIMA()
#model tidak signifikan
model1=Arima(log(domestik.ts), order=c(1,1,1), 
             seasonal=list(order=c(0,1,0),period=12), include.mean=F)
model2=Arima(log(domestik.ts), order=c(1,1,0), 
             seasonal=list(order=c(0,1,0),period=12), include.mean=F)
model3=Arima(log(domestik.ts), order=c(0,1,1), 
             seasonal=list(order=c(0,1,0),period=12), include.mean=F)

summary(model1)
summary(model2)
summary(model3)

printstatarima(model1)
printstatarima(model2)
printstatarima(model3)

#Nilai AR dianggap 0 karna dianggap setelah stasioner
#nilai meluruh secara eksponensial

#uji diagnostik
par(mfrow=c(1,1))
tsdiag(model1)
tsdiag(model2)
tsdiag(model3)


#peramalan
domestik.pred=predict(model1,n.ahead=4)
domestik.pred

#membalikkan ke data awal
domestik.pred2=exp(domestik.pred$pred)
domestik.pred2

#nilai prediksi
domestik.fitted2=exp(fitted(model1))
domestik.fitted2

#visualisasi
#grafik data asli dan data prediksi
ts.plot(domestik.ts, col="green", xlim=c(2014,2020))
lines(domestik.fitted2, col="blue")
lines(domestik.pred2, col="red")

#akurasi peramalan
mape=mean(abs(domestik.ts-(domestik.fitted2))/(domestik.ts))*100
mape
akurasi=100-mape
akurasi
