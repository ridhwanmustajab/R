#Metode dekomposisi dengan R

#Memanggil data mangga
mangga = read.csv("D:/Tugas/Semester 5/ARW/Modul 5-Dekomposisi/mangga search.csv", 
                     sep = ";")
View(mangga)

#Mengubah menjadi kolom time series dan melihat plot
mangga.ts = ts(mangga$Sum.of.mangga...Indonesia., start = c(2017,1), frequency = 12)
mangga.ts
plot.ts(mangga.ts)

#Melakukan dekomposisi dari data mangga
decompose.mangga = decompose(mangga.ts, type = "multiplicative")
plot(decompose.mangga)

#Masing-masing komponen hasil dekomposisi dapat diakses dengan perintah berikut
decompose.mangga$trend
decompose.mangga$seasonal
decompose.mangga$random

#Melakukan peramalan indeks musiman untuk sejumlah periode ke depan
library(forecast)
sindexf(decompose.mangga,3)

#Memanggil data deseasonalized (data tanpa komponen musiman)
seasadj(decompose.mangga)

plot(mangga.ts, col = "blue")
lines(seasadj(decompose.mangga), col = "red")
legend("topleft", c("aktual","penyesuaian musiman"), bty = "n", cex = 0.08,
       col = c("blue","red"), text.col = c("blue","red"), lty = 1)

#Melakukan peramalan dengan metode dekomposisi multiplicative
nonseasonal= HoltWinters(seasadj(decompose.mangga), alpha = NULL, beta = NULL,
                         gamma = FALSE)

#Melakukan penggabungan peramalan data
predict.mangga = predict(nonseasonal, 3)*sindexf(decompose.mangga,3)
plot(mangga.ts, xlim=c(2017,2021), ylim=c(150,400),col = "blue")
lines(predict.mangga, col = "red")
legend("topleft", c("aktual","peramalan"), bty = "n", cex = 0.8,
       col = c("blue","red"), text.col = c("blue","red"),lty = 1)
