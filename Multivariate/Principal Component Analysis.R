# Input data
prak8_ridhwan = read.delim("clipboard")
View(prak8_ridhwan)

# mengecek missing data
summary(prak8_ridhwan)

# Uji Kecukupan Data dan Uji Kelayakan Model
library(psych)
KMO(prak8_ridhwan)

# data yang layak lanjut PCA = nilai MSA di atas 0,5 saja
wine_data<-prak8_ridhwan[,-c(3)]
head(wine_data)

# Uji korelasi antar variabel (MULTIKOLENIERITAS)
# H0 : Tidak terdapat korelasi dari semua variabel
# H1 : Paling sedikit terdapat satu korelasi antar variabel
library(REdaS)
bart_spher(wine_data)

# Analisis Komponen Utama = Yh > 1
R <- cor(wine_data)
eigen<- eigen(R)
eigen$values

# pembentukan komponen utama
pcadata<- princomp(wine_data, cor=TRUE, score=TRUE)
summary(pcadata)
loadings(pcadata)

# plot komponen yang terbentuk
plot(pcadata, type="lines")

# korelasi antar komponen utama
View(cor(pcadata$scores))
pcadata$scores

# korelasi antar komponen utama
library(PerformanceAnalytics)
chart.Correlation(pcadata$score)

