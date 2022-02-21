# Analisis faktor
library(psych)
library(REdaS)
library(GPArotation)
library(MVN)
library(mvnormtest)

# input data
no2_ridhwan <- read.delim("clipboard")
no2_ridhwan

# pengecekan data misng
summary(no2_ridhwan)

# mendeteksi data outlier
library(MVN)
## cek dan mengatasi cara pertama
## menampilkan data outlier dan mengatasi tahap pertama
cek.outlier1 = mvn(no2_ridhwan, multivariateOutlierMethod = "adj",
                   showNewData = TRUE)
no2_ridhwan_data1 = cek.outlier1$newData # menghapus data outlier
# menampilkan data outlier perbaikan 1
cek.outlier2 = mvn(no2_ridhwan_data1, multivariateOutlierMethod = "adj",
                   showNewData = TRUE)
no2_ridhwan_data2 = cek.outlier2$newData # menghapus data outlier
# menampilkan data outlier perbaikan 2
cek.outlier3 = mvn(no2_ridhwan_data2, multivariateOutlierMethod = "adj",
                   showNewData = TRUE)
no2_ridhwan_data3 = cek.outlier3$newData # menghapus data outlier
# menampilkan data outlier perbaikan 3
cek.outlier4 = mvn(no2_ridhwan_data3, multivariateOutlierMethod = "adj",
                   showNewData = TRUE)
no2_ridhwan_data4 = cek.outlier4$newData # menghapus data outlier
# menampilkan data outlier perbaikan 4
cek.outlier5 = mvn(no2_ridhwan_data4, multivariateOutlierMethod = "adj",
                   showNewData = TRUE)

# korelasi
View(cor(no2_ridhwan_data4 ))

# uji kecukupan data dan kelayakan model menggunakan KMOl
library(psych)
KMO(no2_ridhwan_data4 )

# Uji Korelasi antar variabel
library(REdaS)
bart_spher(no2_ridhwan_data4 )

## Analisis faktor #
#1. Menentukan banyak faktor yang terbentuk
R <- cor(no2_ridhwan_data2)
eigen<- eigen(R)
eigen$values

#2. Proses factoring
library(GPArotation)
factors_none <- fa(no2_ridhwan_data4 , nfactors = 5, rotate="none")
factors_none


# membuat plot
plot(factors_none$values, type="b", ylab="NilaiEigenvalues", 
     xlab="Komponen yang terbentuk")

