ridhwan3=read.delim ("clipboard")
View(ridhwan3)

summary(ridhwan3)

multivariat=ridhwan3[,2:4]
multivariat

#1. uji outlier 
library(MVN)
hasil = mvn(multivariat,multivariateOutlierMethod = "adj", showNewData = TRUE)
#membuang data outlier
ridhwan3_baru=hasil$newData
ridhwan3_baru

#2. uji normal multivariat
##uji normalitas data awal
#diperoleh data awal tidak normal
hasil_awal = mvn(data = multivariat, mvnTest = "hz")
hasil_awal

hasil1 = mvn(data=ridhwan3_baru, mvnTest = "hz")
hasil1

#3. uji homogenitas 
## H0 : Homogenitas (adanya kesamaan variansi)
## H1 : Heterokedastisitas 
ridhwan = ridhwan3[1:33,1:4]
ridhwan
homogen = bartlett.test(ridhwan$Useful, ridhwan$Group)
homogen
homogen1 = bartlett.test(ridhwan$Difficulty,ridhwan$Group)
homogen1
homogen2 = bartlett.test(ridhwan$Importance,ridhwan$Group)
homogen2

#4. uji manova 
##berdasarkan modul 
# H0 : tidak terdapat pengaruh dari semua perlakuan yang diterapkan terhadap respon yang diamati
# H1 : paling sedikit ada satu perlakuan yang mempengaruhi respon (YI&Y2) yang diamati
summary(manova(cbind(Useful,Difficulty,Importance)~Group,
               data=ridhwan),test = "Hotelling-Lawley")