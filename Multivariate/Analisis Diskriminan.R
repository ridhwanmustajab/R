data_ridhwan=read.delim ("clipboard")
View(data_ridhwan)

#Pengecekan data missing
summary(data_ridhwan)

#uji normalitas
library(MVN)
hz.test<- mvn(data_ridhwan[,2:10], mvnTest = "hz",multivariateOutlierMethod = "adj", showNewData = FALSE)
hz.test 

#uji homoskedastisitas
#H0 : homoskedastisitas
#H1 : heteroskedastisitas
library(biotools)
factor(data_ridhwan[,1])
hc.test <- boxM(data_ridhwan[,2:10],data_ridhwan[,1])
hc.test

#cek korelasi dan no multikolinearitas (korelasi>0.5 maka multiko)
kor<-cor(data_ridhwan[,2:10])
kor

library(candisc)
x<-as.matrix(data_ridhwan[,2:10])
x.manova<-manova(x~data_ridhwan[,1])
cc<-candisc(x.manova)
cc

#uji perbandingan rata-rata skor diskriminan
x<-as.matrix(data_ridhwan[,2:10])
x.manova<-manova(x~data_ridhwan[,1])
x.wilks<-summary(x.manova, test="Wilks")
x.wilks
cc<-candisc(x.manova)
cc

#model diskriminan
library(caret)
model.full <- lda(Y~., data = data_ridhwan)
model.full

library(klaR)
model.stepwise<-greedy.wilks(Y~., data = data_ridhwan, niveau = 0.05)
model.stepwise

model_baru<- lda(Y~Na+Fe+Ca+Ba, data = data_ridhwan)
model_baru

#ringkasan akurasi
confusion.lda<-table(predict(model_baru)$class,data_ridhwan$Y) 
confusion.lda
mean(predictions$class==data_ridhwan$Y)
(confusion.lda[1,2]+confusion.lda[2,1])/sum(confusion.lda)

confusionMatrix(predict(model_baru, data_ridhwan)$class,as.factor(data_ridhwan$Y))
