#Cluster Non Hierarki
#Peneliti sudah menentuka terlebih dahulu kelompok yg akan terbentuk
#Metode yg populer yaitu K-Means: data besar

smt_ridhwan <- read.delim("clipboard")
View(smt_ridhwan)

index <- smt_ridhwan[,c(2:7)]
rownames(index) <- smt_ridhwan$Provinsi[1:34]
head(index) #perubahan nama kolom menjadi index

#Cek outlier data tanpa membuang datanya
library(MVN)
outlier <- mvn(index, multivariateOutlierMethod = "quan", showNewData = TRUE)

#Pengecekan Multikolinearitas
#Asumsi no multiko
#Ada gejala salah satu variabel yg mengalami gejala multiko, dapat menggunakan salah satu variabel
#Misal X1 dan X3 mengalami gejala multiko, misal dapat digunakan X1 sana atau X3 saja
library(PerformanceAnalytics)
chart.Correlation(index)
##atau bisa juga pakai korelasi
kor=cor(index)
kor

#standarisasi data menggunakan Z-score normalization
data_standarisasi=scale(index)
data_standarisasi

#Menghitung jarak antara observasi
kluster <- data_standarisasi
kluster
library(factoextra)
jarak <- get_dist(kluster, method = "euclidean")
fviz_dist(jarak, gradient=list(low="#00AFBB", mid="white", high="#FC4E07"))# jarak jauh = orange 
# jarak dekat = biru
#Visualisasi

#Penentuan kluster
#centers:banyaknya kelompok cluster
#nstart:banyaknya iterasi
k3=kmeans(kluster,centers = 3,nstart = 25) #Peneliti menentukan 3 kelompok
k3

#Visualisai hasil pengelompokan (tinggi, sedang, dan rendah)
library(ggpubr)
fviz_cluster(k3, data=kluster) 
#mengelompokkan kabuten berdasarkan tingkat kemiskinan 

##Tambahan##
#Pendekatan untuk mendapatkan k optimal menggunakan metode elbow, slihoute, dan gap statistics
fviz_nbclust(kluster,kmeans, method="wss") #Metode Elbow atau within sum square
#angka yang mulai landai ada di data ke-4 atau 5

fviz_nbclust(kluster,kmeans, method="silhouette") #Metode silhouette
#7 kulster

fviz_nbclust(kluster,kmeans, method="gap_stat") #metode gap statistcs
#1 kluster

#Hasil data frame yang berisi cluster
hasil_ridhwan <- data.frame(index, k3$cluster)
hasil_ridhwan[order(hasil_ridhwan[,7]),]

#Membuat barplot untuk mengidentifikasi karakteristik dari setiap kelompok
kluster1 <-subset(hasil_ridhwan, k3.cluster==1)
kluster2 <-subset(hasil_ridhwan, k3.cluster==2)
kluster3 <-subset(hasil_ridhwan, k3.cluster==3)

kluster_1<-sapply(kluster1, mean)
kluster_2 <-sapply(kluster2, mean)
kluster_3<-sapply(kluster3, mean)
mean_total_ridhwan <-rbind(kluster_1, kluster_2, kluster_3)
mean_total_ridhwan

barplot(t(mean_total_ridhwan[,-7]), beside=T)