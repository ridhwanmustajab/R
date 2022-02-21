#Input data pada modul
hirarki_ridhwan<-read.delim("clipboard")
#Pengecekan missing data
summary(hirarki_ridhwan)

#Uji Multikolinearitas dengan nilai VIF < 10
# H0 : Tidak terjadi multikolinieritas
# H1 : Terjadi multikolinieritas
library(car)
attach(hirarki_ridhwan)
#Multiko Kelapa Pad dengan variabel lainnya
multiko1=vif(lm(Padi~Jagung+Kedelai+Kacang.Tanah+Ubi.Jalar+Ubi.kayu+Kacang.Hijau))
#Multiko Jagung dengan variabel lainnya
multiko2=vif(lm(Jagung~Padi+Kedelai+Kacang.Tanah+Ubi.Jalar+Ubi.kayu+Kacang.Hijau))
#Multiko Kedelai dengan variabel lainnya
multiko3=vif(lm(Kedelai~Padi+Jagung+Kacang.Tanah+Ubi.Jalar+Ubi.kayu+Kacang.Hijau))
#Multiko Kacang Tanah dengan variabel lainnya
multiko4=vif(lm(Kacang.Tanah~Padi+Jagung+Kedelai+Ubi.Jalar+Ubi.kayu+Kacang.Hijau))
#Multiko Ubi Jalar dengan variabel lainnya
multiko5=vif(lm(Ubi.Jalar~Padi+Jagung+Kedelai+Kacang.Tanah+Ubi.kayu+Kacang.Hijau))
#Multiko Ubi Kayu dengan variabel lainnya
multiko6=vif(lm(Ubi.kayu~Padi+Jagung+Kedelai+Kacang.Tanah+Ubi.Jalar+Kacang.Hijau))
#Multiko Kacang Hijau dengan variabel lainnya
multiko7=vif(lm(Kacang.Hijau~Padi+Jagung+Kedelai+Kacang.Tanah+Ubi.Jalar+Ubi.kayu))
multiko1
multiko2
multiko3
multiko4
multiko5
multiko6
multiko7

#Uji Multiko dengan melihat matriks korelasi
korelasi=cor(hirarki_ridhwan[,3:9])
View(korelasi)

#Analisis Kluster Hirarki
datahirarki_ridhwan=hirarki_ridhwan[,3:9]
#Metode Average Linkage
metode_al<-hclust(dist(scale(datahirarki_ridhwan)),method="ave")
plot(metode_al)
#Metode Single Linkage
metode_sl<-hclust(dist(scale(datahirarki_ridhwan)), method = "single")
plot(metode_sl)
#Metode Complete Linkage
metode_cl=hclust(dist(scale(datahirarki_ridhwan)), method="complete")
plot(metode_cl)
#Metode Ward
metode_ward=hclust(dist(scale(datahirarki_ridhwan)), method="ward.D")
plot(metode_ward)
#Metode Centroid
metode_centro=hclust(dist(scale(datahirarki_ridhwan)), method="centroid")
plot(metode_centro)

#Menentukan banyak kelompok 
rect.hclust(metode_ward,4)

#Pengelompokan data
anggota<-cutree(metode_ward,4) 
tabel=data.frame(hirarki_ridhwan,anggota)
head(tabel)
write.csv(tabel, file="D:\\Tugas\\Semester 6\\Prak. Stat. Multivariat Terapan\\Laporan 5\\Lap5 ridhwan.csv")
