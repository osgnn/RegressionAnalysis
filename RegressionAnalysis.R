#Regression Analysis
#Admission_Predict
setwd(" ")
kabul=read.csv("Lisansustu_Kabul.csv" , header=TRUE)

row.names(kabul) = kabul[,1]
kabul = kabul[,2:dim(kabul)[2]]

summary(kabul) 
str(kabul)

numSutun=sapply(kabul, is.numeric)
data=kabul[,numSutun]
data


complete.cases(kabul)
require(mice)
md.pattern(kabul) 

plot(kabul)
boxplot(kabul)
boxplot(kabul[[3]])
hist(kabul[[2]],9)

require(ggplot2)
#install.packages("ggthemes")
require(ggthemes)
ggplot(kabul, aes(x=TOEFL_Skor)) + geom_histogram(fill = "red", bins = 15)
ggplot(kabul, aes(x=TOEFL_Skor)) + geom_density(fill = "blue")
ggplot(kabul, aes(x=TOEFL_Skor)) + geom_bar(fill = "pink")

kabul[,3] <- as.factor(kabul[,3])

ggplot(kabul, aes(x= TOEFL_Skor, y=GRE_Skor)) + geom_point(colour="Blue")
ggplot(kabul, aes(x= TOEFL_Skor, y=GRE_Skor))+ geom_line(colour="Blue")

ggplot(kabul, aes(x= TOEFL_Skor , y=GRE_Skor, as.factor(Universite_Puani))) + geom_point()
ggplot(kabul, aes(x= TOEFL_Skor , y=GRE_Skor, shape= as.factor(Universite_Puani))) + geom_point()
ggplot(kabul, aes(x= TOEFL_Skor , y=GRE_Skor, size= Kabul_Olasiligi)) + geom_point()
ggplot(kabul, aes(x=as.factor(rating), y=price)) + geom_boxplot()


#ggplot ile hepsi nümerik oldugu için, bir scatter plot yapalim, 
#x kordinati 1'inci sütun (sut1)
#y kordinati 2inci sütun (sut2)
#nokta büyüklükleri 3'üncü sütun (sut3)
#renk ise 4'üncü sütun olsun (sut4)
#önce sütun isimlerini yukaridaki yoruma göre degistirelim
# for (i in 1:ncol(kabul)){names(kabul)[i]=paste0("sut",i)}
# names(kabul)

a=ggplot(data=kabul, aes(x= GRE_Skor , y=TOEFL_Skor , color= Kabul_Olasiligi))+labs(title="Grafik", x="Marka", y="Fiyat")
a+geom_point()
a+ geom_line()
geom_boxplot(kabul)
c=ggplot(data=kabul,aes(x=TOEFL_Skor,y=Lisans_Genel_not_ortalamasi,col=Kabul_Olasiligi))
c+geom_point()

hic=lm(Kabul_Olasiligi ~ 1, data=kabul)
hic

hep=lm(Kabul_Olasiligi ~ . , data=kabul)
hep

step(hic, scope = list(upper=hep), data=newdata, direction="both")

cor(kabul)
cor(Lisans_Genel_not_ortalamasi, Kabul_Olasiligi)#0.8824126

plot(kabul)
p=ggplot(data=kabul,aes(x=Lisans_Genel_not_ortalamasi,y=Kabul_Olasiligi))
p+geom_point()+geom_smooth(method=lm)

# Root Mean Squared Error

rmse = function(err) {
  sqrt(mean(err^2))
}


#Linear Regresyon

sample1=sample.int(n=nrow(kabul),size=floor(.75*nrow(kabul)),replace=F)
sample1

train=kabul[sample1,]
X_train=train[,1:7]
y_train=train[,8]
test=kabul[-sample1,]
X_test=test[1:7]
y_test=test[,8]

b=data.frame(Kabul_Olasiligi=kabul$Kabul_Olasiligi)
b
a=subset(kabul,select=-Kabul_Olasiligi)
a

iliski=lm(Kabul_Olasiligi~GRE_Skor+TOEFL_Skor+Universite_Puani+Amac_Beyani+Tavsiye_Gucu+Lisans_Genel_not_ortalamasi+Arastirma_Deneyimi,data=kabul[sample1, ])
iliski

tahmin=predict.lm(iliski,newdata=a[-sample1,])
tahmin

error.lm = rmse(b[-sample1,]-tahmin)

tablo=data.frame("gerçek_Kabul_Olasiligi"=b[-sample1,],"tahmin_Kabul_Olasiligi"=tahmin)
tablo

# SUPPORT VECTOR MACHINES (SVM)
#install.packages("e1071")
library(e1071)
svm_model = svm(formula = Kabul_Olasiligi ~ ., data = kabul)
predictYsvm = predict(svm_model, a[-sample1,])

error.svm = rmse(b[-sample1,]-predictYsvm)
predictYsvm

summary(predictYsvm)

# Graphic

grafikData = as.data.frame(cbind(indeks=1:length(b[-sample1,])[1],gercek_olasilik=b[-sample1,], tahmin, predictYsvm))
p=ggplot(data=grafikData,aes(x=indeks,y=gercek_olasilik))+geom_point()
p
p+geom_point(aes(x=indeks,y=tahmin), colour="red")+geom_point(aes(x=indeks,y=predictYsvm), colour="blue")

grafikData = as.data.frame(cbind(Lisans_Genel_not_ortalamasi=a[-sample1,][6],gercek_olasilik=b[-sample1,], tahmin, predictYsvm))
p=ggplot(data=grafikData,aes(x=Lisans_Genel_not_ortalamasi,y=gercek_olasilik))+geom_point()
p
p+geom_line(aes(x=Lisans_Genel_not_ortalamasi,y=tahmin), colour="red")+geom_line(aes(x=Lisans_Genel_not_ortalamasi,y=predictYsvm), colour="blue")


