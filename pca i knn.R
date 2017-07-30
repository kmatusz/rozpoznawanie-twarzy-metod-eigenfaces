library(EBImage)

setwd("...\\wszystkie")
nazwy<-list.files()   #nazwy zdjęć do treningu





a<-readImage(nazwy[1])   #przykładowe zdjęcie
a<-channel(a, "gray")
a <- resize(a, w = 80, h = 80)
display(a)




bufor<-resize( readImage(nazwy[1]), w=80, h=80)   
l<-list(1,2)
i <- 1
while(i<101) {    #wczytanie zdjęć (w grayscale i rozmiar ok)
  bufor<-resize( readImage(nazwy[i]), w=80, h=80)  
  bufor<-channel(bufor, "gray")    #grayscale
  l[[i]] <-bufor
  i <- i + 1
  
}

macierz<-as.vector(as.array(l[[i]]))

i<-2
while(i<101){  #macierz ze wszystkich, pionowo obrazek, kolumnami
  macierz<-cbind(macierz, as.vector(as.array(l[[i]])))
  i=i+1
  
  
}

#   przykład matrix(as.vector(as.array(y)), nrow=80 ) #jest kolumnami

###średnia twarz
srednie<-apply(macierz, 1, mean)
zdj_srednie<-matrix(srednie, nrow=80, byrow=FALSE)   



###pca na macierzy zdjęć
pca2<-prcomp(t(macierz), scale. = T)  


###wariancja
rank_wariancji<-pca2$sdev^2
rank_wariancji<-rank_wariancji/sum(rank_wariancji)    
plot(cumsum(rank_wariancji))


###normalizacja
foto<-as.Image(matrix(pca2$x[,1], nrow=80, byrow=FALSE))
foto_norm<- (foto- min(foto))/(max(foto)-min(foto))
display(foto_norm)




smiec<-t(pca2$x %*% t(pca2$rotation)) * pca2$scale + pca2$center


###obcięcie wymiarów  
pca_x_obciete<-cbind(pca2$x[,1:10], matrix(rep(0, 9000), nrow=100))
smiec<-t(pca_x_obciete %*% t(pca2$rotation)) *pca2$scale+pca2$center

### wyświetlenie kompresji
smiec[,1]%>%matrix( nrow=80 )%>%as.Image()->foto
foto_norm<- (foto- min(foto))/(max(foto)-min(foto))
display(foto_norm)




#################### knn   ###############################################
##########################################################################


setwd("C:\\Users\\Alicja\\Desktop\\Nowy folder (2)\\faces94\\test")
nazwy_testowe<-list.files()
nazwy_testowe<-nazwy_testowe[1:100]

#### wczytanie testowych
bufor<-resize( readImage(nazwy_testowe[1]), w=80, h=80)   
l<-list(1,2)
i <- 1
while(i<101) {    #wczytanie zdjęć (w grayscale i rozmiar ok)
  bufor<-resize( readImage(nazwy_testowe[i]), w=80, h=80)  
  bufor<-channel(bufor, "gray")    #grayscale
  l[[i]] <-bufor
  i <- i + 1
  
}

macierz_testowe<-as.vector(as.array(l[[1]]))

i<-2
while(i<101){  #macierz ze wszystkich, pionowo obrazek, kolumnami
  macierz_testowe<-cbind(macierz_testowe, as.vector(as.array(l[[i]])))
  i=i+1
}

###wyświetlenie testowego
#macierz_testowe[,5]%>%
#  matrix( nrow=80 )%>%as.Image()%>%display()






### obcięcie nazw
library(tm)
nazwy_obciete<-removeWords(nazwy,".1.jpg")
nazwy_testowe_obciete<-removeWords(nazwy_testowe,".11.jpg")

knn_trening<-pca2$x

###projekcja testowych na pca
knn_test<-t((macierz_testowe/pca2$center-pca2$scale))%*%pca2$rotation



### właściwe knn (dla wszystkich pca)
library(class)
knn_pred<- knn(knn_trening,  knn_test,  nazwy_obciete ,k=1) 




####ewaluacja
install.packages("gmodels")
library(gmodels)


#16% dobrych ocen

###obcięcie wymiarów  
knn_trening_obciete<-pca2$x[,1:50]
knn_test_obciete<-t((macierz_testowe/pca2$center-pca2$scale))%*%pca2$rotation
knn_test_obciete<-knn_test_obciete[,1:50]


knn_pred_50<- knn(knn_trening_obciete,  knn_test_obciete,  nazwy_obciete ,k=1)

mean(nazwy_testowe_obciete==knn_pred_50)


knn_accuracy<-3
i<-2
while(i<101){
  knn_trening_obciete<-pca2$x[,1:i]
  knn_test_obciete<-t((macierz_testowe/pca2$center-pca2$scale))%*%pca2$rotation
  knn_test_obciete<-knn_test_obciete[,1:i]
  
  
  knn_pred_50<- knn(knn_trening_obciete,  knn_test_obciete,  nazwy_obciete ,k=1)
  
  
  knn_accuracy[i]<-mean(nazwy_testowe_obciete==as.vector(knn_pred_50))
  i<-i+1
  
}

plot(knn_accuracy[-1])

# najlepszy wynik przy wszystkich użytych pca to ok 16%. Dlaczego tak mało?
# najważniejszy powód to baaardzo mała liczba egzemplarzy treningowych-
# knn sobie wtedy nie radzi najlepiej. 


































##############################################################################
############################### zabawy  ######################################


eigen1_foto<-( pca$rotation[,2]-min(pca$rotation[,2]))/
  (max(pca$rotation[,2])-min(pca$rotation[,2]))

eigen1_foto<-as.Image(matrix(eigen1_foto, nrow=80, byrow=FALSE))

display(eigen1_foto)

bufor2<-macierz





pierwsze20<-bufor2[,1:20]




display(foto_norm)



plot(apply(pca$x, 2, median)[1:2000])



punkty_klas<-transpose(pca$x)
punkty_klas$nazwa<-nazwy


install.packages("class")
library(class)


c = pca$x %*% t(pca$rotation)






macierz[,1]
pca2$x%*%transpose(pca2$rotation)%>%t()%>%View



bufor2<-pca2$x[,1]/norm(pca2$x[,1], "2")





foto<-as.Image(matrix(pca2$rotation[,1], nrow=80, byrow=FALSE))*bufor2[1]+
  as.Image(matrix(pca2$rotation[,2], nrow=80, byrow=FALSE))*bufor2[2]+
  as.Image(matrix(pca2$rotation[,3], nrow=80, byrow=FALSE))*bufor2[3]+
  as.Image(matrix(pca2$rotation[,4], nrow=80, byrow=FALSE))*bufor2[4]+
  as.Image(matrix(pca2$rotation[,5], nrow=80, byrow=FALSE))*bufor2[5]+
  as.Image(matrix(pca2$rotation[,6], nrow=80, byrow=FALSE))*bufor2[6]+
  as.Image(matrix(pca2$rotation[,7], nrow=80, byrow=FALSE))*bufor2[7]


foto<-as.Image(matrix(pca2$rotation[,1], nrow=80, byrow=FALSE))*bufor2[1]

i<-2
while (i<101){
  foto<-foto+as.Image(matrix(pca2$rotation[,i], nrow=80, byrow=FALSE))*bufor2[i]
  i<-i+1
}



foto_norm<- (foto- min(foto))/(max(foto)-min(foto))
display(foto_norm)











kor<-cov(transpose(macierz))

pca<-prcomp(kor, scale. = T)



