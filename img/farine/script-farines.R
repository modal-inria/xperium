library(fda)
# library(mclust)
# source('fun_log_dens.r')
# source('../funHDDC/R functions/EM.r')
# source('../funHDDC/R functions/fun_hddc.r')

#############################################################################################################
#######                                            DATA                                            ##########
#############################################################################################################
#  individus en colonne
# --------- farines en 3 classes --------
dataB=read.table('../../Functional Clustering/funHDDC/dataset_funct/Farines/far_bonne_colonnes.txt')
dataM=read.table('../../Functional Clustering/funHDDC/dataset_funct/Farines/far_mauv_colonnes.txt')
dataMo=read.table('../../Functional Clustering/funHDDC/dataset_funct/Farines/far_moy_colonnes.txt')
data=cbind(as.matrix(dataB[,2:51]),as.matrix(dataM[,2:41]),as.matrix(dataMo[,2:26]))
cls = cbind(matrix(3,1,50),matrix(1,1,40),matrix(2,1,25))
K=3

#sous echantillonnage pour que fclust passe
# perm=sample(1:115,50)
# data=data[,perm]
# cls=cls[,perm]



# # png('Farines.png',width=1000, height=1000)
# plot(1:241,data[,1],type='l',xlab="time",ylab="",xlim=c(0,250),ylim=c(0,max(data)));for(i in 2:115)lines(1:241,data[,i],type='l')
# # dev.off()
# # png('Farines-3.png',width=1000, height=1000)
plot(1:241,data[,1],type='l',xlab="time",ylab="",col=cls[,1],xlim=c(0,250),ylim=c(0,max(data)),main='Kneading (3 groups)');for(i in 2:115)lines(1:241,data[,i],type='l',col=cls[,i])
# # dev.off()

plot(1:241,data[,1],type='l',ylab='dureté de la pâte',xlab="temps de pétrissage (secondes)",col=1,xlim=c(0,250),ylim=c(0,max(data)));for(i in 2:115)lines(1:241,data[,i],type='l',col=1)

plot(1:241,data[,1],type='l',ylab='dureté de la pâte',xlab="temps de pétrissage (secondes)",col=cls[,1],xlim=c(0,250),ylim=c(0,max(data)));for(i in 2:115)lines(1:241,data[,i],type='l',col=cls[,i])

plot(1:241,data[,1],type='l',ylab='dureté de la pâte',xlab="temps de pétrissage (secondes)",col='green',xlim=c(0,250),ylim=c(0,max(data)));
lines(1:241,data[,65],col='red');
lines(1:241,data[,100],col="orange");

cls = cbind(matrix('green',1,50),matrix('red',1,40),matrix('orange',1,25))

for (i in seq(100,0,-10)){
  clsnew=cls
  clsnew[,sample(1:length(cls),length(cls)*i/100)]=clsnew[,sample(1:length(cls),length(cls)*i/100)]
  plot(1:241,data[,1],type='l',ylab='dureté de la pâte',xlab="temps de pétrissage (secondes)",col=clsnew[,1],xlim=c(0,250),ylim=c(0,max(data)));for(i in 2:115)lines(1:241,data[,i],type='l',col=clsnew[,i])
}

