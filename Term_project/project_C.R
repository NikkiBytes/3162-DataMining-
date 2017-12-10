#Nichollette Acosta | ITcs 3162 | Term Project

#install teaching demos, bio3d 

#install.packages(c("TeachingDemos"),repo="http://cran.r-project.org",
                 #+ dep=TRUE)
                 
                 
#rm(list=ls())   


setwd("~/3162-16/Project")
source("http://www.bioconductor.org/biocLite.R")
biocLite()
biocLite("ALL")
biocLite("multtest")

library("ALL")
library(multtest)
data(ALL)
data(golub) #dataset





#selecting genes related to "Cyclin"
#compute Euclidean distane between the gene expressions 


index <- grep("Cyclin", golub.gnames[,2])
golub.gnames[index,2]
dist.cyclin <- dist(golub[index,], method="euclidian")
eucl_matrix <- as.matrix(dist.cyclin)
rownames(eucl_matrix) <- colnames(eucl_matrix) <- golub.gnames[index,3]
eucl_matrix[1:5,1:5]


#Relating data generation processes to cluster trees.
data(golub, package="multtest")
clustal_data <- data.frame(golub[1042,],golub[2124,])
colnames(clustal_data)<-c("CCND3 CYCLIN D3","ZYXIN")
par(mfrow=c(1,2))
gol.fac <- factor(golub.cl,levels=0:1, labels= c("ALL","AML"))
plot(clustal_data, pch=as.numeric(gol.fac),
     main="Plot of gene CCND3 Cyclin D3 and Zyxin expressions for ALL and AML patients",
     col = c("green","blue")[as.numeric(gol.fac)])
legend("topright",legend=c("ALL","AML"),pch=1:2)
plot(hclust(dist(clustal_data,method="euclidian"),method="single"), main="Single linkage cluster
diagram from gene CCND3 Cyclin D3 and Zyxin expressions")

#K-Means Cluster analysis to simulate gene exprs from 2 different norm pop
#Randomly taken 50 gene expr for two persons from the N(0,0.5) and 50 gene expr
#for 2 persons from the N(2,0.5) pop. 

#Data is 2 matrices of order 50 by 2

#################### PART 1 ####################################################### 
#K-means clustering with 2 cluster of sizes 50, 50 

data1 <- rbind(matrix(rnorm(100,0,0.5), ncol = 2),
              + matrix(rnorm(100,2,0.5), ncol = 2))
clusterA <- kmeans(data1,2)

#K-means clustering with 2 cluster of sizes 75, 75 
data2 <- rbind(matrix(rnorm(100,0,0.75), ncol = 2),
              + matrix(rnorm(100,2,0.75), ncol = 2))
clusterB <- kmeans(data2,2)


#the output of k-means cluster analysis is assigned to a list called c1
par(mfrow=c(1,2))
plot(data1, col = clusterA$cluster, main = "Size 50")
points(clusterA$centers, col = 1:2, pch = 8, cex=2)
plot(data2, col = clusterB$cluster,  main = "Size 75")
points(clusterB$centers, col = 1:2, pch = 8, cex=2)

#We can use a bootstrap to estimate 95% confidence intervals around cluster means.
#The idea is to resample with replacement fro the given sample 1000 times
#with replacement and to compute quantiles for the corresponding confidence intervals
#
init <- matrix(c(0,0,2,2), nrow = 2, ncol=2, byrow=TRUE)


cA <- kmeans(data1, init, nstart = 10)
cB <- kmeans(data2, init, nstart = 10)

n <- 100; n_boot<-1000
boot.cA <- matrix(0,nrow=n_boot,ncol = 4)
boot.cB<- matrix(0,nrow=n_boot,ncol = 4)

for (i in 1:n_boot){
  dat.star <- data1[sample(1:n,replace=TRUE),]
  cA <- kmeans(dat.star, init, nstart = 10)
  boot.cA[i,] <- c(cA$centers[1,],cA$centers[2,])
}



quantile(boot.cA[,1],c(0.025,0.975))
quantile(boot.cA[,2],c(0.025,0.975))
quantile(boot.cA[,3],c(0.025,0.975))
quantile(boot.cA[,4],c(0.025,0.975))



for (i in 1:n_boot){
  dat.star <- data2[sample(1:n,replace=TRUE),]
  cB <- kmeans(dat.star, init, nstart = 10)
  boot.cB[i,] <- c(cB$centers[1,],cB$centers[2,])
}

quantile(boot.cB[,1],c(0.025,0.975))
quantile(boot.cB[,2],c(0.025,0.975))
quantile(boot.cB[,3],c(0.025,0.975))
quantile(boot.cB[,4],c(0.025,0.975))


########################### PART II ###############################################3

#Above analysis we found that the expr values of the genes "CCND3 Cyclin D3" and "Zyxin" 
#are closely related to the distinction b/w ALL and AML. 
#Hence a 2-means cluster analysis of the gene expr values are appropriate here

#K-means clustering with 2 clusters of size 11, 27: 




data(golub, package="multtest")

data <- data.frame(golub[1042,],golub[2124,])
colnames(data)<-c("CCND3 Cyclin D3","Zyxin")
clusterC <- kmeans(data, 2,nstart = 10)
clusterC

plot(data, col = clusterC$cluster, 
     main="Cluster Analysis on CCND3 Cyclin D3 and Zyxin,ALL(black) AML(red)")
points(clusterC$centers, col = 1:2, pch = 8, cex=2)
#The two clusters discriminate exactly the ALL patients from the AML patients
#This is also seen from the figureX, generated below.
#where expression values of
#CCND3 Cyclin D3 are depicted on the horizontal axis and those of Zyxin
#on the vertical, and the ALL patients are in red and the AML patients in
#black. By the bootstrap the cluster means and their con¯dence intervals can
#be estimated.
mean(data.frame(boot.cA))
quantile(boot.cA[,1],c(0.025,0.975))
quantile(boot.cA[,2],c(0.025,0.975))
quantile(boot.cA[,3],c(0.025,0.975))
quantile(boot.cA[,4],c(0.025,0.975))
