#install.packages("akmeans") #install kmeans in R


# attach packages to workspace
library(akmeans)
library(lsa)
library(cluster)
library(vegan)
library(ggvis)

cname <- file.path("C:", "/RData/50Articles") 
#cname <- ("~", "Desktop", "10Magainzes)
#Use if on MAC^ 

dir(cname) 

#Load the package for text mining and load texts
library(tm)
docs <- Corpus(DirSource(cname))
summary(docs)

inspect(docs[2]) #test and inspect a document

library(SnowballC)
#the following code edits the data and removes excess information
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stemDocument)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)

inspect(docs[2]) #test


#Now Create Matrix
dtm <- DocumentTermMatrix(docs)

m <- as.matrix(dtm)
write.csv(m,file="dtmKMeans.csv")
#Transpose of dtm
tdm <- TermDocumentMatrix(docs)
tdm



normalized_dtm <- quick.norm(dtm,1) #normalize matrix, sum = 1




##EUCLIDEAN MATRIX
stdEuc <- dist((1/normalized_dtm)*(dtm)) #standardize Euclidean Distance
euc <- as.matrix(stdEucl)
euc.sort <- sort(euc, decreasing=TRUE)


kfit <- kmeans(stdEuc, 3, nstart=100)
clusplot(euc, kfit$cluster, color=T, shade=T, labels=1, lines=0)

wss = kmeans(euc, centers=1)$tot.withinss
for (i in 2:15)
  wss[i] = kmeans(euc, centers=i)$tot.withins

plot(1:15, wss, type='b', xlab="Number of Clusters",
     ylab="Within groups sum of squares")

##COSINE MATRIX

cosSim <- cosine(as.matrix(tdm))# Cosine similarity b/w each pair of rows
cos <- as.matrix(cosSim)
cos.sort <- sort(cos, decreasing=TRUE)


kfit <- kmeans(cosSim, 3, nstart=100)
clusplot(cos, kfit$cluster, color=T, shade=T, labels=1, lines=0)

wss = kmeans(cos, centers=1)$tot.withinss
for (i in 2:15)
  wss[i] = kmeans(cos, centers=i)$tot.withins

plot(1:15, wss, type='b', xlab="Number of Clusters",
     ylab="Within groups sum of squares")

##JACCARD MATRIX

binary.dm <- as.matrix(dtm)
binary.dm[binary.dm > 1] <- 1 


jacc.mat <- vegdist(binary.dm, method = "jaccard")
jacc <- as.matrix(jacc.mat)
jacc.sort <- sort(jacc, decreasing=TRUE)


kfit <- kmeans(jacc.mat, 3, nstart=100)
clusplot(jacc, kfit$cluster, color=T, shade=T, labels=1, lines=0)

wss = kmeans(jacc, centers=1)$tot.withinss
for (i in 2:15)
  wss[i] = kmeans(jacc, centers=i)$tot.withins

plot(1:15, wss, type='b', xlab="Number of Clusters",
     ylab="Within groups sum of squares")




