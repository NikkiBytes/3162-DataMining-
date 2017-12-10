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
dtm

inspect(dtm[1:5, 1:10])

#the following is a transpose of the above matrix 
tdm <- TermDocumentMatrix(docs)
tdm


#organize terms by frequency
freq <- colSums(as.matrix(dtm))
length(freq)

ord <- order(freq)
dm <- as.matrix(dtm)


write.csv(dm, file="datatm.csv")

#remove sparse terms
dtms <- removeSparseTerms(dtm, 0.1) # 10% empty space
inspect(dtms)



###Part II: COMPUTE THE SIMILARITY B/W EACH PAIR OF ARTICLES 

#Euclidean Distance:


normMatrix <- norm(dm, "f") #normalize matrix
stdEucl <- dist((1/normMatrix)*(dtm)) #standardize Euclidean Distance
euc <- as.matrix(stdEucl)
euc.sort <- sort(euc, decreasing=TRUE)
write.csv(euc, file="EuclideanDistance.csv")
write.csv(euc.sort, file="EuclideanDistanceSorted.csv")

#Cosine Similarity

cosSim <- cosine(as.matrix(tdm))# Cosine similarity b/w each pair of rows
cos <- as.matrix(cosSim)
cos.sort <- sort(cos, decreasing=TRUE)
write.csv(cos, file="CosineSimilarityMatrix.csv")
write.csv(cos.sort, file="CosineSimilaritySorted.csv")


#Jaccard

binary.dm <- as.matrix(dtm)
binary.dm[binary.dm > 1] <- 1 


jacc.mat <- vegdist(binary.dm, method = "jaccard")
jacc <- as.matrix(jacc.mat)
jacc.sort <- sort(jacc, decreasing=TRUE)
write.csv(jacc, file="JaccDistanceMatrix.csv")
write.csv(jacc.sort, file="JaccardDistanceSorted.csv")

