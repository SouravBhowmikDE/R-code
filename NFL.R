data1<-read.csv("C:/Users/soura/Desktop/Data Science/Data Sets/NFLData.csv",head=T)
#View(data1)

library(tm)
#library(filehash)

# Convert the machine log(content column) into text corpus
log <- Corpus (VectorSource(data1$content))

inspect(log[1:5])

#log1 <- Corpus (VectorSource(data))

#convert tstamp column to text corpus
timedata <- Corpus (VectorSource(data$tstamp ))

#viewing the contents of each of the corpus
log[[2]]$content
timedata[[5]]$content

#Converting to lowercase
log <- tm_map  (log,content_transformer( tolower))


log <- tm_map(log,removeWords,stopwords(kind="en"))

#Removing puntuations
log <- tm_map(log,removePunctuation)

#Removing numbers
log <- tm_map(log,removeNumbers)

#Eliminating white spaces
log <- tm_map(log,stripWhitespace)

log <- tm_map(log, removeWords, c("else"," the","are","for",
                                                        "has","they","as","a","his","on",
                                                        "when","is","in","already","next",
                                                        "make","now","see"))

#Creating a DocumentTermMatrix
doctm<-DocumentTermMatrix(log) 

inspect(doctm[1:10, 1001:1010])

#------------------------------------------------------------------------------------------------------------

#Finding frequency of terms
findFreqTerms(doctm, 200)

#Finding the tf idf
docterms <-DocumentTermMatrix(log, control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
#----------------------------------------------------------------------------------------------------------------


# tfxidf weighting
dtm_tfxidf <- weightTfIdf(doctm)
inspect(dtm_tfxidf[1:10, 1001:1010])

# K-means document clustering

#converting it to a matrix
mtx <- as.matrix(dtm_tfxidf)
rownames(mtx) <- 1:nrow(mtx)


# normalising the matrix vectors
norm_eucl <- function(mtx) mtx/apply(mtx, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(mtx)

### clustering into 10 clusters
cl10 <- kmeans(m_norm, 10)
table(cl10$cluster)

  
x <- data.frame(Log = data$content, Cluster = cl10$cluster)
write.csv(x, file = "C:/Users/soura/Desktop/Data Science/Data Sets/Cluster_Out.csv", row.names=TRUE)

data1 <- read.csv("C:/Users/soura/Desktop/Data Science/Data Sets/Cluster_Out.csv")

#### Find top 5 words in all 10 clusters
reduced_dtm <- removeSparseTerms(doctm, sparse= 0.9999)
newdtm <- as.matrix(reduced_dtm)


#operating on each of the 10 clusters
for (N in 1:length(cl10$withinss)) {
  a <- sort(colSums(newdtm[cl10$cluster == N, ]),
            decreasing = TRUE)
  df <- data.frame(names(a), a)
  colnames(df) <- c("word","count")
  
  if (N == 1){
    x <- data.frame(N, length(which(data1$Cluster == N )), df$word[1:5], 
                    df$count[1:5], as.numeric(rownames(x))[1:5])
    colnames(x) = c("Loggroup", "Logcount", "Top Words", "Word Count", "Counter")
  } else {
    y <- data.frame(N, length(which(data1$Cluster == N )), df$word[1:5],
                    df$count[1:5], as.numeric(rownames(x))[1:5]) 
    colnames(y) = c("Loggroup", "Logcount", "Top Words", "Word Count", "Counter")
    x <- rbind(x, y)
  }
}

#### Write these cluster wise top words into the "TopWords.csv" file
write.csv(x, file = "C:/Users/soura/Desktop/Data Science/Data Sets/Topwords.csv", row.names=FALSE)

#-------------------------------------------------------------------------------------------------------------



library(quanteda)
# create a document-feature matrix
IHADdfm <- dfm(doc.text, ignoredFeatures = c("will", stopwords("english")), verbose = FALSE)
# 12 most frequent features
topfeatures(IHADdfm, 12)
## freedom      one     ring    dream      let      day    negro    today     able    every together    years 
##      13       12       12       11       10        9        8        7        7        7        6        5 
# a word cloud, if you wish
plot(IHADdfm, random.order = FALSE)


meta(log)
meta(log[[567]])
