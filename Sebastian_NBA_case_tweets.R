#' Title: NBA Case Analysis
#' NAME: SEBASTIAN VALENZUELA URENA
#' Date: March 2022
#' The whole code is free from error, it has warnings about the version of some libraries but it runs perfectly###

# To limit errors 
Sys.setlocale('LC_ALL','C')

# set working directory
setwd("~/Documents/HULT/MBA/Texting Mining/Text-Mining-NLP/Case/Case I/Data")

# Loadin libraries (Some get warnings about the version but they run appropriate)
library(ggplot2)
library(ggthemes)
library(stringi)
library(tm)
library(stringi)
library(stringr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(SentimentAnalysis)
library(syuzhet)
library(lubridate)
library(scales)
library(reshape2)
library(wordcloud)
library(wordcloud2)
library(data.table)

# load the data in objects (To use as analysis by month)
Dec2019 <- read.csv('C_Dec2019.csv')
July2020 <- read.csv('J_July2020.csv')
Aug2020 <- read.csv('K_Aug2020.csv')
Sep2020 <- read.csv('L_Sep2020.csv')
Oct2020 <- read.csv('M_Oct2020.csv')

#Merging the csv to create a file with all months (To analyze as a whole)
files <- list.files(pattern = '*.csv')
temp <- lapply(files, fread, sep= ',')
all_data <- rbindlist(temp)

#Eliminating duplicates from the database (By month)
Dec2019 <- Dec2019[!duplicated(Dec2019$text), ]
July2020 <- July2020[!duplicated(July2020$text), ]
Aug2020 <- Aug2020[!duplicated(Aug2020$text), ]
Sep2020 <- Sep2020[!duplicated(Sep2020$text), ]
Oct2020 <- Oct2020[!duplicated(Oct2020$text), ]

#Eliminating duplicates from the database (All months)
all_data <- all_data[!duplicated(all_data$text), ]

#Finding missing values and counting them if there are any
is.na(all_data)
sum(is.na(all_data))

#creating function to clean data from RT and http
basicSubs <- function(x){
  x <- gsub('http\\S+\\s*', '', x)
  x <- gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', x)
  x <- tolower(x)
  return(x)
}

##Starting the analysis of players using the whole data to observe different insights###  
# Apply the function to the text
txt <- basicSubs(all_data$text)

# Observe different athlete names
lebron  <- sum(stri_count(txt, regex ='lebron'|'lebron james'))
giannis  <- sum(stri_count(txt, regex ='giannis', regex= 'giannis antetokounmpo'))
kawhi <- sum(stri_count(txt, regex ='kawhi'))
lillard <- sum(stri_count(txt, regex ='lillard'))
harden <- sum(stri_count(txt, regex ='harden'))
curry <- sum(stri_count(txt, regex ='curry'))

# Organize term objects into a data frame
termFreq <- data.frame(terms = c('lebron','giannis','kawhi', 'lillard', 'harden', 'curry'),
                       freq  = c(lebron, giannis, kawhi, lillard, harden, curry))

# Examine
termFreq

# Plot a geom_bar with ggplot2 by filling in the correct data, adding a layers "theme_gdocs() + theme(legend.position = "none")"
ggplot(termFreq, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none") + ggtitle("Athletes") 

# Observe different brands names
nike <- sum(stri_count(txt, regex ='nike'))
adidas <- sum(stri_count(txt, regex ='adidas'))

# Organize term objects into a data frame
termFreq_brands <- data.frame(terms = c('nike','adidas'),
                       freq  = c(nike, adidas))

# Examine
termFreq_brands

# Plot a geom_bar with ggplot2 by filling in the correct data, adding a layers "theme_gdocs() + theme(legend.position = "none")"
ggplot(termFreq_brands, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none") + ggtitle("Brands")

#############Starting the analysis of selected months ###########

#Creating the function to clean text
cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Create some stopwords including "nba" since is the most popular but no insights in it####
stops <- c(stopwords('english'), 'nba', 'rt')

###############################October 2020####################################

#Apply VCorpus to create a Corpus 
# Apply the VCorpus Function to a VectorSource of the original text object
cleanTxt <- VCorpus(VectorSource(Oct2020$text))
# Clean the Corpus with your cleanCorpus function
cleanTxt <- cleanCorpus(cleanTxt, stops)
# Construct a DTM 
dtm <- DocumentTermMatrix(cleanTxt)
dtm2 <- removeSparseTerms(dtm, sparse = 0.99)
#Create a matrix with the dtm
dtm_matrix <- as.matrix(dtm2)
#get total frequency 
freq <- colSums(dtm_matrix)
#create order
ord <- order(freq, decreasing = TRUE)
#Most frequent occurring terms
freq[head(ord,200)]

#Creating a plot to visualize the data 
data_histogramOct2020 <- data.frame(term= names(freq), occurrences= freq)

p <- ggplot(subset(data_histogramOct2020, freq>3500), aes(term, occurrences))
p <- p + geom_bar(stat='identity', fill='mediumpurple2')
p <- p + theme(axis.text.x = element_text(angle=0, hjust = 1)) + labs(title = "October 2020")
p <- p + geom_text(aes(label=occurrences), colour='white',hjust=1.25, size=3.0) + coord_flip()
p

#creating a Word Cloud
# Bigram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}
# Make bi-gram TDM according to the tokenize control & convert it to matrix
October2020TDM  <- TermDocumentMatrix(cleanTxt, 
                               control=list(tokenize=bigramTokens))
October2020TDM2 <- removeSparseTerms(October2020TDM, sparse = 0.99)
October2020TDMm <- as.matrix(October2020TDM2)

# Get Row Sums & organize
October2020TDMv <- sort(rowSums(October2020TDMm), decreasing = TRUE)
October2020DF   <- data.frame(word = names(October2020TDMv), freq = October2020TDMv)

# Choose a color & drop light ones
pal <- brewer.pal(8, "Purples")
pal <- pal[-(1:2)]

# Make simple word cloud
# Reminder to expand device pane
set.seed(1234)
wordcloud(October2020DF$word,
          October2020DF$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))


#############################September 2020####################################

#Apply VCorpus to create a Corpus 
# Apply the VCorpus Function to a VectorSource of the original text object
cleanTxtSep2020 <- VCorpus(VectorSource(Sep2020$text))
# Clean the Corpus with your cleanCorpus function
cleanTxtSep2020 <- cleanCorpus(cleanTxtSep2020, stops)
# Construct a DTM 
dtm02 <- DocumentTermMatrix(cleanTxtSep2020)
dtm002 <- removeSparseTerms(dtm02, sparse = 0.99)
#Create a matrix with the dtm
dtm_matrixSep2020 <- as.matrix(dtm002)
#get total frequency 
freqSep2020 <- colSums(dtm_matrixSep2020)
#create order
ordSep2020 <- order(freqSep2020, decreasing = TRUE)
#Most frequent occurring terms
freqSep2020[head(ordSep2020,143)]

#Creating a plot to visualize the data 
data_histogramSep2020 <- data.frame(term= names(freqSep2020), occurrences= freqSep2020)

pSep <- ggplot(subset(data_histogramSep2020, freqSep2020>5000), aes(term, occurrences))
pSep <- pSep + geom_bar(stat='identity', fill='orangered3')
pSep <- pSep + theme(axis.text.x = element_text( hjust = 1)) + labs(title = "September 2020")
pSep <- pSep + geom_text(aes(label=occurrences), colour='white',hjust=1.25, size=3.0) + coord_flip()
pSep

#creating a Word Cloud

# Make bi-gram TDM according to the tokenize control & convert it to matrix
September2020TDM  <- TermDocumentMatrix(cleanTxtSep2020, 
                                      control=list(tokenize=bigramTokens))
September2020TDM2 <- removeSparseTerms(September2020TDM, sparse = 0.99)
September2020TDMm <- as.matrix(September2020TDM2)

# Get Row Sums & organize
September2020TDMv <- sort(rowSums(September2020TDMm), decreasing = TRUE)
September2020DF   <- data.frame(word = names(September2020TDMv), freq = September2020TDMv)

# Choose a color & drop light ones
pal <- brewer.pal(8, "Reds")
pal <- pal[-(1:2)]

# Make simple word cloud
# Reminder to expand device pane
set.seed(1234)
wordcloud(September2020DF$word,
          September2020DF$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))

############################August 2020########################################

#Apply VCorpus to create a Corpus 
# Apply the VCorpus Function to a VectorSource of the original text object
cleanTxtAug2020 <- VCorpus(VectorSource(Aug2020$text)) 
# Clean the Corpus with your cleanCorpus function
cleanTxtAug2020 <- cleanCorpus(cleanTxtAug2020, stops)
# Construct a DTM 
dtm02_Aug2020 <- DocumentTermMatrix(cleanTxtAug2020)
dtm002_Aug2020 <- removeSparseTerms(dtm02_Aug2020, sparse = 0.99)
#Create a matrix with the dtm
dtm_matrixAug2020 <- as.matrix(dtm002_Aug2020)
#get total frequency 
freqAug2020 <- colSums(dtm_matrixAug2020)
#create order
ordAug2020 <- order(freqAug2020, decreasing = TRUE)
#Most frequent occurring terms
freqAug2020[head(ordAug2020,146)]

#Creating a plot to visualize the data 
data_histogramAug2020 <- data.frame(term= names(freqAug2020), occurrences= freqAug2020)

pAug <- ggplot(subset(data_histogramAug2020, freqAug2020>8000), aes(term, occurrences))
pAug <- pAug + geom_bar(stat='identity', fill='dodgerblue1')
pAug <- pAug + theme(axis.text.x = element_text(hjust = 1)) + labs(title = "August 2020")
pAug <- pAug + geom_text(aes(label=occurrences), colour='white',hjust=1.25, size=3.0) + coord_flip()
pAug

#creating a Word Cloud

# Make bi-gram TDM according to the tokenize control & convert it to matrix
August2020TDM  <- TermDocumentMatrix(cleanTxtAug2020, 
                                        control=list(tokenize=bigramTokens))
August2020TDM2 <- removeSparseTerms(August2020TDM, sparse = 0.99)
August2020TDMm <- as.matrix(August2020TDM2)

# Get Row Sums & organize
August2020TDMv <- sort(rowSums(August2020TDMm), decreasing = TRUE)
August2020DF   <- data.frame(word = names(August2020TDMv), freq = August2020TDMv)

# Choose a color & drop light ones
pal <- brewer.pal(8, "Blues")
pal <- pal[-(1:2)]

# Make simple word cloud
# Reminder to expand device pane
set.seed(1234)
wordcloud(August2020DF$word,
          August2020DF$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))

##############################July 2020########################################

#Apply VCorpus to create a Corpus 
# Apply the VCorpus Function to a VectorSource of the original text object
cleanTxtJuly2020 <- VCorpus(VectorSource(July2020$text)) 
# Clean the Corpus with your cleanCorpus function
cleanTxtJuly2020 <- cleanCorpus(cleanTxtJuly2020, stops)
# Construct a DTM 
dtm02_July2020 <- DocumentTermMatrix(cleanTxtJuly2020)
dtm002_July2020 <- removeSparseTerms(dtm02_July2020, sparse = 0.99)
#Create a matrix with the dtm
dtm_matrixJuly2020 <- as.matrix(dtm002_July2020)
#get total frequency 
freqJuly2020 <- colSums(dtm_matrixJuly2020)
#create order
ordJuly2020 <- order(freqJuly2020, decreasing = TRUE)
#Most frequent occurring terms
freqJuly2020[head(ordJuly2020,146)]

#Creating a plot to visualize the data 
data_histogramJuly2020 <- data.frame(term= names(freqJuly2020), occurrences= freqJuly2020)

pJuly <- ggplot(subset(data_histogramJuly2020, freqJuly2020>5000), aes(term, occurrences))
pJuly <- pJuly + geom_bar(stat='identity', fill='green4')
pJuly <- pJuly + theme(axis.text.x = element_text(hjust = 1)) + labs(title = "July 2020")
pJuly <- pJuly + geom_text(aes(label=occurrences), colour='white',hjust=1.25, size=3.0) + coord_flip()
pJuly

#creating a Word Cloud

# Make bi-gram TDM according to the tokenize control & convert it to matrix
July2020TDM  <- TermDocumentMatrix(cleanTxtJuly2020, 
                                     control=list(tokenize=bigramTokens))
July2020TDM2 <- removeSparseTerms(July2020TDM, sparse = 0.99)
July2020TDMm <- as.matrix(July2020TDM2)

# Get Row Sums & organize
July2020TDMv <- sort(rowSums(July2020TDMm), decreasing = TRUE)
July2020DF   <- data.frame(word = names(July2020TDMv), freq = July2020TDMv)

# Choose a color & drop light ones
pal <- brewer.pal(8, "Greens")
pal <- pal[-(1:2)]

# Make simple word cloud
# Reminder to expand device pane
set.seed(1234)
wordcloud(July2020DF$word,
          July2020DF$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))


###############December 2019#################################################
#Apply VCorpus to create a Corpus 
# Apply the VCorpus Function to a VectorSource of the original text object
cleanTxtDec2019 <- VCorpus(VectorSource(Dec2019$text))
# Clean the Corpus with your cleanCorpus function
cleanTxtDec2019 <- cleanCorpus(cleanTxtDec2019, stops)
# Construct a DTM 
dtmDec2019 <- DocumentTermMatrix(cleanTxtDec2019)
dtm2Dec2019 <- removeSparseTerms(dtmDec2019, sparse = 0.99)
#Create a matrix with the dtm
dtm_matrixDec2019 <- as.matrix(dtm2Dec2019)
#get total frequency 
freqDec2019 <- colSums(dtm_matrixDec2019)
#create order
ordDec2019 <- order(freqDec2019, decreasing = TRUE)
#Most frequent occurring terms
freqDec2019[head(ordDec2019,200)]

#Creating a plot to visualize the data 
data_histogramDec2019 <- data.frame(term= names(freqDec2019), occurrences= freqDec2019)

pDec2019 <- ggplot(subset(data_histogramDec2019, freqDec2019>8000), aes(term, occurrences))
pDec2019 <- pDec2019 + geom_bar(stat='identity', fill='grey63')
pDec2019 <- pDec2019 + theme(axis.text.x = element_text(angle=0, hjust = 1)) + labs(title = "December 2019")
pDec2019 <- pDec2019 + geom_text(aes(label=occurrences), colour='white',hjust=1.25, size=3.0) + coord_flip()
pDec2019

#creating a Word Cloud
# Bigram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}
# Make bi-gram TDM according to the tokenize control & convert it to matrix
December2019TDM  <- TermDocumentMatrix(cleanTxtDec2019, 
                                      control=list(tokenize=bigramTokens))
December2019TDM2 <- removeSparseTerms(December2019TDM, sparse = 0.99)
December2019TDMm <- as.matrix(December2019TDM2)

# Get Row Sums & organize
December2019TDMv <- sort(rowSums(December2019TDMm), decreasing = TRUE)
December2019DF   <- data.frame(word = names(December2019TDMv), freq = December2019TDMv)

# Choose a color & drop light ones
pal <- brewer.pal(8, "Greys")
pal <- pal[-(1:2)]

# Make simple word cloud
# Reminder to expand device pane
set.seed(1234)
wordcloud(December2019DF$word,
          December2019DF$freq,
          max.words    = 40,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))


####Analyzing sentiments (Miami Heat - October 2020)####


#Dividing by most frequent teams and getting their sentiments

Oct2020_Miami <- Oct2020[which(Oct2020$team == 'Miami Heat'),]
Oct2020_Lakers <- Oct2020[which(Oct2020$team == 'LA Lakers'),]


#Analyzing sentiments (Atlanta team only in October 2020)

sentiments_MiamiOct2020 <- get_nrc_sentiment(Oct2020_Miami$text)

barplot(colSums(sentiments_MiamiOct2020),
        las=2,
        col=rainbow(10),
        ylab='Count',
        main='Sentiment Scores for Miami Heat')

#Analyzing sentiments (Atlanta team only in October 2020)

sentiments_LakersOct2020 <- get_nrc_sentiment(Oct2020_Lakers$text)

barplot(colSums(sentiments_LakersOct2020),
        las=2,
        col=rainbow(10),
        ylab='Count',
        main='Sentiment Scores for LA Lakers')


#End of the code


