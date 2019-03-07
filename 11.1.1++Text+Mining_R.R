rm(list=ls())

#Set working directory
setwd("C:/Users/admin/akansha")
getwd()

#load libraries
library(stringr)
library(tm)
library(wordcloud)
library(slam)
library(sentiment)
install.packages("Rcpp")
install.packages("Rcpp")
install.packages("sentimentr")
library(sentimentr)

#Load comments/text
post = read.csv("Post.csv", header = T)

#Load defined stop words
#stop_words = read.csv("stopwords.csv", header = T)
#names(stop_words) = "StopWords"

#Delete the leading spaces
post$Post = str_trim(post$Post)

#Select only text column
post = data.frame(post[1:200,2])
names(post) = "comments"
post$comments = as.character(post$comments)

##Pre-processing
#convert comments into corpus
postCorpus = Corpus(VectorSource(post$comments))

writeLines(as.character(postCorpus[[3]]))
#postCorpus = tm_map(postCorpus, PlainTextDocument)

#case folding
postCorpus = tm_map(postCorpus, tolower)

#remove stop words
postCorpus = tm_map(postCorpus, removeWords, stopwords('english'))

#remove punctuation marks
postCorpus = tm_map(postCorpus, removePunctuation)

#remove numbers
postCorpus = tm_map(postCorpus, removeNumbers)

#remove unnecesary spaces
postCorpus = tm_map(postCorpus, stripWhitespace)

# #convert into plain text
# postCorpus = tm_map(postCorpus, PlainTextDocument)
# 
# #create corpus
# postCorpus = Corpus(VectorSource(postCorpus))

#Build document term matrix
tdm = TermDocumentMatrix(postCorpus)
#tdm_min = TermDocumentMatrix(postCorpus, control=list(weighting=weightTfIdf, minWordLength=4, minDocFreq=10))

#Convert term document matrix into dataframe
TDM_data = as.data.frame(t(as.matrix(tdm))) 

##calculate the terms frequency
words_freq = rollup(tdm, 2, na.rm=TRUE, FUN = sum)

#Convert into matrix
words_freq = as.matrix(words_freq)

#Convert to proper dataframe
words_freq = data.frame(words_freq)

#Convert row.names into index
words_freq$words = row.names(words_freq)
row.names(words_freq) = NULL
words_freq = words_freq[,c(2,1)]
names(words_freq) = c("Words", "Frequency")

#Most frequent terms which appears in atleast 700 times
findFreqTerms(tdm, 100)

##wordcloud
postCorpus_WC = postCorpus

pal2 = brewer.pal(8,"Dark2")
png("wordcloud_v2.png", width = 12, height = 8, units = 'in', res = 300)
wordcloud(postCorpus_WC, scale = c(5,.2), min.freq = 30, max.words = 150, random.order = FALSE, rot.per = .15, colors = pal2)
dev.off()

#Remove the defined stop words
postCorpus_WC = tm_map(postCorpus_WC, removeWords, c('will', 'also', 'can',
                                               stopwords('english')))
postCorpus_WC = tm_map(postCorpus_WC, removeWords, stop_words$StopWords)


#sentiment Analysis
#Another method
library(RSentiment)
post = data.frame(post[1:200,])
names(post) = 'comments'

df = calculate_sentiment(post$comments)

#Another method
#Install sentiment library using binay source
#install.packages("E:/Others/Edwisor/ContentRevamp/Advanced Predictive Analytics/Sentiment Analysis/sentiment_0.2.tar.gz", repos = NULL, type="source")
library(sentiment)

#classifying the corpus as negative and positive and neutral
polarity = classify_polarity(post$comments, algorithm = "bayes", verbose = TRUE)
polarity = data.frame(polarity)

#Attached sentiments to the comments
newdocs = cbind(post, polarity)

#Pie chart to visualise polarity
df = data.frame(table(newdocs$BEST_FIT))

#Interactive visualisations using plotly
library(plotly)

plot_ly(df, labels = ~Var1, values = ~Freq, type = 'pie') %>%
        layout(title = 'Sentiment Analysis',
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



