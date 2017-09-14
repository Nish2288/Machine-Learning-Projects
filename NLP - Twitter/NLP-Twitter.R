#install.packages('tm')
#install.packages('twitteR')
#install.packages('wordcloud')
#install.packages('RColorBrewer')
#install.packages('e1071')
#install.packages('class')


library(tm)
library(twitteR)
library(wordcloud)
library(e1071)
library(class)

ckey<-"AN9nCziLy3kYgMqbLnoQYuvpU"
skey<-"DbbDPSKMsTGk7OI74QTTpacNGO4L9D1wslqgCRMJLVxeIMEAPt"
token<-"90484454-3t8SxoOJOpLsvPOEF8tUbtSoGvBvfJFIw1DxOviH0"
stoken<-"PRim0RnifHnROWqdtamg4yH2JJxICOxolnH25a37nUpJ3"

#COnnect to twitter

setup_twitter_oauth(ckey,skey,token,stoken)

#Get the tweet

tweets <- searchTwitter("data+Science", n=2000, lang="en")
text <- sapply(tweets, function(x) x$getText())

text <- iconv(text, 'UTF-8', 'ASCII') # remove emoticons
corpus <- Corpus(VectorSource(text)) # create a corpus

term.doc.matrix <- TermDocumentMatrix(corpus,
                                      control = list(removePunctuation = TRUE,
                                                     stopwords = c("data+science","https", stopwords("english")),
                                                     removeNumbers = TRUE,tolower = TRUE))


term.doc.matrix <- as.matrix(term.doc.matrix)

word.freqs <- sort(rowSums(term.doc.matrix), decreasing=TRUE) 
dm <- data.frame(word=names(word.freqs), freq=word.freqs)

wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

















