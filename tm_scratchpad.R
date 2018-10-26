library(tm)
library(SnowballC)
library(stringr)
library(dplyr)
library(ggplot2)
library(textir)

# https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf

# personal computer
setwd("C:/Users/Stephen/Desktop/R/coursera_code/capstone")

# work computer
setwd("H:/R/coursera_code/capstone")

# read in twitter data
con <- file("en_US/en_US.twitter.txt", "r")
twitter <- readLines(con, encoding = "UTF-8")
close(con)

# read in blogs data
con <- file("en_US/en_US.blogs.txt", "r")
blogs <- readLines(con, encoding = "UTF-8")
close(con)

# read in news data
con <- file("en_US/en_US.news.txt", "r")
news <- readLines(con, encoding = "UTF-8")
close(con)

# convert character strings to corpus
twitter_corpus <- Corpus(VectorSource(twitter), readerControl = list(reader = readPlain, language = 'en'))
blogs_corpus <- Corpus(VectorSource(blogs), readerControl = list(reader = readPlain, language = 'en'))
news_corpus <- Corpus(VectorSource(news), readerControl = list(reader = readPlain, language = 'en'))

# exploratory analysis of corpus
inspect(news_corpus[1]) # view meta data on first document in corpus
as.character(news_corpus[[1]]) # view text of first document in corpus
writeLines(as.character(news_corpus[[1]])) # also views text, but without the quotes (not a big improvement)

# clean corpus
news_corpus2 <- tm_map(news_corpus, removeNumbers)
news_corpus2 <- tm_map(news_corpus2, removePunctuation)
# news_corpus2 <- tm_map(news_corpus2, removeWords, stopwords("english")) # may not want to remove stopwords for predictive model
# news_corpus2 <- tm_map(news_corpus2, stemDocument, language = "english") also might not want to stem document for predictive model

# remove any custom words
# news_corpus2 <- tm_map(news_corpus2, removeWords, c("baseball", "football")) 

news_corpus2 <- tm_map(news_corpus2, stripWhitespace)
news_corpus2 <- tm_map(news_corpus2, tolower)
news_corpus2 <- tm_map(news_corpus2, PlainTextDocument)

# create a DocumentTermMatrix
news_dtm <- DocumentTermMatrix(news_corpus2)
inspect(news_dtm)
inspect(news_dtm[1:50, 1:10])

# inspect dtm
# get term frequency by col sums
news_dtm_col_sums <- as.matrix(slam::col_sums(news_dtm, na.rm=TRUE))
# convert to a dataframe
news_freq <- data.frame(term = rownames(news_dtm_col_sums), freq = news_dtm_col_sums[, 1])
rownames(news_freq) <- NULL
news_freq <- arrange(news_freq, desc(freq))
head(news_freq)

# remove sparse terms from dtm
news_dtms <- removeSparseTerms(news_dtm, 0.9)   
inspect(news_dtms)
inspect(news_dtms[1:10, 1:10])

# find frequent terms
findFreqTerms(news_dtm, lowfreq=10000)
findFreqTerms(news_dtm, lowfreq=1000)

# create a TermDocumentMatrix
news_tdm <- TermDocumentMatrix(news_corpus2)
inspect(news_tdm)
inspect(news_tdm[1:50, 1:10])
inspect(news_tdm[11350:11360, 1:10])

# inspect tdm
# get term frequency by col sums
news_tdm_row_sums <- as.matrix(slam::row_sums(news_tdm, na.rm=TRUE))
# convert to a dataframe
news_freq2 <- data.frame(term = rownames(news_tdm_row_sums), freq = news_tdm_row_sums[, 1])
rownames(news_freq2) <- NULL
news_freq2 <- arrange(news_freq2, desc(freq))
head(news_freq2)

# create bigram tdm
BigramTokenizer <- function(x) {
        unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}
news_bigram_tdm <- TermDocumentMatrix(news_corpus2, control = list(tokenize = BigramTokenizer))

# inspect bigram tdm
inspect(news_bigram_tdm)
inspect(news_bigram_tdm[1000:1050, 1:10])
news_bigram_tdm_row_sums <- as.matrix(slam::row_sums(news_bigram_tdm, na.rm=TRUE))
# convert to a dataframe
news_tdm_tfidf  <- data.frame(term = rownames(news_bigram_tdm_row_sums), freq = news_bigram_tdm_row_sums[, 1])
rownames(news_tdm_tfidf ) <- NULL
news_tdm_tfidf  <- arrange(news_tdm_tfidf , desc(freq))
head(news_tdm_tfidf )

# create trigram tdm
trigramTokenizer <- function(x) {
        unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
}
news_trigram_tdm <- TermDocumentMatrix(news_corpus2, control = list(tokenize = trigramTokenizer))

# inspect trigram tdm
inspect(news_trigram_tdm)
inspect(news_trigram_tdm[1000:1050, 1:10])
news_trigram_tdm_row_sums <- as.matrix(slam::row_sums(news_trigram_tdm, na.rm=TRUE))
# convert to a dataframe
news_trigram_freq <- data.frame(term = rownames(news_trigram_tdm_row_sums), freq = news_trigram_tdm_row_sums[, 1])
rownames(news_trigram_freq) <- NULL
news_trigram_freq <- arrange(news_trigram_freq, desc(freq))
head(news_trigram_freq)

# graph frequency of ngrams
# unigram bar chart
news_unigram_bar <- ggplot(data = news_freq[1:50, ], aes(x = reorder(term, freq), y = freq, fill = freq)) + 
        geom_bar(color = "black", stat = "identity") +
        coord_flip() + scale_fill_gradient(low = "lightskyblue", high = "darkblue")

# bigram bar chart
news_bigram_bar <- ggplot(data = news_tdm_tfidf [1:50, ], aes(x = reorder(term, freq), y = freq, fill = freq)) + 
        geom_bar(color = "black", stat = "identity") +
        coord_flip() + scale_fill_gradient(low = "lightskyblue", high = "darkblue")

# trigram bar chart
news_trigram_bar <- ggplot(data = news_trigram_freq[1:50, ], aes(x = reorder(term, freq), y = freq, fill = freq)) + 
        geom_bar(color = "black", stat = "identity") +
        coord_flip() + scale_fill_gradient(low = "lightskyblue", high = "darkblue")

# find most frequent ngrams for test phrases
test1 <- news_trigram_freq[grep("^the best ", news_trigram_freq$term), ]

# highest tf-idf terms
news_keywords <- tfidf(news_dtm)
head(news_keywords)
news_keywords

news_tdm_tfidf <- TermDocumentMatrix(news_corpus2, control = list(weighting = weightTfIdf))
inspect(news_tdm_tfidf[82500:82600, 1:10])
inspect(news_tdm_tfidf[25307:25307, 1:10000])

# get freq matrix with weighting by freq
news_tdm_row_sums <- as.matrix(slam::row_sums(news_tdm, na.rm=TRUE))

# convert to a dataframe
news_tdm_tfidf_row_sums <- as.matrix(slam::row_sums(news_tdm_tfidf, na.rm=TRUE))

news_tfidf  <- data.frame(term = rownames(news_tdm_tfidf_row_sums), tfidf_sum = news_tdm_tfidf_row_sums[ , 1], 
                          freq = news_tdm_row_sums[ , 1])
news_tfidf$avg <- news_tfidf$tfidf_sum / news_tfidf$freq

# for some reason about 10% of terms have tf-idf score > 1 (nevermind: tf-idf can be greater than 1)
rownames(news_tfidf) <- NULL
news_tfidf  <- arrange(news_tfidf , desc(avg))
head(news_tfidf, 10)

news_tfidf$term[which(news_tfidf$avg > 1)]
unique(news_tfidf$freq[which(news_tfidf$avg > 1)])
filter(news_tfidf, term == "earplugs")

# arrange news_tfidf by descending freq
news_tfidf <- arrange(news_tfidf, desc(freq))
head(news_tfidf, 20)

# try using a smaller corpus to see actual docs where a term gets tf-idf > 1 (nevermind: tf-idf can be greater than 1)
news_corpus2[[1]]
as.character(news_corpus2[[1]])
length(news_corpus2)

news_corpus3 <- news_corpus2[1:10]
length(news_corpus3)
news_corpus3[[1]]
as.character(news_corpus3[[1]])

# build small tdm with frequency weighting
news3_tdm <- TermDocumentMatrix(news_corpus3)
inspect(news3_tdm[1:10, ])

# build small tdm with tf-idf weighting
news3_tdm_tfidf <- TermDocumentMatrix(news_corpus3, control = list(weighting = weightTfIdf))
inspect(news3_tdm_tfidf[1:10, ])
news3_tdm_row_sums <- as.matrix(slam::row_sums(news3_tdm, na.rm=TRUE))

# get avg tfidf
news3_tdm_tfidf_row_sums <- as.matrix(slam::row_sums(news3_tdm_tfidf, na.rm=TRUE))

news3_tfidf  <- data.frame(term = rownames(news3_tdm_tfidf_row_sums), sum = news3_tdm_tfidf_row_sums[ , 1], 
                           freq = news3_tdm_row_sums[ , 1])
news3_tfidf$avg <- news3_tfidf$sum / news3_tfidf$freq
head(news3_tfidf)
max(news3_tfidf$avg)
news3_tfidf <- arrange(news3_tfidf, desc(avg))
nrow(news3_tfidf)

# terms "detroit" and "charlevoix" have avg tfidf > 1
inspect(news3_tdm)
inspect(news3_tdm[150:217, ])
inspect(news3_tdm[50:55, ]) # detroit only appears in one document

inspect(news3_tdm_tfidf[50:55, ]) # detroit has a 1.66 tf-idf for that document?! - nevermind tf-idf can be greater than 1

# build my own tf-idf scores
