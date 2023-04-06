##############
##          ##
## Slide 27 ##
##          ##̥
##############

# In the example slides we often load a package with the library()-
# command. For this to work, you must install the package first. For 
# e.g. the ‘RedditExtractoR’- package, you do this as follows (one-
# off):

# install.packages("RedditExtractoR", dependencies = TRUE)

##############
##          ##
## Slide 28 ##̥
##          ##
##############

# The working directory is the directory from which R reads files
# and to which R writes files. Set your working directory using the 
# setwd() function. Important: always use forward slashes!

setwd("C:/Users/darsh/Desktop") # MacOS
# setwd("C:/My Documents") # Windows
getwd() # check current working directory̥̥

##############
##          ##
## Slide 29 ##
##          ##̥
##############

library(RedditExtractoR)

if(file.exists("reddit_data.RData")) {
    load("reddit_data.RData")
} else {
    # create vector with brands
    brands = c("GoogleHome", "Netatmo")
    
    # retrieve relevant thread urls
    for (i in seq(length(brands))) {
        print(paste("Extracting threads for", brands[i], sep = " "))
        if (i == 1) {
            thread_urls <- find_thread_urls(keywords = NA,
                                            sort_by = "top",
                                            subreddit = brands[i],
                                            period = "all")
        } else {
            thread_urls <- rbind(thread_urls, find_thread_urls(keywords = NA,
                                                               sort_by = "top",
                                                               subreddit = brands[i],
                                                               period = "all"))
        }
    }
    
    # deal with NA-values
    thread_urls <- thread_urls[ !is.na(thread_urls$url), ]
    
    # retrieve contents of each thread (this might take some time, run overnight)
    thread_contents <- get_thread_content(thread_urls$url)
    
    # write to disk
    save(thread_contents, thread_urls,file =  "reddit_data.RData")
}

##############
##          ##
## Slide 30 ##
##          ##
##############

# assign unique id
thread_contents$comments$unique_id <- paste(thread_contents[2]$comments$timestamp, 
                                            thread_contents[2]$comments$comment_id, 
                                            sep = "_")

##############
##          ##
## Slide 31 ##
##          ##
##############

# transferring thread_contents$threads$subreddit (brand information) to thread_contents$comments
thread_contents$comments <- merge(thread_contents$comments, thread_contents$threads[, c(1, 7) ], all.x = TRUE)

##############
##          ##
## Slide 32 ##
##          ##
##############

# convert date field to date object
thread_contents$comments$date <- as.Date(thread_contents$comments$date,
                                         "%Y-%m-%d")
str(thread_contents$comments$date)

##############
##          ##
## Slide 33 ##
##          ##
##############

# adding year data
thread_contents$comments$year <- as.numeric(format(thread_contents$comments$date, "%Y"))

# adding seasons data
thread_contents$comments$season <- as.numeric(format(thread_contents$comments$date, "%m"))
thread_contents$comments$season <- ifelse(thread_contents$comments$season %in% c(1:2, 12), "winter",
                                    ifelse(thread_contents$comments$season %in% c(3:5), "spring",
                                           ifelse(thread_contents$comments$season %in% c(6:8), "summer", "fall")))

##############
##          ##
## Slide 34 ##
##          ##
##############

# adding poster activity
poster_activity <- setNames(data.frame(table(thread_contents$comments$author)),
                            c("author", "poster_activity"))
thread_contents$comments <- merge(thread_contents$comments, poster_activity)
remove(poster_activity)

##############
##          ##
## Slide 35 ##
##          ##
##############

library(quanteda)

# creating the corpus
corpus <- corpus(thread_contents[2]$comments, 
                 docid_field = "unique_id",
                 text_field = "comment")

# see the first 10 documents:
summary(corpus, 10)

# randomly select a document:
as.character(corpus[ sample(length(corpus), 1) ])

##############
##          ##
## Slide 36 ##
##          ##
##############

library(cld2)

# language check using Google’s Compact Language Detector 2
docvars(corpus, field = "language") <- detect_language(corpus)

# filter corpus
corpus_subset(corpus, corpus$language == "en")

##############
##          ##
## Slide 37 ##
##          ##
##############

# add average sentiment score (takes a couple of minutes)

library(sentimentr)
docvars(corpus, field = "ave_sentiment") <- sentiment_by(get_sentences(corpus))$ave_sentiment

# explore sentiment scores (adjust thresholds as you see fit)
as.character(corpus[ which(docvars(corpus)$ave_sentiment < -1.5), ])
as.character(corpus[ which(docvars(corpus)$ave_sentiment > 1), ])

##############
##          ##
## Slide 39 ##
##          ##
##############

# tokenize (type ?tokens for more information)
tokens <- tokens(corpus_subset(corpus, corpus$language == "en" & corpus$author != "[deleted]"),
                 what = "word",
                 remove_punct = TRUE,
                 remove_symbols = TRUE,
                 remove_numbers = TRUE,
                 remove_url = TRUE,
                 remove_separators = TRUE,
                 split_hyphens = FALSE,
                 include_docvars = TRUE,
                 padding = FALSE,
                 verbose = TRUE)
tokens <- tokens_tolower(tokens) # remove uppercase letters

##############
##          ##
## Slide 40 ##
##          ##
##############

# explore stopwords
stopwords("en")

# remove stopwords
tokens <- tokens_remove(tokens, 
                        pattern = stopwords("en"))

##############
##          ##
## Slide 41 ##
##          ##
##############

# create the dfm
dfm <- dfm(tokens)

# inspect the dfm
dfm

##############
##          ##
## Slide 42 ##
##          ##
##############

# apply lower cut-off Actual cut-offs are determined by trial and 
# error and often depend on the research context

dfm <- dfm_trim(dfm, min_termfreq = 2)

# inspect the dfm
dfm

##############
##          ##
## Slide 43 ##
##          ##
##############

# converting dfm to a tidy format
library(tidytext)
dfm <- data.frame(tidy(dfm))

##############
##          ##
## Slide 44 ##
##          ##
##############

# using the nrc sentiment and emotion lexicon to make a cross section
nrc_sentiments <-  data.frame(get_sentiments("nrc")) # press 1 if asked for
nrc_sentiments[ sample(nrow(nrc_sentiments), 10), ]

sentiments_nrc <- merge(dfm, nrc_sentiments, by.x = "term", by.y = "word")
sentiments_nrc <- merge(sentiments_nrc, summary(corpus, n = length(corpus)), by.x = "document", by.y = "Text")
remove(nrc_sentiments)

##############
##          ##.
## Slide 45 ##
##          ##
##############

# chi square test: dependence between sentiments and brands?
tbl <- table(sentiments_nrc$sentiment, sentiments_nrc$subreddit)
tbl

# chi square test
chisq.test(tbl)
tbl - chisq.test(tbl)$expected

remove(sentiments_nrc, tbl)

##############
##          ##
## Slide 46 ##
##          ##
##############

# using the bing sentiment lexicon to make a cross section and perform a t-test
# difference in number of upvotes between positive and negative sentiments?
bing_sentiments <-  data.frame(get_sentiments("bing"))
bing_sentiments[ sample(nrow(bing_sentiments), 10), ]

sentiments_bing <- merge(dfm, bing_sentiments, by.x = "term", by.y = "word")
sentiments_bing <- merge(sentiments_bing, summary(corpus, n = length(corpus)), by.x = "document", by.y = "Text")
remove(bing_sentiments)

# t-test
t.test(sentiments_bing$upvotes ~ sentiments_bing$sentiment, paired = FALSE, var.equal = FALSE) # assumption of unequal variances
remove(sentiments_bing)

##############
##          ##
## Slide 47 ##
##          ##
##############

# ANOVA: difference in average sentiment between brands?
sentiments_average <- merge(dfm, summary(corpus, n = length(corpus)), by.x = "document", by.y = "Text")
aggregate(sentiments_average$ave_sentiment, by = list("brand" = sentiments_average$subreddit), FUN = mean)

anova <- aov(sentiments_average$ave_sentiment ~ sentiments_average$subreddit) # estimating the model
summary(anova) # getting the output of the model
remove(anova)

##############
##          ##
## Slide 48 ##
##          ##
##############

# Linear regression: predicting average sentiment score of a review
regression <- lm(ave_sentiment ~ poster_activity + season,
                 data = sentiments_average) # estimating the model
summary(regression) # getting the output of the model
remove(regression)