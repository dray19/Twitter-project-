library("SnowballC")
library("tm")
library("twitteR")
library("syuzhet")
library("devtools")
library(tidyverse)
library("rtweet")
library(tidytext)
library(igraph)
library(ggraph)
library(janeaustenr)
library(broom)
library(proustr)
library(plotrix)
library(sentimentr)
library(SentimentAnalysis)
library(RSentiment)
library(gridExtra)
library(ROAuth)
library(lubridate)
library(grid)
#################################### Get data off twitter #######################################
########## Connect to API
create_token(
  app = "Drazen_Zack_first_app",
  consumer_key = 'Wu7ANSdIZ9yKzBNgTIDPkGlVP',
  consumer_secret = 'wZxM2r2BONOb4YiVWKiYeCnjpNoUNlgRBjvYMuLxEoRts42SQ7',
  access_token = '505322834-rTq1H3dT1Pj0Tvu01QkZCU19JIwAif0BPKrE1XOd',
  access_secret = 'F5fj8Kh8t3KNrPf9EpJ8oerfRsI9zPREvDSyAksNZd5f5')
################################
##############################################################################################################################################################
################# Load saved tweets
setwd("~/Desktop/tweets")
all_tweets_war <- read_csv("warren_09.csv")
all_tweets_kh <-  read_csv("kh_09.csv")
all_tweets_bs <- read_csv("bs_09.csv")
all_tweets_book <-read_csv("book_09.csv")
all_tweets_klob <- read_csv("klob_09.csv")
################### Place tweets into Corpus
cor_kh <- VCorpus(VectorSource(all_tweets_kh$text))
cor_bs <- VCorpus(VectorSource(all_tweets_bs$text))
cor_klob <- VCorpus(VectorSource(all_tweets_klob$text))
cor_book <- VCorpus(VectorSource(all_tweets_book$text))
cor_war <- VCorpus(VectorSource(all_tweets_war$text))
################# Clean Tweets
clean_corpus_war <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, 
                   c(stopwords("en"), "amp", "elizabeth", "warren",  "senwarren"))
  return(corpus)
}
clean_corpus_bs <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, 
                   c(stopwords("en"),"amp", "bernie","sanders", "berniesanders"))
  return(corpus)
}
clean_corpus_book <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, 
                   c(stopwords("en"),"amp" ,"corybooker","booker", "cory"))
  return(corpus)
}
clean_corpus_kh <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, 
                   c(stopwords("en"),"amp", "kamalaharris","kamala","harris"))
  return(corpus)
}
clean_corpus_klob <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, 
                   c(stopwords("en"), "amp","amyklobuchar","amy","klobuchar" ))
  return(corpus)
}

clean_cor_kh = clean_corpus_kh(cor_kh)
clean_cor_bs = clean_corpus_bs(cor_bs)
clean_cor_klob = clean_corpus_klob(cor_klob)
clean_cor_book = clean_corpus_book(cor_book)
clean_cor_war = clean_corpus_war(cor_war)
######################### Create Term Matrix
CreateTermsMatrix <- function(x) {
  x <- TermDocumentMatrix(x)
  x <- as.matrix(x)
  y <- rowSums(x)
  y <- sort(y, decreasing=TRUE)
  return(y)
}
corkh <- CreateTermsMatrix(clean_cor_kh)
corbs <- CreateTermsMatrix(clean_cor_bs)
corklob <- CreateTermsMatrix(clean_cor_klob)
corbook <- CreateTermsMatrix(clean_cor_book)
corwar <- CreateTermsMatrix(clean_cor_war)
###################################### Turn in to DataFrame
df_kh <- data.frame(word=names(corkh), count=corkh)
df_bs <- data.frame(word=names(corbs), count=corbs)
df_klob <- data.frame(word=names(corklob), count=corklob)
df_book <- data.frame(word=names(corbook), count=corbook)
df_war <- data.frame(word=names(corwar), count=corwar)
#######################
df_kh$name = "Kamala Harris"
df_bs$name = "Bernie Sanders"
df_klob$name = "Amy Klobuchar"
df_book$name = "Cory Booker" 
df_war$name = "Elizabeth Warren"
df_all <- rbind(df_book, df_klob, df_bs, df_kh, df_war)
names = unique(df_all$name)
names
top_words <- function(x){
  w <- filter(df_all, name == x)
  w[1:20,] %>%
    ggplot(aes(x=(reorder(word, count)), y=count)) +
    geom_bar(stat='identity', fill="lightblue", col = "black") + coord_flip() + theme(legend.position = "none") +
    labs(x="", title = paste0(x)) + theme(plot.title = element_text(hjust = 0.5, face = "bold")) + 
    theme(text = element_text(size = 20))
}
top_words2 <- function(x){
  w <- filter(df_all, name == x)
  w[1:20,] %>%
    ggplot(aes(x=(reorder(word, count)), y=count)) +
    geom_bar(stat='identity', fill="coral2", col = "black") + coord_flip() + theme(legend.position = "none") +
    labs(x="", title = paste0(x)) + theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme(text = element_text(size = 20))
}
########################################
######################################## Use tidy to create Bigrams
kh_tidy <- tidy(clean_cor_kh)
bs_tidy <- tidy(clean_cor_bs)
book_tidy <- tidy(clean_cor_book)
war_tidy <- tidy(clean_cor_war)
klob_tidy <- tidy(clean_cor_klob)
kh_tidy$name = "Kamala Harris"
bs_tidy$name = "Bernie Sanders"
book_tidy$name = "Cory Booker"
war_tidy$name = "Elizabeth Warren"
klob_tidy$name = "Amy Klobuchar"
df_all2 <- rbind(kh_tidy, bs_tidy, klob_tidy, book_tidy, war_tidy)
############################################## Plot Bigrams
plotBigrams <- function(tibble, topN=15, title="", color=color){
  x <- tibble %>% select(text) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2)
  y <- x %>% count(bigram, sort = TRUE) %>% top_n(topN, wt=n) %>%
    ggplot(aes(x=reorder(bigram, n), y=n)) +
    geom_bar(stat='identity', fill=color, col = "black") + coord_flip() +
    theme(legend.position="none") + labs(x="", y="Count", title=title) + theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme(text = element_text(size = 20))
  return(y)
}
bio_top <- function(x){
  d <- filter(df_all2, name == x)
  plotBigrams(d, title = x, color = "springgreen3")
}
bio_top2 <- function(x){
  d <- filter(df_all2, name == x)
  plotBigrams(d, title = x, color = "cyan3")
}
#############################################
########## Count of negative and postive used to describe candidate
NoNamesTidy <- bind_rows(harris=kh_tidy, sanders=bs_tidy, booker=book_tidy, warren = war_tidy,klobuchar = klob_tidy,  .id="candidate")
words <- NoNamesTidy %>% unnest_tokens(word, text)
dropwords <- c("trump", "like")
sent <- get_sentiments("bing") %>% filter(!word %in% dropwords)
Bing <- words %>% inner_join(sent, by  = "word")
################################################
####### Sentiment of Words Used for Tweets about Candidate
dropwords <- c("trump")
sent_nrc <- get_sentiments("nrc") %>% filter(!word %in% dropwords)
nrc <- words %>% inner_join(sent_nrc, by = "word")
#################################################################################################################################################
####################### Analyzing  the candidates personal tweets  ########################################################
##### get tweets from API
tm <- get_timelines(c("KamalaHarris", "SenSanders", "SenWarren", "CoryBooker", "amyklobuchar"), n = 1500)
###### Put Data in to list by screen name
all <- split(tm, tm$screen_name)
########## get all tweets from Dec 1 on
start_date <- function(x){
  w <- x %>%
    filter(created_at >= "2018-11-01") %>%
    filter(created_at <= "2019-03-31")
  return(w)
}
tml <- lapply(all, start_date)
###########################################
#### Number of tweets by Month
df_all3 <- do.call("rbind", tml)
post <- c("Nov","Dec", "Jan", "Feb", "Mar")
by_month <- function(w){
  p <- ggplot(data = w, aes(x = month(created_at, label = TRUE))) +
    geom_bar(aes(fill = ..count..)) +
    theme(legend.position = "none") +
    xlab("Month") + ylab("Number of tweets") + ggtitle(paste0("Number of Tweets from  @",w$screen_name)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_gradient(low = "red", high = "navy") + scale_x_discrete(limits = post) +
    theme(text = element_text(size = 20))
  return(p)
}
index <- c("amyklobuchar", "CoryBooker"  , "KamalaHarris" ,"SenSanders"  , "SenWarren")
values <- c("Amy Klobuchar", "Cory Booker" , "Kamala Harris", "Bernie Sanders", "Elizabeth Warren")
df_all3$name <- values[match(df_all3$screen_name, index)]
plot_count <- function(x){
  k <- filter(df_all3, name == x)
  by_month(k)
}
###########################################
######### Number tweets by day of the week
by_wday <- function(w){
  p <-ggplot(data = w, aes(x = wday(created_at, label = TRUE))) +
    geom_bar(aes(fill = ..count..)) +
    theme(legend.position = "none") +
    xlab("Day of the Week") + ylab("Number of tweets") + ggtitle(paste0("Number of Tweets from  @",w$screen_name)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_gradient(low = "lightblue", high = "navy") + theme(text = element_text(size = 20))
  return(p)
}
plot_wday <- function(x){
  k <- filter(df_all3, name == x)
  by_wday(k)
}
################################################
######### Sentiment of timeline
sent_func <- function(x){
  x$clean_text <- str_replace_all(x$text, "@\\w+", "")
  sent <- get_nrc_sentiment(x$clean_text)
  all_sent <- cbind(x, sent)
  sent_totals <- data.frame(colSums(all_sent[,c(90:97)]))
  names(sent_totals) <- "count"
  sent_totals <- cbind("sentiment" = rownames(sent_totals), sent_totals)
  rownames(sent_totals) <- NULL
  
  g <- ggplot(data = sent_totals, aes(x = sentiment, y = count)) +
    geom_bar(aes(fill = sentiment), stat = "identity") +
    theme(legend.position = "none") +
    xlab("Sentiment") + ylab("Total Count") + ggtitle(paste0("Total Sentiment Score for All Tweets by  @",x$screen_name)) +
    theme(plot.title =  element_text(hjust = 0.5)) + theme(text = element_text(size = 20))
  
  time <- all_sent %>% 
    group_by(created = cut(created_at, breaks="2 days")) %>%
    summarise(negative = mean(negative),
              positive = mean(positive)) %>% 
    reshape2::melt()
  
  names(time) <- c("timestamp", "sentiment", "meanvalue")
  time$sentiment = factor(time$sentiment,levels(time$sentiment)[c(2,1)])
  
  pn <- ggplot(data = time, aes(x = as.Date(timestamp), y = meanvalue, group = sentiment)) +
    geom_line(size = 2, alpha = 0.7, aes(color = sentiment)) +
    geom_point(size = 1) +
    ylim(0, NA) + 
    scale_colour_manual(values = c("springgreen3", "firebrick1")) +
    theme(legend.title=element_blank(), axis.title.x = element_blank()) +
    scale_x_date(breaks = scales::date_breaks("4 week"), 
                 labels = scales::date_format("%Y-%m-%d")) +
    ylab("Average sentiment score") + 
    ggtitle(paste0("Sentiment Over Time for @", x$screen_name)) +
    theme(plot.title =  element_text(hjust = 0.5)) + theme(text = element_text(size = 20))
  
  return(list(total_sent = g, PN = pn))
}
tml_sent <- lapply(tml, sent_func)
#################################################
######### Biograms of candidates tweets
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, 
                   c(stopwords("en"), "amp","s time"))
  return(corpus)
}
bio_2 <- function(x,col){
  all_cor <- VCorpus(VectorSource(x$text))
  all_clean <- clean_corpus(all_cor)
  tidy <- tidy(all_clean)
  return(tidy)
}
cor_plot <- function(x){
  k <- filter(df_all3, name == x)
  c <- bio_2(k)
  plotBigrams(c, title = x, color = "lightblue")
}
cor_plot2 <- function(x){
  k <- filter(df_all3, name == x)
  c <- bio_2(k)
  plotBigrams(c, title = x, color = "lightcoral")
}
