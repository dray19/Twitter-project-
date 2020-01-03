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
library(gridExtra)
library(lubridate)
library(grid)
#################################### Get data off twitter #######################################
########## Connect to API
create_token(
  app = "Drazen_Zack_first_app",
  consumer_key = '',
  consumer_secret = '',
  access_token = '',
  access_secret = '')
################################
#### Function to remove duplicated tweets
dup_func <- function(x){
  y <- x[!duplicated(x[,"text"]),]
  return(y)
}
############################ Tweets about the candidate by states ##############################
### This part was only done when gathering tweets
### Dont need to run if not gathering tweets from API
################## Kamala Harris 
df_state <- read_csv("long_&_lat.csv")
df_state
state_tweets <- vector("list", 50)
for (i in seq_along(state_tweets)) {
  state_tweets[[i]] <- search_tweets(
    paste0("kamala harris OR kamalaharris geocode:",
           df_state$y[i], ",",
           df_state$x[i], ",",
           df_state$dist[i],"mi"), n = 200
  )
}
names(state_tweets) <- rownames(state.x77)
####### Count tweets by state
count_text <- function(x){
  lw <- length(x$text)
  return(lw)
}
kh <- bind_rows(state_tweets, .id = "state_label")
kh <- data.frame(kh)
head(kh)
kh_new <- apply(kh,2,as.character)
########## Load old tweets
setwd("~/Desktop/tweets")
kh_old <- read_csv("kh_06.csv")
kh_old <- as.data.frame(kh_old)
kh_old$X1 <- NULL
############ Combine New and Old tweets
kh2 <- rbind(kh_old, kh_new)
dim(kh2)
df_kh <- split(kh2, kh2$state_label)
kh_list <-  lapply(df_kh, dup_func)
kh <-  bind_rows(kh_list, .id = "state_label_new")
kh$state_label_new <- NULL
kh <-  as.data.frame(kh)
##### save tweets
setwd("~/Desktop")
write.csv(kh, file = "kh_09.csv")
########################## sanders ##########################
state_tweets_bs <- vector("list", 50)
for (i in seq_along(state_tweets_bs)) {
  state_tweets_bs[[i]] <- search_tweets(
    paste0("bernie sanders OR sensanders geocode:",
           df_state$y[i], ",",
           df_state$x[i], ",",
           df_state$dist[i],"mi"), n = 200
  )
}
names(state_tweets_bs) <- rownames(state.x77)
bs <- bind_rows(state_tweets_bs, .id = "state_label")
bs <- data.frame(bs)
bs_new <- apply(bs,2,as.character)
########## Load old tweets
setwd("~/Desktop/tweets")
bs_old <- read_csv("bs_06.csv")
head(bs_old)
bs_old <- as.data.frame(bs_old)
bs_old$X1 <- NULL
############  Combine New and Old tweets
bs2 <- rbind(bs_old, bs_new)
df_bs <- split(bs2, bs2$state_label)
bs_list <-  lapply(df_bs, dup_func)
lapply(bs_list, count_text)
bs <-  bind_rows(bs_list, .id = "state_label_new")
bs$state_label_new <- NULL
bs <-  as.data.frame(bs)
##### save tweets
setwd("~/Desktop")
write.csv(bs, file = "bs_09.csv")
############################### warren ######################
state_tweets_w <- vector("list", 50)
for (i in seq_along(state_tweets_w)) {
  state_tweets_w[[i]] <- search_tweets(
    paste0("elizabeth warren OR senwarren geocode:",
           df_state$y[i], ",",
           df_state$x[i], ",",
           df_state$dist[i],"mi"), n = 200
  )
}
names(state_tweets_w) <- rownames(state.x77)
war <- bind_rows(state_tweets_w, .id = "state_label")
war <- data.frame(war)
war_new <- apply(war,2,as.character)
########## Load old tweets
setwd("~/Desktop/tweets")
war_old <- read_csv("warren_06.csv")
head(war_old)
war_old <- as.data.frame(war_old)
war_old$X1 <- NULL
w <- split(war_old, war_old$state_label)
lapply(w, count_text)
########################  Combine New and Old tweets
war2 <- rbind(war_old, war_new)
df_war <- split(war2, war2$state_label)
war_list <-  lapply(df_war, dup_func)
war <-  bind_rows(war_list, .id = "state_label_new")
war$state_label_new <- NULL
war <-  as.data.frame(war)
### save tweets
setwd("~/Desktop")
write.csv(war, file = "warren_09.csv")
##################### Booker #####################
state_tweets_book <- vector("list", 50)
for (i in seq_along(state_tweets_book)) {
  state_tweets_book[[i]] <- search_tweets(
    paste0("cory booker OR corybooker geocode:",
           df_state$y[i], ",",
           df_state$x[i], ",",
           df_state$dist[i],"mi"), n = 200
  )
}
names(state_tweets_book) <- rownames(state.x77)
book <- bind_rows(state_tweets_book, .id = "state_label")
book <- data.frame(book)
book_new <- apply(book,2,as.character)
########## Load old tweets
setwd("~/Desktop/tweets")
book_old <- read_csv("book_06.csv")
book_old <- as.data.frame(book_old)
book_old$X1 <- NULL
############ Combine New and Old tweets
book2 <- rbind(book_old, book_new)
df_book <- split(book2, book2$state_label)
book_list <-  lapply(df_book, dup_func)
book <-  bind_rows(book_list, .id = "state_label_new")
book$state_label_new <- NULL
book <-  as.data.frame(book)
#### save tweets
setwd("~/Desktop")
write.csv(book, file = "book_09.csv")
############################## klobuchar ##################
state_tweets_k <- vector("list", 50)
for (i in seq_along(state_tweets_k)) {
  state_tweets_k[[i]] <- search_tweets(
    paste0("amy klobuchar OR amyklobuchar geocode:",
           df_state$y[i], ",",
           df_state$x[i], ",",
           df_state$dist[i],"mi"), n = 200
  )
}
names(state_tweets_k) <- rownames(state.x77)
klob <- bind_rows(state_tweets_k, .id = "state_label")
klob <- data.frame(klob)
klob_new <- apply(klob,2,as.character)
########## Load old tweets
setwd("~/Desktop/tweets")
klob_old <- read_csv("klob_06.csv")
head(klob_old)
klob_old <- as.data.frame(klob_old)
klob_old$X1 <- NULL
###################### Combine New and Old tweets
klob2 <- rbind(klob_old, klob_new)
df_klob <- split(klob2, klob2$state_label)
klob_list <-  lapply(df_klob, dup_func)
klob <-  bind_rows(klob_list, .id = "state_label_new")
klob$state_label_new <- NULL
klob<-  as.data.frame(klob)
##### save tweets
setwd("~/Desktop")
write.csv(klob, file = "klob_09.csv")
###################################################### Start of analyzed data ##########################################################################
################# Load saved tweets gather tweets
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
################# Clean Tweets Functions
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
################################################### analysis begins #######################################
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
########################################
######## Plot top used words 
c1 <- df_kh[1:20,] %>%
  ggplot(aes(x=(reorder(word, count)), y=count)) +
  geom_bar(stat='identity', fill="lightblue", col = "black") + coord_flip() + theme(legend.position = "none") +
  labs(x="", title = "Kamala Harris") + theme(plot.title = element_text(hjust = 0.5, face = "bold")) 

c2 <- df_bs[1:20,] %>%
  ggplot(aes(x=(reorder(word, count)), y=count)) +
  geom_bar(stat='identity', fill="red", col = "black") + coord_flip() + theme(legend.position = "none") +
  labs(x="", title = "Bernie Sanders") + theme(plot.title = element_text(hjust = 0.5, face = "bold")) 

c3 <- df_klob[1:20,] %>%
  ggplot(aes(x=(reorder(word, count)), y=count)) +
  geom_bar(stat='identity', fill="springgreen2", col = "black") + coord_flip() + theme(legend.position = "none") +
  labs(x="", title ="Amy Klobuchar" ) + theme(plot.title = element_text(hjust = 0.5, face = "bold")) 

c4 <- df_book[1:20,] %>%
  ggplot(aes(x=(reorder(word, count)), y=count)) +
  geom_bar(stat='identity', fill="coral2", col = "black") + coord_flip() + theme(legend.position = "none") +
  labs(x="", title ="Cory Booker" ) + theme(plot.title = element_text(hjust = 0.5, face = "bold")) 

c5 <- df_war[1:20,] %>%
  ggplot(aes(x=(reorder(word, count)), y=count)) +
  geom_bar(stat='identity', fill="orange2", col = "black") + coord_flip() + theme(legend.position = "none") +
  labs(x="", title ="Elizabeth Warren") + theme(plot.title = element_text(hjust = 0.5, face = "bold")) 

grid.arrange(c1,c2, c3, c4, c5,top = textGrob("Top Words Used for Tweets about Candidate", gp = gpar(fontsize = 20, face = "bold")))
######################################## Use tidy to create Biograms
kh_tidy <- tidy(clean_cor_kh)
bs_tidy <- tidy(clean_cor_bs)
book_tidy <- tidy(clean_cor_book)
war_tidy <- tidy(clean_cor_war)
klob_tidy <- tidy(clean_cor_klob)
############################################## Plot Bigrams
plotBigrams <- function(tibble, topN=15, title="", color=color){
  x <- tibble %>% select(text) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2)
  y <- x %>% count(bigram, sort = TRUE) %>% top_n(topN, wt=n) %>%
    ggplot(aes(x=reorder(bigram, n), y=n)) +
    geom_bar(stat='identity', fill=color, col = "black") + coord_flip() +
    theme(legend.position="none") + labs(x="", y="Count", title=title) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  return(y)
}
t1 <- plotBigrams(kh_tidy, title = "Kamala Harris",color = "lightblue")
t2 <- plotBigrams(bs_tidy, title = "Bernie Sanders", color = "red")
t3 <- plotBigrams(book_tidy, title = "Cory Booker", color = "coral2")
t4 <- plotBigrams(war_tidy, title = "Elizabeth Warren", color = "orange2")
t5 <- plotBigrams(klob_tidy, title = "Amy Klobuchar", color = "springgreen")

grid.arrange(t1,t2,t3,t4,t5, top = textGrob("Top Bigrams Used for Tweets about Candidate", gp = gpar(fontsize = 20, face = "bold")))
#############################################
########## Count of negative and postive used to describe candidate
NoNamesTidy <- bind_rows(harris=kh_tidy, sanders=bs_tidy, booker=book_tidy, warren = war_tidy,klobuchar = klob_tidy,  .id="candidate")
words <- NoNamesTidy %>% unnest_tokens(word, text)
dropwords <- c("trump", "like")
sent <- get_sentiments("bing") %>% filter(!word %in% dropwords)
Bing <- words %>% inner_join(sent, by  = "word")

b1 <- Bing %>% filter(candidate=="harris") %>% count(word, sentiment, sort=TRUE) %>%
  group_by(sentiment) %>% arrange(desc(n)) %>% slice(1:20) %>%
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  coord_flip() +
  facet_wrap(~sentiment, scales="free_y") +
  labs(x="", y="Number of Times Used", title="Harris Postive & Negative Words") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_fill_manual(values = c("positive"="green", "negative"="red"))

b2 <- Bing %>% filter(candidate=="sanders") %>% count(word, sentiment, sort=TRUE) %>%
  group_by(sentiment) %>% arrange(desc(n)) %>% slice(1:20) %>%
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  coord_flip() +
  facet_wrap(~sentiment, scales="free_y") +
  labs(x="", y="Number of Times Used", title="Sanders Postive & Negative Words") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_fill_manual(values = c("positive"="green", "negative"="red"))

b3 <- Bing %>% filter(candidate=="booker") %>% count(word, sentiment, sort=TRUE) %>%
  group_by(sentiment) %>% arrange(desc(n)) %>% slice(1:20) %>%
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  coord_flip() +
  facet_wrap(~sentiment, scales="free_y") +
  labs(x="", y="Number of Times Used", title="Booker Postive & Negative Words") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_fill_manual(values = c("positive"="green", "negative"="red"))
b4 <- Bing %>% filter(candidate=="warren") %>% count(word, sentiment, sort=TRUE) %>%
  group_by(sentiment) %>% arrange(desc(n)) %>% slice(1:20) %>%
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  coord_flip() +
  facet_wrap(~sentiment, scales="free_y") +
  labs(x="", y="Number of Times Used", title="Warren Postive & Negative Words") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_fill_manual(values = c("positive"="green", "negative"="red"))

b5 <- Bing %>% filter(candidate=="klobuchar") %>% count(word, sentiment, sort=TRUE) %>%
  group_by(sentiment) %>% arrange(desc(n)) %>% slice(1:20) %>%
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  coord_flip() +
  facet_wrap(~sentiment, scales="free_y") +
  labs(x="", y="Number of Times Used", title="Klobuchar  Postive & Negative Words") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_fill_manual(values = c("positive"="green", "negative"="red"))

grid.arrange(b1, b2, b3, b4,b5,  top = textGrob("Top Negative and Postive Words Used for Tweets about Candidate", gp = gpar(fontsize = 20, face = "bold")))
################################################
####### Sentiment of Words Used for Tweets about Candidate
dropwords <- c("trump")
sent_nrc <- get_sentiments("nrc") %>% filter(!word %in% dropwords)
nrc <- words %>% inner_join(sent_nrc, by = "word")
n1 <- nrc %>% filter(candidate=="harris") %>% count(sentiment) %>%
  ggplot(aes(x=sentiment, y=n, fill=sentiment)) +
  geom_bar(stat="identity") + coord_polar() +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  geom_text(aes(label=sentiment, y=13000)) + 
  scale_y_continuous(limits = c(0,20000), breaks = c(0,4000,8000,12000,16000, 20000)) +
  labs(x="", y="", title="Kamala Harris") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

n2 <- nrc %>% filter(candidate=="sanders") %>% count(sentiment) %>%
  ggplot(aes(x=sentiment, y=n, fill=sentiment)) +
  geom_bar(stat="identity") + coord_polar() +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  geom_text(aes(label=sentiment, y=13000)) + 
  scale_y_continuous(limits = c(0,20000), breaks = c(0,4000,8000,12000,16000, 20000)) +
  labs(x="", y="", title="Bernie Sanders") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

n3 <- nrc %>% filter(candidate=="booker") %>% count(sentiment) %>%
  ggplot(aes(x=sentiment, y=n, fill=sentiment)) +
  geom_bar(stat="identity") + coord_polar() +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  geom_text(aes(label=sentiment, y=13000)) + 
  scale_y_continuous(limits = c(0,20000), breaks = c(0,4000,8000,12000,16000, 20000)) +
  labs(x="", y="", title="Cory Booker") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

n4 <- nrc %>% filter(candidate=="warren") %>% count(sentiment) %>%
  ggplot(aes(x=sentiment, y=n, fill=sentiment)) +
  geom_bar(stat="identity") + coord_polar() +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  geom_text(aes(label=sentiment, y=13000)) + 
  scale_y_continuous(limits = c(0,20000), breaks = c(0,4000,8000,12000,16000, 20000)) +
  labs(x="", y="", title="Elizabeth Warren") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

n5 <- nrc %>% filter(candidate=="klobuchar") %>% count(sentiment) %>%
  ggplot(aes(x=sentiment, y=n, fill=sentiment)) +
  geom_bar(stat="identity") + coord_polar() +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  geom_text(aes(label=sentiment, y=13000)) + 
  scale_y_continuous(limits = c(0,20000), breaks = c(0,4000,8000,12000,16000, 20000)) +
  labs(x="", y="", title="Amy Klobuchar") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

grid.arrange(n1,n2,n3,n4,n5,top = textGrob("Top Sentiment of Words Used for Tweets about Candidate", gp = gpar(fontsize = 20, face = "bold")))
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
post <- c("Nov","Dec", "Jan", "Feb", "Mar")
by_month <- function(w){
  p <- ggplot(data = w, aes(x = month(created_at, label = TRUE))) +
    geom_bar(aes(fill = ..count..)) +
    theme(legend.position = "none") +
    xlab("Month") + ylab("Number of tweets") + ggtitle(paste0("Number of Tweets from  @",w$screen_name)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_gradient(low = "red", high = "navy") + scale_x_discrete(limits = post) 
  return(p)
}
tml_month <- lapply(tml, by_month)
grid.arrange(tml_month$amyklobuchar, tml_month$CoryBooker, tml_month$KamalaHarris, tml_month$SenSanders, tml_month$SenWarren,
             top = textGrob("Number of Tweets by Month", gp = gpar(fontsize = 20, face = "bold")))
###########################################
######### Number tweets by day of the week
by_wday <- function(w){
  p <-ggplot(data = w, aes(x = wday(created_at, label = TRUE))) +
    geom_bar(aes(fill = ..count..)) +
    theme(legend.position = "none") +
    xlab("Day of the Week") + ylab("Number of tweets") + ggtitle(paste0("Number of Tweets from  @",w$screen_name)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_gradient(low = "lightblue", high = "navy")
  return(p)
}
wday_tml <- lapply(tml, by_wday)
grid.arrange(wday_tml$amyklobuchar, wday_tml$CoryBooker, wday_tml$KamalaHarris, wday_tml$SenSanders, wday_tml$SenWarren,
             top = textGrob("Number of Tweets by Day of Week", gp = gpar(fontsize = 20, face = "bold")))
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
    theme(plot.title =  element_text(hjust = 0.5))
  
  time <- all_sent %>% 
    group_by(created = cut(created_at, breaks="2 days")) %>%
    summarise(negative = mean(negative),
              positive = mean(positive)) %>% 
    reshape2::melt()
  
  names(time) <- c("timestamp", "sentiment", "meanvalue")
  time$sentiment = factor(time$sentiment,levels(time$sentiment)[c(2,1)])
  
  pn <- ggplot(data = time, aes(x = as.Date(timestamp), y = meanvalue, group = sentiment)) +
    geom_line(size = 1.5, alpha = 0.7, aes(color = sentiment)) +
    geom_point(size = 0.3) +
    ylim(0, NA) + 
    scale_colour_manual(values = c("springgreen3", "firebrick1")) +
    theme(legend.title=element_blank(), axis.title.x = element_blank()) +
    scale_x_date(breaks = scales::date_breaks("4 week"), 
                 labels = scales::date_format("%Y-%m-%d")) +
    ylab("Average sentiment score") + 
    ggtitle(paste0("Sentiment Over Time for @", x$screen_name)) +
    theme(plot.title =  element_text(hjust = 0.5))
  
  return(list(total_sent = g, PN = pn))
}
tml_sent <- lapply(tml, sent_func)
########### emotion sentiment ####################
grid.arrange(tml_sent$amyklobuchar$total_sent, 
             tml_sent$CoryBooker$total_sent,
             tml_sent$KamalaHarris$total_sent,
             tml_sent$SenSanders$total_sent,
             tml_sent$SenWarren$total_sent,
             top = textGrob("Emotion Sentiment Score", gp = gpar(fontsize = 20, face = "bold")))
########## Mean negative & postive words every two days per tweet
grid.arrange(tml_sent$amyklobuchar$PN,
             tml_sent$CoryBooker$PN,
             tml_sent$KamalaHarris$PN,
             tml_sent$SenSanders$PN,
             tml_sent$SenWarren$PN,
             top = textGrob("Average Sentiment Over Time ", gp = gpar(fontsize = 20, face = "bold")))
#################################################
######### Bigrams of candidates tweets
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
tml_bio <- lapply(tml,bio_2)
c1 <- plotBigrams(tml_bio$amyklobuchar, title = paste0("@",tml$amyklobuchar$screen_name), color = "lightblue")
c2 <- plotBigrams(tml_bio$CoryBooker, title = paste0("@",tml$CoryBooker$screen_name), color = "red")
c3 <- plotBigrams(tml_bio$KamalaHarris, title = paste0("@",tml$KamalaHarris$screen_name), color = "orange")
c4 <- plotBigrams(tml_bio$SenSanders, title = paste0("@",tml$SenSanders$screen_name), color = "springgreen")
c5 <- plotBigrams(tml_bio$SenWarren, title = paste0("@",tml$SenWarren$screen_name), color = "lightcoral")
grid.arrange(c1,c2,c3,c4,c5,top = textGrob("Bigrams of Candidates Tweets", gp = gpar(fontsize = 20, face = "bold")))
###############
