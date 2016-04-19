#########################################
#### TripAdvisor Scraper using rvest ####
####        + NLP ANALYSIS           ####
####        D. Stoikovitch           ####
#########################################

### 1/ DATA CLEANING AND PREPARATION ### -----------------------------
library(plyr)
library(dplyr)
library(rvest)
library(stringr)

# enter URL prefix/suffix
nrev <- 1200 #nb of reviews total for our reference
urlparam <- seq.int(0,nrev,10) # sequence of url params to attach to the URLs to take multiple pages
urlprefix <- "https://www.tripadvisor.com/Restaurant_Review-g186338-d1233703-Reviews-or"
urlsuffix <- "-Bocca_di_Lupo-London_England.html#REVIEWS"
url <- vector(mode = "character", length=length(urlparam))
for (k in 1:length(url)) {
  url[k] = paste(urlprefix,urlparam[k],urlsuffix,sep = "")  # concatenate
}

# create list of dataframes corresponding to pages scraped
dflist <- vector(mode = "list", length = length(url))

# loop through results pages and scrape
for (k in 1:length(url)) {
  
  # review bubble data
  reviews <- url[k] %>%
    read_html() %>%
    html_nodes("#REVIEWS .innerBubble")
  id <- reviews %>%
    html_node(".quote a") %>%
    html_attr("id")
  quote <- reviews %>%
    html_node(".quote span") %>%
    html_text()
  rating <- reviews %>%
    html_node(".rating .rating_s_fill") %>%
    html_attr("alt") %>%
    gsub(" of 5 stars", "", .) %>%
    as.integer()
  review <- reviews %>%
    html_node(".entry .partial_entry") %>%
    html_text()
  helpful <- reviews %>%
    html_nodes(".helpful") %>%
    html_text() %>%
    strsplit(., "Thank") %>%
    lapply(., FUN = `[`, 1) %>%
    unlist() %>%
    str_extract(., "[1-9]+")
  picture <- as.character(reviews) %>%
    str_detect(., 'class=\"media')
  
  # contributor badge data
  badges <- url[k] %>%
    read_html() %>%
    html_nodes("#REVIEWS .col1of2")
  level <- as.character(badges) %>%
    str_extract(., '(?<=levelBadge ).*?(?=")') %>%
    str_extract(., "[1-9]+")
  reviews_count <- as.character(badges) %>%
    str_extract(., '(?<=badgeText|">).*?(?=reviews)') %>%
    str_extract(., "[1-9]+")
  
  dflist[[k]] <- data.frame(id, quote, rating, helpful, picture, level, reviews_count, review, 
                            stringsAsFactors = FALSE)
  
}
  
# bind all created dataframes together in one big dataframe
df = rbind.fill(dflist)
rm(badges, dflist, helpful, id, k, level, picture, quote, rating, review, reviews, 
   reviews_count, url, urlparam, urlprefix, urlsuffix)

# clean and enrich data
df$review_words <- gsub("[[:punct:]]", "", df$review) %>% # remove punctuation
  gsub("\\d", "",.) # remove numbers
df = df %>% 
  mutate(review_full = paste(quote, review)) %>%
  mutate(review_length = nchar(gsub(" ", "", df$review_words)))

df[is.na(df)] <- 0
df$helpful = as.integer(df$helpful)
df$level = as.integer(df$level)
df$picture = as.integer(df$picture)
df$reviews_count = as.integer(df$reviews_count)

# non-neutral reviews
bad = df %>% filter(rating < 3)
good = df %>% filter(rating > 3)

## 2/ SUMMARY STATS ## -------------------------------------------------
library(ggplot2)
library(scales)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(extrafont)
source("http://peterhaschke.com/Code/multiplot.R")

summary(df)

a = ggplot(df, aes(x=as.factor(helpful))) + 
  geom_bar() + 
  ds_theme() + 
  labs(x='"Helpful" votes', y = '')
b = ggplot(df, aes(x=as.factor(level))) + geom_bar() + ds_theme() + 
  labs(x='Contributor level', y = '')
c = ggplot(df, aes(x=as.factor(picture))) + geom_bar() + ds_theme() + 
  labs(x='Picture?', y = '')
d = ggplot(df, aes(x=reviews_count)) + geom_histogram(bins = 20) + ds_theme() + 
  labs(x='Reviews already posted', y = '')
e = ggplot(df, aes(x=rating)) + geom_bar() + ds_theme() + 
  labs(x='Rating', y = '')
f = ggplot(df, aes(x=review_length)) + geom_histogram(bins=20) + ds_theme() + 
  labs(x='Review length', y = '') + scale_x_continuous(breaks=seq(100, 300, 100))

multiplot(a,e, cols=1)
multiplot(c, f, cols=1)
multiplot(b, d, cols=1)

aovlength = aov(review_length ~ rating, df)
summary(aovlength)
g = ggplot(df, aes(x=as.factor(rating), y=review_length)) + geom_boxplot(fill='lightgray') +
  scale_y_continuous(limits = c(150,300)) +
  labs(x='Rating', y = 'Review length') 
print(g)

## 3/ WORDCLOUDS ON GOOD/BAD REVIEWS ## ----------------------------------------
library(tm)
library(SnowballC)
library(wordcloud)
library(Matrix)

makeWordCloud <- function(documents) {
  
  corpus = Corpus(VectorSource(documents))
  frequencies = DocumentTermMatrix(corpus)
  word_frequencies = as.data.frame(as.matrix(frequencies))
  words <- colnames(word_frequencies)
  freq <- colSums(word_frequencies)
  wordcloud(words, freq,
            min.freq=sort(freq, decreasing=TRUE)[[100]], #first 100 words
            colors=brewer.pal(8, "Dark2"),
            random.color=TRUE,
            random.order = FALSE)
}

# Using stemming
clean_stem = function(text) {
  cleaned <- text %>%
    gsub("[[:punct:]]", "",.) %>% # remove punctuation 
    gsub("\\d", "",.) %>% # remove numbers 
    tolower(.) %>% # to lowercase
    removeWords(.,c(stopwords("english"))) %>% # remove stop words
    removeWords(., c('food', 'restaurant', 'italian', 'london')) %>% # remove highly frequent words
    stripWhitespace(.) # strip whitespaces
  
  k = length(cleaned)
  for (i in 1:k) {
    temp <- strsplit(cleaned[[i]]," ") # split the review i into words
    temp <- stemDocument(temp[[1]]) # stem the review
    cleaned[[i]]<-paste(temp,collapse=" ") # re-form it as a sentence
  }
  return(cleaned)
}

allreviews = clean_stem(df$review_full)
badreviews = clean_stem(bad$review_full)
goodreviews = clean_stem(good$review_full)
makeWordCloud(allreviews)
makeWordCloud(badreviews)
makeWordCloud(goodreviews)

## 4/ WORD CLUSTERING ANALYSIS FOR TOPICS ##-------------------------------------

# build document-term matrix
corpusbad = Corpus(VectorSource(badreviews))
dtmtemp = TermDocumentMatrix(corpusbad)

# look at most frequent terms and their most correlated terms
findFreqTerms(dtmtemp, lowfreq=10)
findAssocs(dtmtemp,"servic",0.3)
findAssocs(dtmtemp,"waiter",0.3)
findAssocs(dtmtemp,"chef",0.3)
findAssocs(dtmtemp,"expens",0.3)
findAssocs(dtmtemp,"small",0.3)

dtmfinaldf = removeSparseTerms(dtmtemp, sparse = 0.97) # remove very sparse terms in the sparse matrix
#dtmtemp = weightTfIdf(dtmtemp) # we can also apply TF*IDF weighting technique
dtmfinalmat = as.matrix(dtmtemp)

# hierarchical clustering and plot
dtmclust = dist(scale(dtmfinalmat), method="euclidean")
hcluster = hclust(dtmclust, method="ward.D")
plot(hcluster, cex=0.9, hang=-1, main="Word Cluster Dendogram")
rect.hclust(hcluster, k=14)

# using ggdendrogram package for styling
library(ggdendro)
ggdendrogram(hcluster)
ggdendrogram(hcluster, rotate = TRUE, color = "gray")