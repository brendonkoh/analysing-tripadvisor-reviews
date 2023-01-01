library(rvest) #for web scraping
library(RSelenium) #for web scraping
library(netstat) #to retrieve ports not currently in use
library(tidyverse) #for data cleaning and processing
library(tm) #to clean text data
library(syuzhet) #to extract sentiment from text
library(sentimentr)  #for sentiment analysis
library(quanteda) #for tokenisation
library(quanteda.textstats)
library(tidytext) #for bing lexicon
library(wordcloud2) #to create word clouds
library(htmlwidgets) #to export the word clouds
library(webshot) #to export as .png
library(pander) #to create tables

##---Web Scraping---##
#rvest doesn't work but RSelenium does, though it's slower.
url <- "https://www.tripadvisor.co.uk/Attraction_Review-g504004-d2262911-Reviews-Knaresborough_Castle-Knaresborough_North_Yorkshire_England.html"

#Open TripAdvisor URL using Selenium
rD <- rsDriver(browser = "chrome", port = free_port(), chromever="108.0.5359.71", check = F, verbose = F)
remDr <- rD[["client"]]
remDr$navigate(url)

#reading html using selenium and storing it
html <- remDr$getPageSource()[[1]]

#using rvest to extract html info
html <- read_html(html)

#extracting reviews
reviews <- html%>%
  html_nodes('.yCeTE') %>%
  html_text()

#extracting number of reviews
review_count <- html %>% html_nodes('.Ci') %>% html_text()
review_count <- strsplit(review_count,'of ')[[1]][2]
#removing commas in the number and storing it as numeric object
review_count <- as.numeric(gsub(',','',review_count2))

#As TripAdvisor's url for reviews goes up in increments of 10, we can create a sequence of numbers to loop through
num <- seq(10, review_count, 10)

#closing off any programs
Sys.sleep(2)
remDr$closeall()
rD[["server"]]$stop()
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)

#automating web scraping for all reviews
for (i in 1:length(num)) {
    #generating url
    revurl <- paste0("https://www.tripadvisor.co.uk/Attraction_Review-g504004-d2262911-Reviews-or",num[i],"-Knaresborough_Castle-Knaresborough_North_Yorkshire_England.html")
    
    #opening browser through Selenium
    rD <- rsDriver(browser = "chrome", port = free_port(), chromever="108.0.5359.71", check = F, verbose = F)
    remDr <- rD[["client"]]
    remDr$navigate(revurl)
    
    #reading in the webpage
    html <- remDr$getPageSource()[[1]]
    html <- read_html(html)
    #extracting reviews
    reviewed <- html%>%
        html_nodes('.yCeTE') %>%
        html_text()
    #adding our scraped reviews to existing reviews
    reviews <- c(reviews,reviewed)
    #closing browser
    Sys.sleep(2)
    remDr$closeall()
    rD[["server"]]$stop()
}

system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)

#saving reviews so that we don't have to scrape again in case something goes wrong
save(reviews, "reviews.RData")

#Inspecting data
length(reviews)
head(reviews, n = 3)

##---Data Cleaning---##
#merging alternate rows to re-form full review
knaresreview <- sapply(1:(review_count/2), function(x){
  paste(reviews[2*x-1], ".", reviews[2*x])})
head(knaresreview, n = 3)

#removing duplicated entries
knaresreview <- knaresreview[!duplicated(knaresreview)]


##---Cleaning text data---##
#converting everything into lowercase
knaresreviewclean <- tolower(knaresreview)

#removing digits
knaresreviewclean <- gsub(pattern="\\d", replace=" ", knaresreviewclean)

#removing stop words
knaresreviewclean <- removeWords(knaresreviewclean, stopwords())

#removing punctuations
knaresreviewclean <- gsub(pattern="[[:punct:] ]+", replace=" ", knaresreviewclean)

#removing single letters/orphans
knaresreviewclean <- gsub(pattern="\\b[A-z]\\b{1}", replace=" ", knaresreviewclean)

#removing additional white spaces
knaresreviewclean <- stripWhitespace(knaresreviewclean)

#inspecting cleaned data
head(knaresreviewclean, n = 3)

#tokenising words
tokenised <- tokens(knaresreviewclean, what = 'word', remove_number = T, remove_punct = T, remove_symbols = T)

#identifying monograms and bigrams
token_list <- tokens_ngrams(tokenised, n = 1:2,concatenator = ' ')
token_list <- dfm(token_list)
token_freq <- textstat_frequency(token_list)
bigram_freq <- token_freq[grepl(' ',token_freq$feature),]
monogram_freq <- token_freq[!grepl(' ',token_freq$feature),]

#inspecting data
head(monogram_freq, n = 5)
head(bigram_freq, n = 5)


##---Creating word clouds---##
#Obtaining sentiments of words and phrases
monogram_senti <- get_sentiment(monogram_freq$feature, method = 'bing')
bigram_senti <- get_sentiment(bigram_freq$feature, method = 'bing')

#Creating dataframes for the monograms and bigrams
monograms <- data.frame(monogram_freq, monogram_senti)
bigrams <- data.frame(bigram_freq, bigram_senti)

#Creating a vector of inappropriately coded terms
neutrals <- c("ruin", "ruins", "ruined", "cave", "caves", "dungeon", "dungeons", "rail", "rails", "siege", "blind", "disabled", "concession")
positives <- c("cheap")

#Recoding sentiments for monograms
monograms_recoded <- monograms %>%
  mutate(monogram_senti = case_when(
    feature %in% neutrals ~ 0, #checks each row to see if the term in `feature` is within the `neutrals` vector
    feature %in% positives ~ 1, #checks each row for "cheap"
    TRUE ~ as.numeric(monogram_senti) #otherwise, retain original value
  ))

#Recoding sentiments for bigrams
bigrams_recoded <- bigrams %>%
  mutate(bigram_senti = case_when(
    grepl("railway", bigrams$feature) ~ as.numeric(bigram_senti),
    rowSums(sapply(neutrals, grepl, bigrams$feature))>0 ~ as.numeric(bigram_senti)+1, #checking each row to see if any of the terms appear in the `feature` column, +1 to counteract the -1 from these terms
    rowSums(sapply(positives, grepl, bigrams$feature))>0 ~ as.numeric(bigram_senti)+2, #+2 to go from -1 to +1
    TRUE ~ as.numeric(bigram_senti)
  ))

#Creating a data frame of positive terms that appear at least 20 times
pos_mono <- monograms_recoded %>% filter(monogram_senti > 0, frequency > 20) %>% select(feature, frequency)

#Plotting word cloud for positive monograms
posmono_cloud <- wordcloud2(pos_mono, rotateRatio = 0, backgroundColor = "#fffde8", shuffle = T)
posmono_cloud

#Exporting word cloud
saveWidget(posmono_cloud, "posmono.html", selfcontained = F) #as a .html file
webshot("posmono.html", "posmono.png", delay = 10) #as a .png file

#Repeating the process for positive bigrams
pos_bi <- bigrams_recoded %>% filter(bigram_senti > 0, frequency > 20) %>% select(feature, frequency)
posbi_cloud <- wordcloud2(pos_bi, rotateRatio = 0, backgroundColor = "#fffde8", shuffle = T)
posbi_cloud
saveWidget(posbi_cloud, "posbi.html", selfcontained = F)
webshot("posbi.html", "posbi.png", delay = 10)

#negative monograms word cloud
neg_mono <- monograms_recoded %>% filter(monogram_senti < 0, frequency > 4) %>% select(feature, frequency)
negmono_cloud <- wordcloud2(neg_mono, rotateRatio = 0, backgroundColor = "#f5dce2", shuffle = T)
negmono_cloud
saveWidget(negmono_cloud, "negmono.html", selfcontained = F)
webshot("negmono.html", "negmono.png", delay = 10)

#negative bigrams word cloud
neg_bi <- bigrams_recoded %>% filter(bigram_senti < 0, frequency > 4) %>% select(feature, frequency)
negbi_cloud <- wordcloud2(neg_bi, rotateRatio = 0, backgroundColor = "#f5dce2", shuffle = T)
negbi_cloud
saveWidget(negbi_cloud, "negbi.html", selfcontained = F)
webshot("negbi.html", "negbi.png", delay = 10)


##---Conducting sentiment analysis---##
#re-cleaning data for sentiment analysis

#removing digits
knares_senti_clean <- gsub(pattern="\\d", replace=" ", knaresreview)

#Storing bing lexicon
bing <- get_sentiments("bing")
bing <- bing %>% mutate(sentiment = recode(sentiment, negative = -1, positive = 1))

#recoding lexicon
bing_recoded <- bing %>%
  mutate(sentiment = case_when(
    word %in% neutrals ~ 0,
    word %in% positives ~ 1, 
    TRUE ~ sentiment))

#Converting it into a recognisable format for `sentimentr`
bing_key <- as_key(bing_recoded)

#Calculating sentiment scores
knares_sentiment <- sentiment_by(knares_senti_clean, polarity_dt = bing_key)
head(knares_sentiment)

#Checking results
#since `knares_sentiment` is arranged in the same order as the original reviews, I'll just use cbind.
knares_complete <- cbind(knaresreview, knares_sentiment)

#Most positive reviews
posreviews <- arrange(knares_complete, desc(ave_sentiment)) %>% select(1, 5)
pandoc.table(head(posreviews, n = 3), split.tables = Inf)

#Neutral reviews
neutreviews <- knares_complete %>% filter(ave_sentiment == 0) %>% select(1, 5)
pandoc.table(head(neutreviews, n = 3), split.tables = Inf)

#Most negative reviews
negreviews <- arrange(knares_complete, ave_sentiment) %>% select(1, 5)
pandoc.table(head(negreviews, n = 3), split.tables = Inf)

#summary statistics for sentiment
summary(knares_complete$ave_sentiment)
sd(knares_complete$ave_sentiment)

#Plotting distribution of sentiments
knares.plot <- ggplot(data = knares_complete, aes(x = ave_sentiment)) + 
  geom_density(fill = '#7CDF7C',alpha = 0.6) +
  theme_bw()+
  xlab('Sentiment')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_vline(xintercept = 0, color = "#E74C3C", linewidth = 1)

knares.plot

#exporting the plot
ggsave("knaresbing.png", plot = knares.plot, scale = 1.5)