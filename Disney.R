require(devtools)
require(twitteR)
require(dplyr)
require(tools)
# Authorize Tweeter

api_key <- "1KjHmhUZD7BJUJWcEFETIGDZA"
api_secret <- "iiwK1enlyMi0r0aGdDzrbld2DzHMZ0QwVaD1sdqBffCeuA0XbX"
access_token <- "2958192008-ku6bn8pgKakjWNXPM4UAQPpsG9F6hORLJbHQQOP"
access_token_secret <- "YmkCcylldOFlCd2ga10rr2encss3zvueMm1ItortqgwDS"

setup_twitter_oauth(api_key, 
                     api_secret, 
                     access_token, 
                     access_token_secret)


require(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- 	"LFNRqX5i1PkB69SjEEncXWloq"
consumerSecret <- "4sDHqY6aLm7PRfJLxpq6GsWqphZxzX3dXLjssSLXYhO8wPwL3F"
my_oauth <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, 
                             requestURL = requestURL, accessURL = accessURL, authURL = authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
save(my_oauth, file = "my_oauth.Rdata")


# Collect and Store Tweets Data

tweets <- searchTwitter('Disney', 
                       since = '2017-11-25', 
                       lang = 'en', 
                       n = 15000) 

tweets.df <- twListToDF(tweets)

write.csv(tweets.df, "tweets.csv") 



# Retrieve Tweets Data

tweets.df <- read.csv("tweets.csv")
tweets.df$screenName <- as.character(tweets.df$screenName)


# Collect and Store User Data

userinfo <- lookupUsers(tweets.df$screenName)  # Batch lookup of user info
userFrame <- twListToDF(userinfo)
userFrame$screenName <- as.character(userFrame$screenName)

write.csv(userFrame, "raw_userinfo.csv")

remove <- data_frame(location = showNonASCII(userFrame$location), 
                     remove = TRUE)

userFrame <- userFrame %>% anti_join(remove)

locations <- geocode(userFrame$location)
locations$screenName <- userFrame$screenName
locations$location <- userFrame$location

tweets.df <- left_join(x = tweets.df, y = locations, by= "screenName")

write.csv(tweets.df, "tweets+local.csv")



# Data Summary
require(ggmap)
require(ggplot2)
require(grid)

tweets.df <- read.csv("tweets+local.csv")
tweets.df$text <- as.character(tweets.df$text)
tweets.df$screenName <- as.character(tweets.df$screenName)
tweets.df$retweetCount <- as.integer(tweets.df$retweetCount)
tweets.df$lon <- as.numeric(tweets.df$lon)
tweets.df$lat <- as.numeric(tweets.df$lat)


map.data <- map_data("world")   
points <- data.frame(x = tweets.df$lon,  
                     y = tweets.df$lat)

ggplot(map.data) + 
  geom_map(aes(map_id = region),  
           map = map.data,  
           fill = "white",             
           color = "grey20", size = 0.25) + 
  expand_limits(x = map.data$long, y = map.data$lat) +            
  theme(axis.line = element_blank(),  
        axis.text = element_blank(),  
        axis.ticks = element_blank(),                     
        axis.title = element_blank(),  
        panel.background = element_blank(),  
        panel.border = element_blank(),                     
        panel.grid.major = element_blank(), 
        plot.background = element_blank(),                     
        plot.margin = unit(0 * c( -1.5, -1.5, -1.5, -1.5), "lines")) + 
  geom_point(data = points, 
             aes(x = x, y = y), size = 3, 
             alpha = 1/5, color = "darkblue")


# Text Analysis

require(tidyverse)
require(tidytext)
require(stringr)
require(wordcloud)

text.df <- data_frame(user = tweets.df$screenName,
                      text = tweets.df$text, 
                      location = tweets.df$location)

tidy.text <- unnest_tokens(text.df, word, text)

custom_stop_words <- bind_rows(data_frame(word = c("disney", "https", "disney's", "t.co", "rt", "6aue21xvif", "it's", "deal", "hgpmzy40z", "qbfzwwcjw", "40", "75", "it", "set", "assets"), 
                                          lexicon = "custom"), 
                               stop_words)

tidy.text <- tidy.text %>% anti_join(custom_stop_words)

word.freq <- tidy.text %>% count(word, sort=TRUE)

word.freq %>% 
  filter(n > 550) %>%
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_col() + 
  xlab(NULL) + 
  coord_flip() 


# Sentiment Analysis

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

nrcjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy") 

tidy.text %>% 
  inner_join(nrcjoy) %>% 
  count(word, sort = TRUE) 

tidy.text %>% 
  group_by(location) %>%
  inner_join(nrcjoy) %>% 
  count(word, sort = TRUE) 

tidy.text %>% 
  group_by(location) %>%
  inner_join(get_sentiments("nrc")) %>% 
  count(word, sort = TRUE) %>% 
  with(wordcloud(word, n, max.words = 100))

tidy.text %>% 
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

sentiment <- tidy.text %>% 
  inner_join(get_sentiments("bing")) %>%
  group_by(tidy.text$user) %>%
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) 

# ADD SENTIMENT BY STATE & COUNTRY






