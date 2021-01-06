# Load packages

textmining <- c("rtweet", "tidytext", "wordcloud", "wordcloud2", "SnowballC", "textstem", "textshape", "lexicon", "textclean")

formating <- c("RColorBrewer", "mdthemes")

data_mining <- c("tidyverse", "readxl", "lubridate", "janitor", "inspectdf", "scales", "ggchicklet", "xts", "anytime", "fable")


install.load::install_load(c(textmining, formating, data_mining))

theme_set(theme_bw())


# Import data

raw_tweets <- read_xlsx("data/tweets_data.xlsx", sheet = 1)

clean_tweets <- read_xlsx("data/tweets_data.xlsx", sheet = 2)

# Tweets by country

raw_tweets %>% count(country) %>% rename("Number of tweets retrieved" = n) %>% left_join(
  clean_tweets %>% count(country) %>% rename("Distinct tweets by user ID" = n))


## Line plot of country tweets

clean_tweets %>% 
  group_by(country) %>% 
  ts_plot(by = "days") +
  facet_wrap(~country, scales = "free_y" )+
  theme_bw()+
  labs(title = "Frequency of tweets per country", x="Month", y = "Number of daily tweets") +
  theme(legend.position = "none") 

# Data cleaning

## removing foreign characters and url

data_tweets_clean <- clean_tweets %>% mutate(tweet = str_remove_all(tweet, "&amp;|&lt;|&gt;"), tweet = str_replace_all(tweet, "http.*",""))


## Tokenization

data_tweets_token <- data_tweets_clean %>%   unnest_tokens(output = tweets, input = tweet) 

## Remove digit (using regex)

data_tweets_rm_number <- data_tweets_token %>% filter(!str_detect(tweets, "\\b\\d+\\b"))


## remove stopwords

custom_stop_words <- bind_rows(tibble(word = c("pic.twitter.com", "à", "ø", "ù", "î", "la", "ã", "ã", "ùˆù", "have","ð", "de", "â", "le"), lexicon = c("custom")), tidytext::stop_words)


data_tweets_rm_stopword <- data_tweets_rm_number %>%
  anti_join(custom_stop_words, by = c("tweets"= "word")) 


## Lemmatization

data_tweets_lemmatize <- data_tweets_rm_stopword %>%
  mutate_at("tweets", ~lemmatize_words(.)) %>% mutate(tweets = str_squish(tweets)) %>% 
  filter(str_length(tweets) >= 3)

# Most common words in the tweet by country

data_tweets_lemmatize %>% 
  group_by(country) %>% 
  count(tweets, sort = TRUE) %>%
  slice_max(order_by = n, n = 15) %>% ungroup() %>% 
  ggplot(aes(x = reorder_within(tweets, n, country), y = n, fill = country)) + geom_col(show.legend = FALSE) + facet_wrap(~country, scales = "free") + scale_x_reordered() + coord_flip() + 
  labs(y = "Frequency of words", x = NULL, title = NULL)+ theme(axis.text.y = element_text(face = "bold"))


# Sentiment analysis visualization

data <- read_csv("data/sentiment_data.csv")

data <- data %>%  mutate(van_pol_class = factor(van_pol_class, levels = c("Highly Positive", "Positive", "Neutral", "Negative", "Highly Negative")))

# Sentiment classification of the overall tweets 

sentiment_distribution <-  data %>% tabyl(van_pol_class) %>% 
  ggplot(aes(van_pol_class, y = n, fill = van_pol_class, label =  scales::percent(percent))) + 
  geom_chicklet(width = 0.3, show.legend = F)+
  scale_y_continuous(expand = c(0, 0)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_text(size = 8))+
  geom_text(position = position_dodge(0.9), vjust = "inward", hjust = "inward",
            size = 3) + 
  labs(x = "Sentiment classification", y = "Number of tweets") + scale_color_brewer() + 
  theme(axis.ticks.x = element_blank()) + theme_minimal()

sentiment_distribution

# Sentiment classification of tweets by country 

sentiment_by_country <-  data %>% 
  group_by(country, van_pol_class) %>% summarise(n = n(), .groups = "drop") %>% group_by(country) %>% mutate(pct = n/sum(n)*100) %>% arrange(country, desc(n))

sentiment_by_country %>% ggplot(aes(x = country, y = n, fill = van_pol_class))+ geom_col(position = position_dodge())+ labs(y = "Number of tweets", x = "Country", fill = "Sentiment classification")

# Sentiment classification of tweets by month

data %>% mutate(month = month(date, label = TRUE, abbr = FALSE )) %>% group_by(month, van_pol_class) %>% count() %>% ggplot(aes(x = month, y = n, fill = van_pol_class))+ geom_col(position = position_dodge())+ labs(y = "Number of tweets", x = "Month", fill = "Sentiment classification")