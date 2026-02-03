#' class: "Computational Social Science and Digital Behavioral Data, University of Mannheim"
#' title: "TSeminar Presentations and Open Science"
#' author: "Sebastian Stier"
#' lesson: 7
#' institute: University of Mannheim & GESIS
#' date: "2025-05-14"

library(tidyverse)
library(quanteda)

# Sentiment analysis ----

# Load the tweets from the Trump Twitter Archive
df_trump <- read_csv("data/tweets_01-08-2021.csv", col_types = "ccllcddTl")
dfm_nostop <- corpus(df_trump, text_field = "text", docid_field = "id") %>% 
  quanteda::tokens(remove_punct = TRUE, 
                   remove_url = TRUE,
                   remove_numbers = TRUE) %>% 
  tokens_select(c(stopwords("en"), "amp", "rt"), selection = "remove") %>% 
  dfm()

summary(dfm_nostop)
docvars(dfm_nostop)

# quanteda has built in dictionaries ----
#e.g., Lexicoder Sentiment Dictionary (LSD2015) for political sentiment analysis 
lengths(data_dictionary_LSD2015)

# select only the "negative" and "positive" categories
data_dictionary_LSD2015_pos_neg <- data_dictionary_LSD2015[1:2]

# Apply dictionary
df_trump_sent <- dfm_nostop %>% 
  dfm_lookup(data_dictionary_LSD2015_pos_neg) %>% 
  convert(to = "data.frame") %>% 
  as_tibble()

# Add the original tweet text
df_trump_sent$text <- df_trump$text

#SentiWS (SentimentWortschatz) ----
#https://wortschatz-leipzig.de/de/download

# read in party manifestos of German parties in 2013 and 2017
corp_ger <- read_rds("https://www.dropbox.com/s/uysdoep4unfz3zp/data_corpus_germanifestos.rds?dl=1")
corp_ger

summary(corp_ger)
docvars(corp_ger)

# Create a document-frequency matrix, with German stopwords removed, 
#trimmed to features that occur at least 10 times
corp_ger_sentences <- corp_ger %>% 
  corpus_reshape(to = "sentences")
df_manifesto_sentences <- corp_ger_sentences %>% 
  tokens() %>% 
  dfm()

# Download dictionary
#https://downloads.wortschatz-leipzig.de/etc/SentiWS/SentiWS_v2.0.zip
# put the files into the /data folder

# Build a quanteda dictionary from the input
sentiws_positive <- read_table("data/SentiWS_v2.0_Positive.txt", 
                               col_names = c("word_stem", "polarity", "words"))
sentiws_negative <- read_table("data/SentiWS_v2.0_Negative.txt", 
                               col_names = c("word_stem", "polarity", "words"))

# Turn each word and its polarity rating into a row
sentiws_expanded <- sentiws_positive %>%
  bind_rows(sentiws_negative) %>% 
  filter(!is.na(words)) %>%
  separate_rows(words, sep = ",") %>%
  mutate(words = str_trim(tolower(words))) %>%
  select(word = words, polarity) 

# Keep only terms in SentiWS
dfm_sentiment <- dfm_select(df_manifesto_sentences, pattern = sentiws_expanded$word)

# Align polarity vector
polarity_vector <- sentiws_expanded$polarity[match(featnames(dfm_sentiment), sentiws_expanded$word)]
names(polarity_vector) <- featnames(dfm_sentiment)

# Multiply frequencies by polarity weights
weighted_counts <- dfm_sentiment %*% polarity_vector

# Divide by total token count to get average polarity
avg_polarity <- as.numeric(weighted_counts) / ntoken(dfm_sentiment)


# Result as a data frame
doc_sentiment <- tibble(
  document = docnames(df_manifesto_sentences),
  text = as.character(corp_ger_sentences),
  avg_polarity = avg_polarity
)

# Get average per party
doc_sentiment %>% 
  mutate(document = str_remove(document, " .*")) %>% 
  group_by(document) %>% 
  summarise(mean_polarity = mean(avg_polarity, na.rm = T))

