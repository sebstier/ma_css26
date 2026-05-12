#' course: "Computational Social Science and Digital Behavioral Data, University of Mannheim"
#' title: "Introduction to automated text analysis"
#' author: "Sebastian Stier"
#' institute: University of Mannheim & GESIS
library(tidyverse)
# For more advanced text analysis, we use the package quanteda
library(quanteda)

# Text preprocessing ----

# Load the tweets from the Trump Twitter Archive
df_trump <- read_csv("data/tweets_01-08-2021.csv", col_types = "ccllcddTl")

# Create a variable "day"
df_trump <- df_trump %>% 
  mutate(day = as.Date(date))

# Create a corpus of Trump tweets 
glimpse(df_trump)
corp_trump <- corpus(df_trump, text_field = "text", docid_field = "id")
corp_trump
nrow(df_trump)
ncol(df_trump)
docvars(corp_trump)
summary(corp_trump, 5)
df_trump$text[4]

# Subset corpus to tweets in the year 2019
range(corp_trump$day)
corp_trump_subset <- corpus_subset(corp_trump, date >= "2019-01-02" & date <= "2019-12-31")
range(corp_trump_subset$date)
ndoc(corp_trump_subset)
ndoc(corp_trump)

# Subset corpus to tweets in the year 2020 and that have more than 200.000 retweets
corp_trump_subset <- corpus_subset(corp_trump, date >= "2020-01-01" & date <= "2020-12-31" & retweets > 200000)

# Reshape the corpus from document-level to sentence-level
ndoc(corp_trump_subset)
corp_sentences <- corpus_reshape(corp_trump_subset, to = "sentences") # another option is paragraph
ndoc(corp_sentences)
head(corp_trump_subset)
head(corp_sentences)

# Count the number of tokens / words for each document
ntoken(corp_sentences)[1:5]
ntoken(corp_trump_subset)[1:5]
corp_sentences$word_count <- ntoken(corp_sentences)
summary(corp_sentences, 5)

# Next, we need to tokenize a corpus
toks <- tokens(corp_trump_subset)
toks
toks <- tokens(toks, remove_punct = TRUE, remove_numbers = TRUE)
toks
toks_bigrams <- tokens_ngrams(toks, n = 2, concatenator = "_")
toks_bigrams

# Keywords in context
toks <- tokens(corp_trump)
toks <- tokens(toks, remove_punct = TRUE, remove_numbers = TRUE)
kw_fake <- kwic(toks, pattern =  "*climate*", window = 3)
head(kw_fake, 13) # a function for printing the output more clearly

# What does the star do?
test_vec <- c("straße", "straßenverkehrsordnung", "holperstraße", "street", "straßeholper", "straßen", 
              "regelverfahrenstraßenbauen")
test_toks <- tokens(test_vec)
kwic(test_toks, pattern = "straße") # only perfect matches
kwic(test_toks, pattern = "straße*") # word begin with straße
kwic(test_toks, pattern = "*straße") # word end with straße
kwic(test_toks, pattern = "*straße*") # any match where straße is included

# Even more context
kw_fake2 <- kwic(toks, pattern = c("fake", "democr*", "crazy"), window = 4)
head(kw_fake2, 30)

# Sometimes we are looking for more than one word 
kw_multiword <- kwic(toks, pattern = phrase(c("fake news", "crazy nancy")))
head(kw_multiword, 15)

# Remove stopwords
stopwords("en")
stopwords("de")
stopwords("it")
stopwords("fr")
stopwords("es")
toks_nostop <- tokens_select(toks, pattern = c(stopwords("en"), "rt", "amp"), selection = "remove")

# Also remove urls
toks_nostop <- tokens(toks_nostop, remove_url = TRUE)

# Create our first document feature matrix (dfm)
dfm_nostop <- dfm(toks_nostop)
print(dfm_nostop, 500)

# Show the top features
topfeatures(dfm_nostop, 50, decreasing = TRUE)
topfeatures(dfm_nostop, 50, decreasing = FALSE)


# Text analysis models ----
library(quanteda.textmodels)
library(quanteda.textstats)
library(quanteda.textplots)

# Do all preprocessing steps in one tidyverse pipe and remove the token "amp"
dfm_nostop <- df_trump %>% 
  corpus(text_field = "text", docid_field = "id") %>% 
  tokens(remove_punct = TRUE, 
         remove_numbers = TRUE) %>% 
  tokens_select(pattern = c("amp", stopwords("en"), "RT"), selection = "remove") %>% 
  dfm()

# Inspect
topfeatures(dfm_nostop)

# trim the dfm to only words that appear at least 10 times to make modeling more efficient
dfm_nostop
dfm_trimmed <- dfm_nostop %>% 
  dfm_trim(min_termfreq = 50) 
dfm_trimmed

#* Frequency counts ----
# inspect all of the features via a data frame
feature_table <- textstat_frequency(dfm_trimmed) %>% as_tibble()
feature_table
nrow(feature_table)
table(feature_table$feature == "nancy")

# inspect all of the features via a grouped data frame
feature_table_grouped <- textstat_frequency(dfm_trimmed, groups = device)
nrow(feature_table_grouped)
table(feature_table_grouped$feature == "nancy")


#* Dictionary analysis ----
?dictionary
dict <- dictionary(list(fake = c("fake", "cnn"),
                        democrats = c("democr*", "nancy"),
                        republicans = c("repub*", "gop"))
                   )
dfm_dict <- dfm_lookup(dfm_nostop, dictionary = dict)
textstat_frequency(dfm_dict)

# Add a grouping variable and info on the total number of documents
dfm_lookup(dfm_nostop, dictionary = dict, nomatch = "n_unmatched") %>% 
  dfm_group(isRetweet) 
textstat_frequency(dfm_dict)

#* Keyness analysis ----
# We can easily plot differences in word use by group (e.g., parties, gender, etc.)
dfm_trimmed %>% 
  dfm_group(groups = isRetweet) %>% 
  textstat_keyness() %>% 
  textplot_keyness()


# LLM application ----
library(rollama)
# You can explore the various functionalities of rollama here:
# https://jbgruber.github.io/rollama/articles/annotation.html#the-make_query-helper-function

# Example prompt
# First enter "ollama serve" into the Terminal to locally start the ollama server
rollama::ping_ollama()
#?pull_model
#pull_model() # Defaults to "llama3.1". List of models: https://ollama.com/library
show_model()

# Example chatbot interaction
query("Why is the sky blue? Answer with one sentence.")
query("What is the capital of Germany?")

# Classify multiple t#ext documents. We create a subset of interesting Trump tweets
df_trump_to_classify = df_trump %>% 
  slice(80:100) %>% 
  select(text) # select only the text variable to keep the data frame more readable

# Prepare classification task using make_query
queries <- make_query(
  text = df_trump_to_classify$text, 
  prompt = "Categories: positive, neutral, negative",
  template = "{prefix}{text}\n{prompt}",
  system = "Classify the sentiment of these tweets sent by Donald Trump. Answer with just the correct category.",
  prefix = "Text to classify: "
)

# Apply the classification (LLM inference)
df_trump_to_classify$sentiment <- query(queries, screen = FALSE, output = "text")

# Inspect results
View(df_trump_to_classify)

# Improve query to better handle URLs and repeat classification task
queries <- make_query(
  text = df_trump_to_classify$text, 
  prompt = "Categories: positive, neutral, negative",
  template = "{prefix}{text}\n{prompt}",
  system = "Classify the sentiment of these tweets sent by Donald Trump. Answer with just the correct category. If the text contains no meaningful words (e.g., only a URL), return Neutral.",
  prefix = "Text to classify: "
)

df_trump_to_classify$sentiment_refined <- query(queries, screen = FALSE, output = "text")
View(df_trump_to_classify)


# LDA Topic Models ----
library(seededlda)

# Restrict the number of features further, otherwise running the LDA will take long
dfm_trimmed <- dfm_nostop %>% 
  dfm_trim(min_termfreq = 50) # only features that appear at least 50 times
dfm_trimmed

# set a seed in order to keep the output consistent
set.seed(111)

# run the LDA Topic Model
tmod_lda <- textmodel_lda(dfm_trimmed, k = 10)
terms(tmod_lda, 10)
df_terms <- terms(tmod_lda, 15)
View(df_terms)

# Assign topic as a new variable
dfm_trimmed$topic <- topics(tmod_lda)

# Cross-table the topic frequency
table(dfm_trimmed$topic)

# Visualize topic model on the web
library(LDAvis)
phi <- tmod_lda$phi  # topic-term distribution
theta <- tmod_lda$theta  # document-topic distribution
vocab <- featnames(dfm_trimmed) # vocabulary
doc_length <- rowSums(dfm_trimmed)  # length of each document
term_frequency <- colSums(dfm_trimmed)  # term frequency

# Create the JSON object for visualization
json <- LDAvis::createJSON(phi = phi, theta = theta, vocab = vocab, 
                           doc.length = doc_length, term.frequency = term_frequency)

# Visualize
LDAvis::serVis(json)


# Wordfish ----
# read in party manifestos of German parties in 2013 and 2017
corp_ger <- read_rds("https://www.dropbox.com/s/uysdoep4unfz3zp/data_corpus_germanifestos.rds?dl=1")
summary(corp_ger)
docvars(corp_ger)

# Remove German stopwords, use only features that occur at least 50 times and create a dfm
dfm_ger <- corp_ger %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE) %>% 
  tokens_select(pattern = stopwords("de"), selection = "remove") %>%
  dfm() %>%
  dfm_trim(min_termfreq = 30)

# Run a wordfish model
model_wf <- textmodel_wordfish(dfm_ger)
textplot_scale1d(model_wf)

# Validation ----
library(caret)

# We take the two sentiment variables predicted by LLMs from above but harmonize the spelling
df_trump_to_classify <- df_trump_to_classify %>% 
  mutate(sentiment = tolower(sentiment),
         sentiment_refined = tolower(sentiment_refined))

# Binary cross-tab of the two dictionaries
tab_class <- table(sentiment = df_trump_to_classify$sentiment, 
                   sentiment_refined = df_trump_to_classify$sentiment_refined)
tab_class

# Confusion matrix and F1 scores
confusionMatrix(tab_class, mode = "everything")

# Plot
tab_class %>% 
  as.data.frame() %>% 
  ggplot(aes(x = sentiment, y = sentiment_refined, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 6) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  labs(title = "Confusion Matrix")
