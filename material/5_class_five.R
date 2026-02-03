#' class: "Computational Social Science and Digital Behavioral Data, University of Mannheim"
#' title: "Automated text analysis"
#' author: "Sebastian Stier"
#' lesson: 5
#' institute: University of Mannheim & GESIS
#' date: "2025-04-09"

library(tidyverse)
library(quanteda)
library(adaR)

# Load the web tracking data
load("data/toy_browsing.rda")
load("data/toy_survey.rda")

# HOMEWORK ----

# Summarize the number of total visits, Google and Facebook visits per person
df_wt <- toy_browsing %>% 
  mutate(domain = adaR::ada_get_domain(url),
         google = domain == "google.com",
         facebook = domain == "facebook.com")
df_panelist <- df_wt %>% 
  group_by(panelist_id) %>% 
  summarise(total_visits = n(),
            google_visits = sum(google, na.rm = T),
            facebook_visits = sum(facebook, na.rm = T)
            )

# Merge the survey data with the number of total visits, Google visits and Facebook visits 
# per panelist_id
df_panelist_survey <- df_panelist %>% 
  left_join(toy_survey, by = "panelist_id")

# Plot the relation of Facebook visits and age with a point diagram
df_panelist_survey %>% 
  ggplot(aes(x = age, y = facebook_visits)) +
  geom_point() +
  geom_smooth()

df_panelist_survey %>% 
  ggplot(aes(x = facebook_visits)) +
  geom_histogram()

table(df_panelist_survey$facebook_visits)

# Let's explore
df_panelist_survey %>% 
  filter(facebook_visits == 66734)

# Scrape and parse web data ----
library(rvest)

# Subset the web tracking data to visits of the politics section of Fox News
df_fox <- df_wt %>% 
  as_tibble() %>% 
  mutate(fox_politics = str_detect(url, "foxnews.com/politics"),
         foxnews = domain == "foxnews.com") %>% 
  filter(fox_politics == TRUE)
nrow(df_fox)

# Create a vector of unique Fox News political URLs
urls <- unique(df_fox$url)
nrow(df_fox)
length(urls)

# Read the HTML from a Fox News URL
webpage <- read_html(urls[1])

# Extract the headline (<h1> tag)
headline <- webpage %>%
  html_node("h1") %>%  # Modify the tag based on the website
  html_text()

# Extract the body text (<p> tag for paragraphs)
body <- webpage %>%
  html_nodes("p") %>%  # Modify the tag based on the website structure
  html_text() %>%
  paste(collapse = " ")  # Combine paragraphs into a single text

# Show the results
headline
body

# Inspect the output
cat(body)

# Use a for loop to create a data frame with the scraped results from all Fox News URLs

# create an empty data frame
df_text <- data.frame()
for (i in 1:5) {
  
  # Read the HTML from the page
  webpage = read_html(urls[i])
  
  # Extract the headline (<h1> tag)
  headline = webpage %>%
    html_node("h1") %>%  # Modify the tag based on the website
    html_text()
  
  # Extract the body text (<p> tag for paragraphs)
  body = webpage %>%
    html_nodes("p") %>%  # Modify the tag based on the website structure
    html_text() %>%
    paste(collapse = " ")  # Combine paragraphs into a single text
  
  # Save in data frame
  df_text = df_text %>% 
    bind_rows(
      data.frame(url = urls[i],
                 headline = headline,
                 body = body)
    )
  
  Sys.sleep(time = 3)
  
}

# Join the htmls with the web tracking data
df_fox <- df_fox %>% 
  left_join(df_text, by = "url")

# Clean the text a little bit
df_fox <- df_text %>% 
  mutate(body_clean = str_remove(body, "This material may not be published, broadcast, rewritten , or redistributed. ©2025 FOX News Network")
  )
df_fox$body[5]
df_fox$body_clean[5]


# Group exercise: Create a document-feature-matrix from the Trump tweet corpus ----
library(quanteda)

# First load the Trump corpus again
df_trump <- read_csv("data/tweets_01-08-2021.csv", col_types = "ccllcddTl")

# Do all steps in one tidyverse pipe and remove the token "amp"
dfm_nostop <- corpus(df_trump, text_field = "text", docid_field = "id") %>% 
  quanteda::tokens(remove_punct = TRUE, 
                   remove_url = TRUE,
                   remove_numbers = TRUE) %>% 
  tokens_select(c(stopwords("en"), "amp", "rt"), selection = "remove") %>% 
  dfm()

# Inspect
topfeatures(dfm_nostop)

# trim the dfm to only words that appear at least 10 times to make modeling more efficient
dfm_nostop
dfm_trimmed <- dfm_nostop %>% 
  dfm_trim(min_termfreq = 10)
dfm_trimmed


# Further text analysis methods ----
library(quanteda.textmodels)
library(quanteda.textstats)
library(quanteda.textplots)

#* Frequency counts ----
# inspect all of the features via a data frame
feature_table <- textstat_frequency(dfm_trimmed) %>%
  as_tibble()
feature_table
nrow(feature_table)
table(feature_table$feature == "nancy")

# inspect all of the features via a grouped data frame
feature_table_grouped <- textstat_frequency(dfm_trimmed, groups = device)
nrow(feature_table_grouped)
table(feature_table_grouped$feature == "nancy")


#* Dictionary analysis ----
?dictionary
dict <- dictionary(list(fake = c("fake", "fake news"),
                        democrats = c("democrat*", "nancy"),
                        republicans = c("repub*", "gop"))
                   )
dfm_dict <- dfm_lookup(dfm_nostop, dictionary = dict)
textstat_frequency(dfm_dict)

# Add a grouping variable and info on the total number of documents
dfm_dict <- dfm_lookup(dfm_nostop, dictionary = dict, 
                       nomatch = "n_unmatched") %>% 
    dfm_group(device) 


#* Keyness analysis ----
# We can easily plot differences in word use by group (e.g., parties, gender, etc.)
dfm_trimmed %>% 
    dfm_group(groups = isRetweet) %>% 
    textstat_keyness() %>% 
    textplot_keyness()

