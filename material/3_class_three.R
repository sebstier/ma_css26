#' class: "Computational Social Science and Digital Behavioral Data, University of Mannheim"
#' title: "Designed Digital Data"
#' author: "Sebastian Stier"
#' lesson: 3
#' institute: University of Mannheim & GESIS
#' date: "2025-03-12"

library(tidyverse)

# Exercise 1: Explore a toy web tracking and survey dataset ----

# Load the browsing data
filename <- "data/toy_browsing.rda"
download.file(url = "https://osf.io/download/52pqe/", destfile = filename)
load(filename)

# Load the survey data
filename <- "data/toy_survey.rda"
download.file(url = "https://osf.io/download/jyfru/", destfile = filename)
load(filename)
rm(filename)

# Load the data
# different ways of storing data
# save() load() #rda
# write_rds() read_rds() #rds from the tidyverse
# write_csv read_csv() #csv from the tidyverse 
list.files("data")
load("data/toy_browsing.rda")
load("data/toy_survey.rda")

# Create object df_wt for further analysis
df_wt <- toy_browsing %>% 
  as_tibble()
table(df_wt$wave)

# START OF HOMEWORK

# Explore the dataset: what is the number of rows, columns, unique persons, 
# what is the covered date range?
glimpse(df_wt)
nrow(df_wt)  
ncol(df_wt)
n_distinct(df_wt$panelist_id)
length(unique(df_wt$panelist_id))
range(df_wt$timestamp)
range(as.Date(df_wt$timestamp))

# Calculate the mean and median number of website visits (number of rows) 
# per wave and device
table(df_wt$wave)
table(df_wt$device)
df_wt %>% 
  group_by(panelist_id, wave, device) %>% 
 # group_by(wave, device) %>% 
  summarise(n_visits = n()) %>% 
  ungroup() %>% 
  group_by(wave, device) %>% 
  summarise(mean_visits = mean(n_visits),
            median_visits = median(n_visits))

# How many of the visits happened on mobile vs. desktop for each wave? 
# What is the share of mobile vs. desktop per wave?
table(df_wt$wave, df_wt$device)
df_wt %>% 
  group_by(wave, device) %>% 
  summarise(total_visits = n()) %>% 
  group_by(wave) %>% 
  mutate(wave_n = sum(total_visits),
         share = total_visits/wave_n)

# Plot a time series of the number of visits per day
df_wt %>% 
  mutate(day = as.Date(timestamp)) %>% 
  group_by(day) %>% 
  summarise(n_visits = n()) %>% 
  ggplot2::ggplot(aes(x = day, y = n_visits)) +
  geom_line()

# END OF HOMEWORK ----

# Exercise 2: Domain augmentation of the web tracking data ----

# What are the top ten visited domains in the data?
## Install the R package adaR: https://gesistsa.github.io/adaR/
library(adaR)
## Apply the relevant function from the package to extract domains from URLs
glimpse(df_wt)
df_wt <- df_wt %>% 
  mutate(domain = adaR::ada_get_domain(url))

# Rank the domains according to their appearance
df_wt %>% 
  group_by(domain) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

# Inspect whether there are NAs in domain; what can explain the NAs?
table(is.na(df_wt$domain))
table(df_wt$domain == "nytimes.com")

df_wt %>% 
  filter(is.na(domain))

## HOMEWORK STARTS

# Summarize the number of total visits, Google and Facebook visits per person

# Merge the survey data with the number of total visits, Google visits and Facebook visits 
# per panelist_id


# Plot the relation of Facebook visits and age with a point diagram

## HOMEWORK ENDS


# Exercise 3: Analysis of news website visits ----

# Merge the news domain information with the web browsing data
## Load U.S. news domain list
news_list <- read.csv("https://raw.githubusercontent.com/ercexpo/us-news-domains/main/us-news-domains-v2.0.0.csv")

# First, check whether there are duplicates in the news data 
nrow(news_list)
news_list <- news_list %>% 
  as_tibble() %>% 
  filter(!duplicated(domain))
nrow(news_list)

# de-duplicate a vector
unique(c("sebastian", "sebastian", "felix"))

# remove the duplicates
news_list <- news_list %>% 
  filter(!duplicated(domain))
nrow(news_list)

# Finally, join the web tracking data with the news lists
news_list$news <- 1
nrow(df_wt)
df_wt_news <- df_wt %>% 
  left_join(news_list, by = "domain")
names(news_list)
nrow(df_wt)
names(df_wt)
table(df_wt$news, useNA = "a")

# Identify the web tracking visits whose URL contains "trump"
## hint: ?str_detect
df_wt_news <- df_wt_news %>% 
  mutate(trump = str_detect(url, "trump"))
table(df_wt_news$trump)
df_wt_news %>% 
  group_by(news, trump) %>% 
  summarise(n = n())

# Some more explorations of our new variables: where outside of news websites does trump occur?
# most popular trump domains
table(df_wt_news$news, useNA = "a")
df_nonnews_trump <- df_wt_news %>% 
  filter(trump == TRUE & is.na(news)) 
df_nonnews_trump %>% 
  group_by(domain) %>% 
  count() %>% 
  arrange(desc(n))

df_wt_news <- df_wt_news %>% 
  mutate(yahoo_news = str_detect(url, "yahoo.com/news"),
         news_new = case_when(yahoo_news == TRUE ~ 1,
                          news == 1 ~ 1,
                          .default = 0)
         )
table(df_wt_news$yahoo_news)
table(df_wt_news$news)
table(df_wt_news$news_new)

