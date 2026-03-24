#' course: "Computational Social Science and Digital Behavioral Data, University of Mannheim"
#' title: "Web tracking data exercises"
#' author: "Sebastian Stier"
#' institute: University of Mannheim & GESIS
library(tidyverse)


# YouTube API ----
library(tuber)
# If you want to use this, do your API verification here: 
#https://developers.google.com/youtube/v3/getting-started

#client_id <- "YOUR-CLIENT-ID"
#client_secret <- "YOUR-CLIENT-SECRET"

yt_oauth(
  app_id = client_id,
  app_secret = client_secret
)

#check out the functions in 
#tuber::
get_stats(video_id = "24UV7imfIRk")

get_video_details(video_id = "24UV7imfIRk")

df_yt <- get_comment_threads(video_id = "24UV7imfIRk", max_results = 20)


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
table(df_wt$device)

# Explore the dataset: what is the number of rows, columns, unique persons, 
# what is the covered date range?

# Calculate the mean and median number of website visits (number of rows)
# per device


# What is the share of mobile vs. desktop per wave?


# Plot a time series of the number of visits per day


# Exercise 2: Domain augmentation of the web tracking data ----

# What are the top ten visited domains in the data?
## Install the R package adaR: https://gesistsa.github.io/adaR/
library(adaR)
## Apply the relevant function from the package to extract domains from URLs
glimpse(df_wt)
df_wt <- df_wt %>% 
  mutate(domain = adaR::ada_get_domain(url))

# Rank the domains according to their appearance

# Inspect whether there are NAs in domain; what can explain the NAs?


# Summarize the number of total visits, Google and Facebook visits per person

# Merge the survey data with the number of total visits, Google visits and Facebook visits 
# per panelist_id


# Plot the relation of Facebook visits and age with a point diagram



# Exercise 3: Analysis of news website visits ----

# Merge the news domain information with the web browsing data
## Load U.S. news domain list
news_list <- read.csv("https://raw.githubusercontent.com/ercexpo/us-news-domains/main/us-news-domains-v2.0.0.csv")

# First, check whether there are duplicates in the news data 
nrow(news_list)

# de-duplicate a vector
unique(c("sebastian", "sebastian", "felix"))

# remove the duplicates

# Finally, join the web tracking data with the news lists


# Identify the web tracking visits whose URL contains "trump"
## hint: ?str_detect


# Some more explorations of our new variables: where outside of news websites does trump occur?
# most popular trump domains

