#' course: "Computational Social Science and Digital Behavioral Data, University of Mannheim"
#' title: "Web tracking data exercises"
#' author: "Sebastian Stier"
#' institute: University of Mannheim & GESIS
library(tidyverse)


# YouTube API ----
library(tuber)
# If you want to use this, do your API verification here: 
#https://developers.google.com/youtube/v3/getting-started
# more precisely get an API key here: https://console.cloud.google.com/apis/credentials

#client_id <- "YOUR-CLIENT-ID"
#client_secret <- "YOUR-CLIENT-SECRET"

yt_oauth(
  app_id = client_id,
  app_secret = client_secret
)
# After you were offline, you have to re-authenticate in a browser using the following command
# unlink(".httr-oauth")
# then run the chunk yt_oauth... above again

#check out the functions in 
#tuber::
get_stats(video_id = "24UV7imfIRk")

get_video_details(video_id = "24UV7imfIRk")

df_yt <- get_comment_threads(
  filter = list(video_id = "24UV7imfIRk"),
  max_results = 20
)

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
list.files("data")
load("data/toy_browsing.rda")
load("data/toy_survey.rda")

# different ways of storing data
# save() load() #rda
# write_rds() read_rds() #rds from the tidyverse
# write_csv read_csv() #csv from the tidyverse 

# Create object df_wt for further analysis
df_wt <- toy_browsing %>% 
  as_tibble()
table(df_wt$device)
glimpse(df_wt)

### HOMEWORK UNTIL 22 April 2026 ####
# Explore the dataset: what is the number of rows, columns, unique persons, 
# what is the covered date range?

# Calculate the mean and median number of website visits (number of rows)
# per device

# What is the share of mobile vs. desktop per wave?


# Plot a time series of the number of website visits per day

### HOMEWORK ENDS ####


# Exercise 2: Domain augmentation of the web tracking data ----

# What are the top ten visited domains in the data?
## Install the R package adaR: https://gesistsa.github.io/adaR/
library(adaR)
## Apply the relevant function from the package to extract domains from URLs
df_wt <- df_wt %>% 
  mutate(domain = adaR::ada_get_domain(url))

# Rank the domains according to their appearance
df_wt %>% 
  count(domain) %>% 
  arrange(desc(n))

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


