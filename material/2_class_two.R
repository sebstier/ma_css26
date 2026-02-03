#' course: "Computational Social Science and Digital Behavioral Data, University of Mannheim"
#' title: "Research ethics in CSS and web data collection"
#' author: "Sebastian Stier"
#' class: 2
#' institute: University of Mannheim & GESIS
#' date: "2025-02-26"


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
get_stats(video_id = "IXDR2-WWY5Y")

get_video_details(video_id = "IXDR2-WWY5Y")

df_yt <- get_comment_threads(c(video_id = "IXDR2-WWY5Y"), max_results = 20)


# Data visualization using gapminder data ----
library(tidyverse)
#library(ggplot2) # ggplot2 is part of the tidyverse and should already be loaded
library(gapminder)

# Create a scatter plot / point diagram of lifeExp and gdpPercap
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  scale_x_log10()

# Save the plot
#ggsave saves the plot that is shown
ggsave("plots/example_plot.png", width = 8, height = 6.5, dpi = 800) # dpi increases the quality
ggsave("plots/example_plot.pdf", width = 8, height = 6.5) #fig1_gdpcap_lifeExp.png

# A more specific way to save specific plots
p1 <- ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  scale_x_log10()
ggsave(plot = p1, 
       filename = "plots/example_plot.pdf", width = 8, height = 6.5) #fig1_gdpcap_lifeExp.png

# Calculate the (worldwide) average GDP per capita per year per continent
# Plot this as a bar chart
table(gapminder$year)
gapminder %>% 
  group_by(year, continent) %>% 
  summarise(mean_gdp = mean(gdpPercap)) %>% 
  #filter(year == 1972)
  ggplot(aes(x = year, y = mean_gdp, fill = continent)) +
  #geom_bar(stat = "identity") # equivalent to geom_col
  geom_col()

# mutate vs summarise
gapminder %>% 
  group_by(year, continent) %>% 
  summarise(mean_gdp = mean(gdpPercap)) %>% 
  filter(year == 1972)
gapminder %>% 
  group_by(year, continent) %>% 
  mutate(mean_gdp = mean(gdpPercap)) %>% 
  filter(year == 1972)

# Trump Twitter Archive ----

# Download the Trump Twitter archive and save the file in the folder "data"
# https://drive.google.com/file/d/1xRKHaP-QwACMydlDnyFPEaFdtskJuBa6/view
library(tidyverse)
list.files("data")
#df_trump <- read_csv("data/tweets_01-08-2021.csv")
#glimpse(df_trump)
df_trump <- read_csv("data/tweets_01-08-2021.csv",
                     col_types = "ccllcddTl")
# df_trump <- read.csv("data/tweets_01-08-2021.csv", 
#                      colClasses = c("id" = "character")) 
summary(df_trump)
glimpse(df_trump)

# Different data formats
#read_csv
# write_rds(df_trump, "data/df_trump.rds")
# df_trump %>% 
#   write_rds("data/df_trump.rds")
list.files("data")
df_trump <- read_rds("data/df_trump.rds")


# Use group_by() and summarize() to summarize the number of tweets per day
# These two commands are mostly used in combination:
# "group_by" groups columns by a grouping variable
# "summarize" consolidates the mentioned column based on the grouping variable
# into a single row
df_trump %>% 
  # Let's just create a copy of "date"
  # mutate(day = date)
  # Let's create a sum index of favorites and retweets
  # mutate(sum_favs_rets = favorites+retweets)
  mutate(day = as.Date(date)) %>% 
  group_by(day) %>% 
  summarise(sum_tweets = n(),
            sum_favorites = sum(favorites),
            mean_favorites = mean(favorites),
            median_favorites = median(favorites)) %>% 
  arrange(desc(sum_favorites))
#Number of days in the raw data
df_trump <- df_trump %>% 
  mutate(day = as.Date(date))
n_distinct(df_trump$day)


# Some basic text operations ----

# Let's explore the function ?str_detect
# Some tests
test_vec <- c("fakenews", "fake", "FAKE", "FakE", "FAKENEWS", 
              "gesetz", "wahl", "bundestagswahl")
tolower(test_vec)
toupper(test_vec)
test_vec
str_detect(test_vec, "fake")
str_detect(tolower(test_vec), "fake")

# Let's apply to gapminder: let's detect countries based on name patterns
test <- gapminder %>% 
  filter(str_detect(tolower(country), "ger")) %>% 
  as.data.frame()
View(test)

as.data.frame(gapminder)
gapminder
head(as.data.frame(gapminder), 10)

# Calculate the occurrence of the words "crazy" or "fake" across devices
table(df_trump$device)
names(df_trump)
df_trump %>% 
  mutate(text_lower = tolower(text),
         fake = str_detect(text_lower, "fake"),
         crazy = str_detect(text_lower, "crazy")) %>% 
  # and calculate the share of tweets per device that contain either "crazy" or "fake"
  group_by(device) %>% 
  summarise(total_tweets = n(),
            sum_crazy = sum(crazy),
            sum_fake = sum(fake),
            share_crazy_fake = (sum_crazy+sum_fake) / total_tweets*100)
table(df_trump$device)
  
# What does the n() do?
df_trump %>% 
  summarise(n = n())
nrow(df_trump)
df_trump %>% 
  group_by(device) %>% 
  summarise(n = n())


# Visualizing the Trump tweets dataset ----

# Create a time series plot of the daily share of "fake" over time
# 1) mutate: create day as.Date()
# 2) mutate: lower(text)
# 3) mutate: create fake
# 3) group_by day

df_trump %>% 
  mutate(day = as.Date(date),
         fake = str_detect(tolower(text), "fake")) %>% 
  group_by(day) %>%
  summarise(sum_fake = sum(fake),
            total_tweets = n(),
            share_fake = sum_fake/total_tweets) %>% 
  ggplot(aes(x = day, y = sum_fake)) +
  geom_point() +
  geom_smooth()

