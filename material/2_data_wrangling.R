#' course: "Computational Social Science and Digital Behavioral Data, University of Mannheim"
#' title: "Data wrangling and first text analysis steps"
#' author: "Sebastian Stier"
#' institute: University of Mannheim & GESIS
library(tidyverse)


# Trump Twitter Archive ----

# Download the Trump Twitter archive and save the file in the folder "data"
# https://drive.google.com/file/d/1xRKHaP-QwACMydlDnyFPEaFdtskJuBa6/view
list.files("data")
df_trump <- read_csv("data/tweets_01-08-2021.csv",
                     col_types = "ccllcddTl")
#df_trump <- read.csv("data/tweets_01-08-2021.csv")
n_distinct(df_trump$id)
nrow(df_trump)
# df_trump <- read.csv("data/tweets_01-08-2021.csv", 
#                      colClasses = c("id" = "character")) 
# Check if the tweet IDs are unique
n_distinct(df_trump$id)
nchar(df_trump$id)
summary(df_trump)
glimpse(df_trump)

# Check out the date range
range(df_trump$date)

# Create a code chunk where we add new variables to the data frame
df_trump <- df_trump %>% 
  mutate(day = as.Date(date),
         year = year(date))

# Use mutate(), then group_by() and summarize() to summarize the number of tweets per day 
df_trump %>% 
  #mutate(day = as.Date(date)) %>% 
  group_by(day) %>% 
  summarise(n_tweets = n()) %>% 
  arrange(desc(n_tweets))

# How many unique days are in the data frame?
n_distinct(df_trump$day)

# Now also summarize the number of retweets per day
df_trump %>% 
  group_by(day) %>% 
  summarise(n_retweets = sum(retweets)) %>% 
  arrange(desc(n_retweets))

# Aggregate the number of tweets and retweets per year
# Hint: use year()
df_trump %>% 
  group_by(year) %>% 
  summarise(n_tweets = n(),
            n_retweets = sum(retweets),
            mean_rts = n_retweets / n_tweets)


# We use mutate() and case_match() to create a variable indicating that the tweet was sent via 
# iPhone or Android or another device
table(df_trump$device)
df_trump %>% 
  count(device)
df_trump <- df_trump %>% 
  mutate(device_rec = case_match(device, 
                                 "Twitter for Android" ~ "Android",
                                 "Twitter for iPhone" ~ "iPhone",
                                 .default = "Other"))
df_trump %>% count(device_rec, device)
df_trump %>% count(device_rec)

# Some basic text operations ----

# Some tests
test_vec <- c("fakenews", "fake", "FAKE", "FakE", "FAKENEWS", "gesetz", "wahl", "bundestagswahl")
test_vec_lowered <- tolower(test_vec)
toupper(test_vec)
str_detect(test_vec, "fake")
str_detect(test_vec_lowered, "fake")

#  Create a subset of the data frame with tweets that contain "crazy" (or "fake" across devices)
# hint: use ?str_detect
nrow(df_trump)
df_trump_crazy <- df_trump %>% 
  filter(str_detect(tolower(text), "crazy|fake"))

# Data visualization using gapminder data ----
library(ggplot2) # ggplot2 is part of the tidyverse and should already be loaded
library(gapminder)

# Create a scatter plot of lifeExp and gdpPercap
gapminder %>% 
  ggplot(aes(x = lifeExp, y = gdpPercap, size = pop, color = continent)) +
  geom_point() +
  scale_y_log10() +
  labs(x = "Life expectancy", y = "GDP per capita (logged)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save the plot
ggsave(filename = "plots/lifeExp_gdpPercap.png", dpi = 800, height = 6, width = 10,
       bg = "white")

# Create a bar chart showing the GDP/Capita of European countries in the year 2007


# Calculate the (worldwide) average GDP per capita per year and plot this as a bar chart
# Sum the total world population per year. Plot the results in a bar chart for the years 1992-2007
case_match()

# Visualizing the Trump tweets dataset ----

# Calculate the share of tweets per device that contain either "crazy" or "fake"


# Use the subset of the data that contains the tweets with either "crazy" or "fake" (created above)


# Add the variables to the data frame

# Create a time series plot of the daily share of "crazy" and "fake" over time

