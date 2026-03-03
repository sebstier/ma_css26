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
# df_trump <- read.csv("data/tweets_01-08-2021.csv", 
#                      colClasses = c("id" = "character")) 
summary(df_trump)
glimpse(df_trump)

# Check if the tweet IDs are unique
n_distinct(df_trump$id)
nrow(df_trump)


# Use group_by() and summarize() to summarize the number of tweets per day 

# Now also summarize the number of retweets

# Aggregate the number of tweets and retweets per year
# Hint: use year()


# Some basic text operations ----

# Calculate the occurance of the words "crazy" or "fake" across devices
# hint: use ?str_detect
df_trump

# Some tests
test_vec <- c("fakenews", "fake", "FAKE", "FakE", "FAKENEWS", "gesetz", "wahl", "bundestagswahl")
tolower(test_vec)
toupper(test_vec)
str_detect(test_vec, "fake")


# Data visualization using gapminder data ----
library(ggplot2) # ggplot2 is part of the tidyverse and should already be loaded
library(gapminder)

# Create a scatter plot of lifeExp and gdpPercap

# Save the plot

# Create a bar chart showing the GDP/Capita of European countries in the year 2007


# TODO HOMEWORK  
# Calculate the (worldwide) average GDP per capita per year and plot this as a bar chart
# Sum the total world population per year. Plot the results in a bar chart for the years 1992-2007


# Visualizing the Trump tweets dataset ----

# We use mutate() and case_match() to create a variable indicating that the tweet was sent via 
#iPhone or Android or another device
table(df_trump$device)
df_trump %>% 
  count(device)

# Calculate the share of tweets per device that contain either "crazy" or "fake"


# Create a subset of the data that contains the tweets with either "crazy" or "fake"


# Add the variables to the data frame

# Create a time series plot of the daily share of "crazy" and "fake" over time

