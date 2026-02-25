#' course: "Computational Social Science and Digital Behavioral Data, University of Mannheim"
#' title: "Data wrangling and first text analysis steps"
#' author: "Sebastian Stier"
#' institute: University of Mannheim & GESIS


# Trump Twitter Archive ----

# Download the Trump Twitter archive and save the file in the folder "data"
# https://drive.google.com/file/d/1xRKHaP-QwACMydlDnyFPEaFdtskJuBa6/view
library(tidyverse)
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
# These two commands are mostly used in combination:
# "group_by" groups columns by a grouping variable
# "summarize" consolidates the mentioned column based on the grouping variable
# into a single row
df_trump 

df_trump  


# Some basic text operations ----

# Calculate the occurance of the words "crazy" or "fake" across devices
# hint: use ?str_detect
df_trump

# Some tests
test_vec <- c("fakenews", "fake", "FAKE", "FakE", "FAKENEWS", "gesetz", "wahl", "bundestagswahl")
tolower(test_vec)
toupper(test_vec)
str_detect(test_vec, "fake")

#TODO HOMEWORK
fruit <- c("apple", "banana", "pear", "pineapple")
str_detect(fruit, "a")
str_detect(fruit, "^a")
str_detect(fruit, "a$")
str_detect(fruit, "b")
str_detect(fruit, "[aeiou]")
