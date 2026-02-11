#' course: "Computational Social Science and Digital Behavioral Data, University of Mannheim"
#' title: "Basic tidyverse and data wrangling"
#' author: "Sebastian Stier"
#' institute: University of Mannheim & GESIS


# Exercise 0: Install [R](https://cran.rstudio.com) and [RStudio Desktop](https://posit.co/downloads/) ----
# see also script 0_basics.R

# Exercise 1: Setup and R packages ----
## a. Create a folder for the R scripts and materials of this class and 
      # set the R working directory to this folder.
getwd()
setwd("YOURPATH")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# wd not needed if you create a project that will set the wd for you

# New project top left File -> New Project

## b. Create subfolders "data" and "plots"
dir.create("data")
dir.create("plots")

## c. Install the R package *tidyverse*. 
#install.packages("tidyverse")
library(tidyverse)
dplyr::select()
?select
# e.g. select a variable
starwars
colnames(starwars)
names(starwars)
row.names(starwars)
select(starwars, height)
select(starwars, c(height, mass, sex))
starwars %>% 
  select(., c(height, mass)) %>% 
  mutate(new_var = height+mass)

# how to get the pipe?
# CTRL + SHIFT + M
# How to run code
# CRTL + Enter

## d. Check the version of the *tidyverse* package
packageVersion("dplyr")
R.version

## e. List all your files in the working directory (folder) and the environment (top right)
list.files()
list.files("data")
getwd()
# create a test object
test <- "test"
starwars <- starwars
ls()

# Exercise 2: Transform a data frame into a tibble and name the differences between the two formats. ----
# let's first install and load the gapminder dataset
#install.packages("gapminder")
library(gapminder)

# How do R packages work?
# The tidyverse contains all the packages we will use today
# Hence ggplot2 and dplyr are ready to go after installing tidyverse
names(gapminder)
gapminder[, 1]
gapminder %>% select(country)
gapminder %>% 
    select(year, country) 
select(gapminder, c(year, country))

glimpse(gapminder)

# You can call functions from specific packages
dplyr::select()
#library(psych)

# Look up the values in a variable
table(gapminder$country)
gapminder %>% 
  count(country)

# Exercise 3: Gapminder explorations ----

# Explore the dataset
head(gapminder)
glimpse(gapminder)
View(gapminder)
summary(gapminder)
typeof(gapminder$pop)

# Produce a data frame with the data for Germany and some additional countries
gapminder_ger <- gapminder %>% 
  filter(country == "Germany")
nrow(gapminder)
nrow(gapminder_ger)
# Equivalent result in base r
gapminder_ger <- base::subset(gapminder, country == "Germany")

# Produce a data frame with the data for Germany and France
gapminder_ger_fra <- gapminder %>% 
  filter(country == "Germany" | country == "France")
nrow(gapminder_ger_fra)
gapminder_ger_fra

# Subset the data to France and the year 2007
gapminder %>% 
  filter(country == "France" | year == 2007) %>% 
  # and arrange the dataframe based on the nr. of observations
  count(country) %>% 
  arrange(desc(n))

# How many countries do we have in the data? List them
gapminder %>% 
  count(country)
unique(gapminder$country)
n_distinct(gapminder$country)

# Pipe-Operation with filter(), arrange()
# Select all country-years with a population size < 100 Mio., 
# arrange by GDP/capita in decreasing order (show the top 5 country-years)
gapminder %>% 
  filter(pop < 100000000) %>% 
  arrange(desc(gdpPercap)) %>% 
  #head(5)
  tail(5)


