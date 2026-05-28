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
setwd("yourpath")
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
?select
select(starwars, c(height, mass, sex))
starwars %>% 
  select(height, mass)

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

# factors vs. character vectors
gapminder$country_chr <- as.character(gapminder$country)

# Look up the values in a variable
table(gapminder$country, useNA = "a") # "always
gapminder %>% 
  count(country)
?count
summary(gapminder)


# Exercise 3: Gapminder explorations ----

# Explore the dataset
head(gapminder)
glimpse(gapminder)
View(gapminder)
summary(gapminder)
typeof(gapminder$pop)

### HOMEWORK ####
# you need the function ?filter
# Produce a data frame with the data for Germany
df.germany <- filter(gapminder, country == "Germany")
df.germany <- gapminder %>% 
    filter(country == "Germany")
ls()

# Produce a data frame with the data for Germany and France
df.ger_fra_ita <- gapminder %>% 
  filter(country == "Germany" | country == "France" | country == "Italy")
df.ger_fra_ita <- gapminder %>% 
  filter(country == c("Germany", "France", "Italy"))
df.ger_fra <- gapminder %>% 
  filter(country %in% c("Germany", "France"))

table(df.ger_fra_ita$country_chr)
table(df.ger_fra_ita$country)

# Subset the data to France and the year 2007
df.fra_07 <- gapminder %>% 
  filter(country == "France" & year == 2007)

# How many countries do we have in the data? List them
table(gapminder$country)
gapminder %>% 
  distinct(country)
unique(gapminder$country)
length(unique(gapminder$country))
summary(gapminder)

### HOMEWORK ####

# Pipe-Operation with filter(), arrange()
# Select all country-years with a population size < 100 Mio., 
# arrange by GDP/capita in decreasing order (show the top 5 country-years)
gapminder %>% 
  filter(pop < 100000000) %>% 
  arrange(desc(gdpPercap)) %>% 
 # tail(n = 5) #tail end of the distribution
  head(n = 5)

# Calculate the (worldwide) average GDP per capita 
gapminder$gdpPercap_recoded <- gapminder$gdpPercap
gapminder$gdpPercap_recoded[1] <- NA
gapminder$gdpPercap[1]
mean(gapminder$gdpPercap)
mean(gapminder$gdpPercap_recoded)
mean(gapminder$gdpPercap_recoded, na.rm = T)
gapminder %>% 
  summarise(mean_gdp = mean(gdpPercap))

# Now do the (worldwide) average GDP per capita per year per continent
gapminder %>% 
  group_by(year, continent) %>% 
  summarise(mean_gdp = mean(gdpPercap))
df_gdp_mean_continent_year <- gapminder %>% 
  group_by(year, continent) %>% 
  summarise(mean_gdp = mean(gdpPercap))
View(df_gdp_mean_continent_year)

# We will assess the differences between mutate vs summarise
hist(gapminder$pop)
hist(gapminder$gdpPercap)
gapminder_transformed <- gapminder %>% 
  mutate(gdp_logged = log10(gdpPercap))
ncol(gapminder_transformed)
ncol(gapminder)
gapminder_transformed %>% 
  select(gdpPercap, gdp_logged)
hist(gapminder_transformed$gdp_logged)

