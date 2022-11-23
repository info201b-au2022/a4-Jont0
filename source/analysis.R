library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#
#
#How many males in the year 2000 were incarcerated in jail
male_jail_2000 <- incarceration_data %>%
  filter(year == "2000") %>%
  select(male_jail_pop) %>%
  summarise(male_jail_pop = sum(male_jail_pop))
print(male_jail_2000)

#In the year 2000, how many inmates were black?
black_male_incarceration_2000 <- incarceration_data %>%
  filter(year == "2000") %>%
  select(black_prison_pop) %>%
  summarise(black_prison_pop = sum(black_prison_pop))
print(black_male_incarceration_2000)

#In the year 2000 how many white inmates were incarcerated
white_incarceration_2000 <- incarceration_data %>%
  filter(year == "2000") %>%
  select(white_prison_pop) %>%
  summarise(white_prison_pop = sum(white_prison_pop))
print(white_incarceration_2000)

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  # TODO: Implement this function 
  total_population <- 
return()   
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  # TODO: Implement this function 
  return()   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


