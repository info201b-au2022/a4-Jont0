library(tidyverse)
library(dplyr)
library(ggplot2)

#code to read incarceration data
df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
View(df)

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
male_jail_2000 <- df %>%
  filter(year == "2000") %>%
  select(male_jail_pop) %>%
  summarise(male_jail_pop = sum(male_jail_pop))
print(male_jail_2000)

#In the year 2000, how many inmates were black?
black_male_incarceration_2000 <- df %>%
  filter(year == "2000") %>%
  select(black_prison_pop) %>%
  summarise(black_prison_pop = sum(black_prison_pop))
print(black_male_incarceration_2000)

#In the year 2000 how many white inmates were incarcerated
white_incarceration_2000 <- df %>%
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
  total_population <- df %>%
    group_by(year) %>%
    filter(total_jail_pop != "NA") %>%
    summarise(total_jail_pop =sum(total_jail_pop))
  return(total_population)   
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  ggplot(get_year_jail_pop(), aes(x = year, y = total_jail_pop)) +
    geom_col(position = "dodge") +
    labs(title = "U.S. Jail Population Distribution (1970-2018)",
         caption = "Figure 1. Representation of the changing Jail Population in the United States between the years 1970 to 2018.",
         x = "Year",
         y = "Total Inmate Population")
  
  # TODO: Implement this function 
} 
plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

get_jail_pop_by_states <- function(states) {
  visualization_df <- df %>%
    filter(state %in% states) %>%
    group_by(year, state) %>%
    summarise(total_jail_population = sum(total_jail_pop, na.rm = TRUE))
  return(visualization_df)
}

get_jail_pop_by_states(c("AL", "MD", "NY", "SD", "TN", "WA", "CA"))

plot_jail_pop_by_states <- function(states) {
  plot_df <- get_jail_pop_by_states(states)
  
  plot <- ggplot(data = plot_df) +
    geom_line(aes(x = year, y = total_jail_population, color = state)) +
    labs(
      title = "Different State Jail Population (1970-2018)", +
    caption = "Figure.2 Visualization of an array of State Jail Population between the years 1970 to 2018.") +
    ylab("Total Jail Population") + 
    xlab("Years") +
    guides(fill = guide_legend(title = "States")) +
    scale_y_continuous(labels = scales::comma)
  return(plot)
}
options(dplyr.summarise.inform = FALSE)
plot_jail_pop_by_states(c("AL", "MD", "NY", "SD", "TN", "WA", "CA"))

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
region_plus_total_white_jail_pop <- function() {
  dv <- df %>%
    drop_na() %>%
    group_by(region) %>%
    summarize(white_jail_population = sum(white_jail_pop)) %>%
    select(region, white_jail_population)
  return(dv)
}

region_plus_total_white_jail_pop()

region_plus_total_black_jail_pop <- function() {
  dv <- df %>%
    drop_na() %>%
    group_by(region) %>%
    summarize(black_jail_population = sum(black_jail_pop)) %>%
    select(region, black_jail_population)
  return(dv)
}

region_plus_total_black_jail_pop()

region_black_and_white_data <- left_join(region_plus_total_black_jail_pop(), region_plus_total_white_jail_pop())

white_black_data <- region_black_and_white_data %>%
  select(region, white_jail_population, black_jail_population) %>%
  gather(key = race, value = population, -region)

#code for actual plot

white_black_plot <- function() {
  ggplot(white_black_data) +
    geom_col(
      mapping = aes(x = population, y = region, fill = race), position = "dodge"
    ) +
    labs(
      title = "White vs Black Jail Population by Region",
      caption = "Figure 3. Connection between the White and Black Inmates by the four regions."
    ) +
    xlab("Population") +
    ylab("Region") +
    guides(fill = guide_legend(title = "Race")) +
    scale_fill_hue(labels = c("White Population", "Black Population")) 
}

white_black_plot()

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 

texas_data <- function() {
  all_texas_pop <- df %>%
    filter(state == "TX") %>%
    filter(year == "2018") %>%
    select(total_jail_pop) %>%
    summarise(total_jail_pop = sum(total_jail_pop)) %>%
    return(all_texas_pop)
}

black_texas_pop <- function() {
  btexas_pop <- df %>%
    filter(state == "TX") %>%
    filter(year == "2018") %>%
    select(total_jail_pop) %>%
    group_by(black_jail_pop) %>%
    summarise(black_jail_pop = sum(black_jail_pop))
  return(btexas_pop)
}

plot_texas <- ggplot(data = texas_data, values = "Total Texans Incarcerated", color = "white") +
  scale_fill_continuous(name = "Texans in Jail (2018)", label = scales::comma) +
  theme(legend.position = "right")
labs(title = "Map of Black Texans Imprisioned vs Total Amount of Texans",
     caption = "Figure 4. Illustrates the difference between the number of Imprisioned Black Texans to All Texans")
plot_texas
