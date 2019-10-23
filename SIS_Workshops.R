# Basic concepts

1 + 2

x <- 1 + 2

y <- c(1, 2, 3, 4)


# Set working directory and load libraries
setwd("Research communication/Workshops/R SIS Seminar 2019/")
library(tidyverse)
library(plyr)
library(janitor)
library(reshape2)

# Importing data
raw.data <- read_csv("library stats.csv", locale=locale(encoding = "UTF-8"))

names(), summary(), View(), head()


# Selecting libraries
hist(raw.data$hours, breaks = 25)
plot(raw.data$staff.total)
plot(raw.data$surface)

raw.data %>% 
  filter(is.na(surface)) %>% 
  dplyr::count(library)


# Building a clean data set
clean.data <- raw.data %>% 
  select(-music, -audiobooks, -movies, -videogames) %>% 
  filter(!is.na(surface)) %>% 
  mutate(
    ave.circulation=books/loans,
    ave.borrows=books/members
  )


# Summarise the data at the borough level
clean.data %>% 
  ddply(c("borough", "year"), summarise,
        loans=sum(loans),
        staff.total=sum(staff.total),
        books=sum(books)) %>% 
  dplyr::arrange(desc(loans))


# Access climate data
emissions <- tempfile()
download.file("https://www150.statcan.gc.ca/n1/tbl/csv/38100097-eng.zip", emissions)

raw.env.data <- unz(emissions, "38100097.csv") %>% read_csv() 

# Clean climate data
clean.env.data <- raw.env.data %>% 
  clean_names() %>% 
  dplyr::rename(
    year=ref_date,
    region=geo
  ) %>% 
  select(year, region, sector, value) %>% 
  mutate(
    block=case_when(
      region %in% c("Canada") ~ "National",
      region %in% c("New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Prince Edward Island") ~ "Atlantic",
      region %in% c("Quebec", "Ontario") ~ "Central",
      region %in% c("Alberta", "Saskatchewan", "Manitoba") ~ "Prairies",
      region %in% c("British Columbia") ~ "Pacific",
      region %in% c("Northwest Territories", "Nunavut", "Yukon") ~ "North"
    )
  )

ghg.summary <- clean.env.data %>% 
  filter(sector=="Total, industries and households") %>% 
  filter(year==2016) %>% 
  dplyr::arrange(desc(value))

clean.env.data %>% 
  filter(sector=="Total, industries and households") %>% 
  filter(block!="National") %>% 
  ddply(c("year", "block"), summarise,
        ave.emissions=mean(value)) %>% 
  filter(year==2017)


# Access population data
raw.pop.data <- read_csv("population.csv", 
                         col_names = c("year", "code", "level", "region", "gnr", "gnr_lf", "quality", "geo_code", "dim", "mid", "notes", "total", "male", "female"),
                         skip=1
                         )

head(raw.pop.data)
table(raw.pop.data$dim)

province.population <- raw.pop.data %>% 
  filter(dim=="Population, 2016") %>% 
  select(region, total)


# Join data sets
anti_join(ghg.summary, province.population, by="region")

test <- ghg.summary %>% 
  add_row(year=2017, region="Elbonia", sector="Total, industries and households", value=19109, block="Outside")
anti_join(test, province.population, by="region")
anti_join(province.population, test, by="region")

ghg.summary <- full_join(ghg.summary, province.population, by="region") %>% 
  mutate(percapita.emissions=value/total)

ghg.summary %>% 
  dplyr::arrange(desc(percapita.emissions))
