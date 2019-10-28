# Set working directory and load libraries
setwd("~/Research communication/Workshops/R SIS Seminar 2019/")
library(tidyverse)
library(hexbin)
library(plyr)
library(janitor)
library(reshape2)



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
  select(year, region, sector, value) 

ghg.summary <- clean.env.data %>% 
  filter(sector=="Total, industries and households") %>% 
  filter(year==2016) %>% 
  dplyr::arrange(desc(value))


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



# Visualizing the data



# Load NYC housing sales data set (1=Manhattan, 2=Bronx; 3=Brooklyn; 4=Queens; 5=Staten Island)
nyc.property <- read_csv(
  "nyc-rolling-sales.csv"
)


# Descriptive statistics: measures of central tendancy



# Descriptive statistics: measures of dispersion


# Descriptive stats by borough



# Visualize price range by land size







