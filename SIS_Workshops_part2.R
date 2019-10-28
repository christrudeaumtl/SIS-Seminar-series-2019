# Set working directory and load libraries
setwd("~/Research communication/Workshops/R SIS Seminar 2019/")
library(tidyverse)
library(plyr)
library(janitor)
library(reshape2)
library(psych)



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
anti_join(ghg.summary, province.population, by="region")

test <- ghg.summary %>% 
  add_row(year=2017, region="Elbonia", sector="Total, industries and households", value=19109, block="Outside")
anti_join(test, province.population, by="region")
anti_join(province.population, test, by="region")

ghg.summary <- full_join(ghg.summary, province.population, by="region") %>% 
  mutate(percapita.emissions=value/total)

ghg.summary %>% 
  dplyr::arrange(desc(percapita.emissions))


# Visualizing the data
ghg.summary %>% 
  mutate(
    region=fct_reorder(region, percapita.emissions)
  ) %>% 
  ggplot(
    aes(
      x=region,
      y=percapita.emissions
    )
  ) +
  geom_col() +
  coord_flip()


# Load NYC housing sales data set (1=Manhattan, 2=Bronx; 3=Brooklyn; 4=Queens; 5=Staten Island)
nyc.property <- read_csv(
  "nyc-rolling-sales.csv", 
  col_types = cols(BOROUGH=col_factor(levels=c(1, 2, 3, 4, 5))),
  na=c("N/A", "-")
) %>% 
  clean_names() %>% 
  select(-x1) %>% 
  mutate(
    borough=mapvalues(
      borough, 
      c(1, 2, 3, 4, 5), 
      c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island")
    ),
    price_square_foot=sale_price/gross_square_feet,
    sale_month=lubridate::month(sale_date)
  )


# Descriptive statistics: measures of central tendancy
min(nyc.property$sale_price, na.rm=T)
max(nyc.property$sale_price, na.rm=T)
mean(nyc.property$sale_price, na.rm = T)
median(nyc.property$sale_price, na.rm = T)


# Descriptive statistics: measures of dispersion
sd(nyc.property$sale_price, na.rm=T)


# Descriptive stats by borough
nyc.property %>% 
  ddply(~borough, summarise,
        mean=mean(sale_price, na.rm=T),
        median=median(sale_price, na.rm=T),
        sd=sd(sale_price, na.rm=T)) %>% 
  dplyr::arrange(mean)


# Visualize price range by land size
nyc.property %>% 
  filter(sale_price<mean(sale_price, na.rm=T)+2*sd(sale_price, na.rm=T)) %>% 
  filter(gross_square_feet<mean(gross_square_feet, na.rm=T)+2*sd(gross_square_feet, na.rm=T)) %>% 
  filter(price_square_foot>10) %>% 
  ggplot(
    aes(
      x=gross_square_feet,
      y=sale_price
    )
  ) +
  geom_hex(bins=50) +
  geom_smooth(
    method="lm",
    se=F,
    colour="red"
  ) +
  facet_wrap(~borough) +
  scale_y_continuous(labels=scales::comma) +
  labs(
    x="Square footage",
    y="Sale price",
    fill="Count"
  )

nyc.property %>% 
  filter(sale_price<mean(sale_price, na.rm=T)+2*sd(sale_price, na.rm=T)) %>% 
  filter(gross_square_feet<mean(gross_square_feet, na.rm=T)+2*sd(gross_square_feet, na.rm=T) & gross_square_feet>0) %>% 
  filter(price_square_foot>10) %>% 
  ggplot(
    aes(
      x=gross_square_feet,
      y=sale_price/1000000,
      colour=building_class_category
    )
  ) +
  geom_point() +
  geom_smooth(
    method="lm",
    se=F,
    colour="red"
  ) +
  facet_wrap(~borough) +
  scale_y_continuous(labels=scales::comma) +
  labs(
    x="Square footage",
    y="Sale price, in millions",
    colour="Building category"
  )


# Visualize the range in prices by borough
nyc.property %>% 
  filter(sale_price<mean(sale_price, na.rm=T)+sd(sale_price, na.rm=T)) %>% 
  ggplot(
    aes(
      x=borough,
      y=sale_price
    )
  ) +
  geom_boxplot()


nyc.property %>% 
  filter(sale_price<mean(sale_price, na.rm=T)+sd(sale_price, na.rm=T)) %>% 
  ggplot(
    aes(
      x=borough,
      y=sale_price
    )
  ) +
  geom_violin() +
  labs(
    x=NULL,
    y="Sale price",
    title="Housing price range per borough",
    subtitle="Sept. 01, 2016 to Aug. 31, 2017"
  ) +
  scale_y_continuous(labels=scales::comma)

nyc.property %>% 
  filter(sale_price<mean(sale_price, na.rm=T)+sd(sale_price, na.rm=T)) %>% 
  ggplot(
    aes(
      x=borough,
      y=sale_price,
      fill=borough
    )
  ) +
  geom_violin(
    draw_quantiles = c(0.25, 0.5, 0.75)
  ) +
  labs(
    x=NULL,
    y="Sale price",
    title="Housing price range per borough",
    subtitle="Sept. 01, 2016 to Aug. 31, 2017"
  ) +
  scale_y_log10(labels=scales::comma)


# Look at sales over the course of the year
nyc.property %>%
  ggplot(
    aes(
      x=as.factor(sale_month),
      fill=building_class_category
    )
  ) +
  geom_bar(position="dodge") +
  scale_y_discrete(
    labels=c(1:12)
  ) +
  labs(
    x="Month",
    y="Number of sales"
  )