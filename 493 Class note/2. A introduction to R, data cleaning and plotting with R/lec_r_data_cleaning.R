## ----setup, include=FALSE------------------------------------------------------
knitr::opts_chunk$set(
  echo=TRUE, cache=FALSE, warning=FALSE, message=FALSE, 
  fig.align="center",
  fig.width=7,
  fig.height=3.5,
  out.width='99%'
)

# packages
library(fontawesome)
library(pacman)
p_load(tidyverse, gapminder)

# 
options(digits = 4)

# set transparent background 
theme_set(theme_grey())
theme_update(
  plot.background = element_rect(fill = "transparent", colour = NA),
  legend.background = element_rect(fill = "transparent", colour = NA)
)

# grey, blue, orange, green, yellow
my_colors <- c("#606060", "#3C78B0", "#D55E00", "#64B4C2", "#E69F00")

# set seed
set.seed(1234)



## ---- eval=FALSE---------------------------------------------------------------
## # load library
## library(gapminder)


## ------------------------------------------------------------------------------
# check out data
str(gapminder)


## ------------------------------------------------------------------------------
gapminder[1,] # First row


## ------------------------------------------------------------------------------
gapminder[1:3, 3:4] # first three rows, third and fourth column 


## ------------------------------------------------------------------------------
gapminder$gdpPercap[1:10]


## ------------------------------------------------------------------------------
gapminder[gapminder$year==1952, ]


## ------------------------------------------------------------------------------
head(gapminder$year==1952, 20) # display first 20 elements


## ------------------------------------------------------------------------------
vector_w_missing <- c(1, 2, NA, 4, 5, 6, NA)


## ------------------------------------------------------------------------------
mean(vector_w_missing)
mean(vector_w_missing, na.rm = TRUE)


## ------------------------------------------------------------------------------
vector_w_missing == NA


## ------------------------------------------------------------------------------
is.na(vector_w_missing)


## ------------------------------------------------------------------------------
mean(vector_w_missing[!is.na(vector_w_missing)])


## ----tverse, eval=FALSE--------------------------------------------------------
## #
## library(tidyverse)


## ----tverse_pkgs---------------------------------------------------------------
tidyverse_packages()


## ---- eval=FALSE---------------------------------------------------------------
## #
## library(dplyr)


## ------------------------------------------------------------------------------
log(mean(gapminder$pop))


## ------------------------------------------------------------------------------
gapminder$pop %>% mean() %>% log()


## ---- message=FALSE, warning=FALSE---------------------------------------------
#
gapminder %>%
  filter(country == "Canada") %>%
  head(4)


## ---- eval=FALSE---------------------------------------------------------------
## take_this_data %>%
##   do_first_thing(with = this_value) %>%
##   do_next_thing(using = that_value) %>% ...


## ---- eval=FALSE---------------------------------------------------------------
## gapminder %>% lm(pop ~ year, data = .)


## ---- eval=FALSE---------------------------------------------------------------
## lm_pop_year <- gapminder %>%
##   filter(continent == "Americas") %>%
##   lm(pop ~ year, data = .)


## ------------------------------------------------------------------------------
gapminder |> head(2)


## ------------------------------------------------------------------------------
gapminder %>%
  filter(country == "Canada") %>%
  head(4)


## ------------------------------------------------------------------------------
gapminder %>%
  filter(country == "Canada" & year > 1980) %>%
  head(4)


## ---- eval=FALSE---------------------------------------------------------------
## gapminder %>%
##   filter(country == "Canada" & year > 1980)


## ---- eval=FALSE---------------------------------------------------------------
## gapminder %>%
##   filter(country == "Canada" | year > 1980)


## ------------------------------------------------------------------------------
former_yugoslavia <- 
  c("Bosnia and Herzegovina", "Croatia","Montenegro", "Serbia", "Slovenia")
yugoslavia <- gapminder %>% 
  filter(country %in% former_yugoslavia)
tail(yugoslavia, 2)


## ------------------------------------------------------------------------------
yugoslavia %>%
  arrange(year, desc(pop)) %>%
  head(4)


## ------------------------------------------------------------------------------
yugoslavia %>%
  select(country, year, pop) %>%
  head(4)


## ------------------------------------------------------------------------------
yugoslavia %>%
  select(-continent, -pop, -lifeExp) %>%
  head(4)


## ------------------------------------------------------------------------------
gapminder %>%
  select(where(is.numeric)) %>%
  head(4)


## ------------------------------------------------------------------------------
yugoslavia %>%
  select(Life_Expectancy = lifeExp) %>%
  head(4)


## ------------------------------------------------------------------------------
yugoslavia %>%
  select(country, year, lifeExp) %>%
  rename(Life_Expectancy = lifeExp) %>%
  head(4)


## ------------------------------------------------------------------------------
yugoslavia %>% 
  filter(country == "Serbia") %>%
  select(year, pop, lifeExp) %>%
  mutate(pop_million = pop / 1000000) %>%
  head(2)


## ---- eval=FALSE---------------------------------------------------------------
## ifelse(test = x==y, yes = first_value , no = second_value)


## ------------------------------------------------------------------------------
example <- c(1, 0, NA, -2)
ifelse(example > 0, "Positive", "Not Positive")


## ------------------------------------------------------------------------------
yugoslavia %>%
  mutate(
    short_country = 
      ifelse(country == "Bosnia and Herzegovina","B and H", as.character(country))
  ) %>%
  select(country, short_country, year, pop) %>%
  arrange(year, short_country) %>%
  head(2)


## ------------------------------------------------------------------------------
gapminder %>%
  mutate(
    gdpPercap_ordinal = case_when(
      gdpPercap <  700 ~ "low",
      gdpPercap >= 700 & gdpPercap < 800 ~ "moderate",
      TRUE ~ "high" )
  ) %>%
  slice(6:9) # get rows 6 through 9


## ------------------------------------------------------------------------------
yugoslavia %>% filter(year==1982)


## ------------------------------------------------------------------------------
yugoslavia %>% filter(year==1982) %>%
  summarize(
    n_obs = n(),
    total_pop = sum(pop),
    mean_life_exp = mean(lifeExp),
    range_life_exp = max(lifeExp) - min(lifeExp)
  )


## ------------------------------------------------------------------------------
yugoslavia %>% filter(year==1982) %>%
  summarize(
    across(c(lifeExp, pop), list(avg = ~mean(.), sd = ~sd(.)))
  )


## ---- eval = FALSE-------------------------------------------------------------
## yugoslavia %>% filter(year==1982) %>%
##   summarize(
##     across(
##       c(lifeExp, pop),
##       list(avg = ~mean(.), sd  = ~sd(.))
##     )
##   )


## ------------------------------------------------------------------------------
yugoslavia %>% 
  group_by(year) %>%
  summarize(
    num_countries = n_distinct(country),
    total_pop = sum(pop)
  ) %>% head(3)


## ------------------------------------------------------------------------------
yugoslavia %>% select(country, year, pop) %>%
  filter(year >= 2002) %>%
  group_by(country) %>%
  mutate(lag_pop = lag(pop, order_by = year),
         pop_chg = pop - lag_pop) %>%
  head(4)


## ------------------------------------------------------------------------------
gapminder %>%
  group_by(continent, year) %>%
  summarize(mean_gdp = mean(gdpPercap)) %>%
  head(2)


## ------------------------------------------------------------------------------
gapminder %>%
  group_by(continent, year) %>%
  summarize(mean_gdp = mean(gdpPercap)) %>%
  ungroup() %>%
  head(2)


## ---- eval=FALSE---------------------------------------------------------------
## mean(swiss[swiss$Education > mean(swiss$Education), "Education"])


## ---- eval=FALSE---------------------------------------------------------------
## library(dplyr)
## swiss %>%
##   filter(Education > mean(Education)) %>%
##   summarize(mean = mean(Education))


## ----flights, echo = F---------------------------------------------------------
library(nycflights13)

## ---- eval = F-----------------------------------------------------------------
## library(nycflights13)
## flights
## planes


## ----join1---------------------------------------------------------------------
left_join(flights, planes) %>%
  select(year, month, day, dep_time, arr_time, carrier, flight, tailnum) %>%
  head(4)


## ----join2---------------------------------------------------------------------
left_join(
    flights,
    planes %>% rename(year_built = year), # not necessary w/ below line, but helpful
    by = "tailnum" # be specific about the joining column
  ) %>%
  select(year, month, day, dep_time, arr_time, carrier, flight, tailnum, year_built) %>%
  head(3)


## ------------------------------------------------------------------------------
stocks <-
  data.frame(
    time = as.Date('2009-01-01') + 0:1,
    X = rnorm(2, 0, 1),
    Y = rnorm(2, 0, 2),
    Z = rnorm(2, 0, 4)
  )
stocks


## ----pivot_longer1-------------------------------------------------------------
#
stocks %>%
  pivot_longer(-time, names_to = "stock", values_to = "price")


## ----pivot_longer2-------------------------------------------------------------
tidy_stocks <-
  stocks %>%
  pivot_longer(-time, names_to = "stock", values_to = "price")


## ----pivot_wider1--------------------------------------------------------------
tidy_stocks %>%
  pivot_wider(names_from = "stock", values_from = "price")


## ----pivot_wider2--------------------------------------------------------------
tidy_stocks %>%
  pivot_wider(names_from = "time", values_from = "price")


## ----sep1----------------------------------------------------------------------
economists <- 
  data.frame(name = c("Adam.Smith", "Paul.Samuelson", "Milton.Friedman"))
economists %>%
  separate(name, c("first_name", "last_name")) %>%
  head(1)


## ----sep2----------------------------------------------------------------------
jobs <- 
  data.frame(
    name = c("Jack", "Jill"),
    occupation = c("Homemaker", "Philosopher, Philanthropist, Troublemaker")
  )
jobs


## ----sep3----------------------------------------------------------------------
# split out Jill's various occupations into different rows
jobs %>% separate_rows(occupation)


## ----unite1--------------------------------------------------------------------
gdp <- 
  data.frame(
    yr = rep(2016, times = 4),
    mnth = rep(1, times = 4),
    dy = 1:4,
    gdp = rnorm(4, mean = 100, sd = 2)
  )
gdp


## ----unite2--------------------------------------------------------------------
# combine "yr", "mnth", and "dy" into one "date" column
gdp %>% unite(date, c("yr", "mnth", "dy"), sep = "-")


## ----unite3--------------------------------------------------------------------
gdp_u <- gdp %>%
  unite(date, c("yr", "mnth", "dy"), sep = "-") %>%
  as_tibble()
gdp_u


## ----unite4, message=F---------------------------------------------------------
library(lubridate)
gdp_u %>% mutate(date = ymd(date))


## ----cross1--------------------------------------------------------------------
crossing(side=c("left", "right"), height=c("top", "bottom"))


## ------------------------------------------------------------------------------
# packages
library(tsibble)
library(lubridate)


## ------------------------------------------------------------------------------
# Google mobility data
mobility_data <-
  read_csv("data/Global_Mobility_Report.csv",col_names = TRUE) %>%
  filter(country_region == "Canada")


## ------------------------------------------------------------------------------
# sub-region codes
region_code <- c('CA-BC', 'CA-AB', 'CA-ON', 'CA-QC')
#
data <- mobility_data %>%
  filter(iso_3166_2_code %in% region_code) %>%
  select(
    date,
    province = iso_3166_2_code,
    retail = retail_and_recreation_percent_change_from_baseline,
    transit = transit_stations_percent_change_from_baseline,
    workplace = workplaces_percent_change_from_baseline,
    residential = residential_percent_change_from_baseline
  )


## ------------------------------------------------------------------------------
#
head(data, 6)


## ------------------------------------------------------------------------------
# tidy
data_daily <- data %>%
  pivot_longer(
    c("retail","transit","workplace","residential"),
    names_to = "index",
    values_to = "value"
  ) %>%
  group_by(province, index)


## ------------------------------------------------------------------------------
#
head(data_daily, 6)


## ------------------------------------------------------------------------------
# now we can plot the daily indexes
p1 <-
  ggplot(data_daily, aes(x = date, y = value, color = province)) +
  geom_line(size = .5) +
  scale_y_continuous(name = "", limits = c(-100,50), breaks = c(50,0,-50,-100)) +
  scale_x_date(name = "", date_breaks = "6 month", date_labels = "%m/%y") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  facet_wrap( ~ index)


## ---- echo=F, fig.height=4.8---------------------------------------------------
#
p1


## ------------------------------------------------------------------------------
#
data_weekly <- data_daily %>%
  mutate(y_week = yearweek(date)) %>%
  group_by(province, index, y_week) %>%
  summarize(
    date = first(date),
    y_week = first(y_week),
    province = first(province),
    index = first(index),
    value = mean(value)
  ) %>%
  arrange(province, index)


## ------------------------------------------------------------------------------
#
head(data_weekly, 6)


## ---- echo=F, fig.height=4.8---------------------------------------------------
# now we can plot the weekly indexes
p2 <-
  ggplot(data_weekly, aes(x = date, y = value, color = province)) +
  geom_line(size = .5) +
  scale_y_continuous(name = "", limits = c(-100,50), breaks = c(50,0,-50,-100)) +
  scale_x_date(name = "", date_breaks = "6 month", date_labels = "%m/%y") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  facet_wrap( ~ index)
#
p2

