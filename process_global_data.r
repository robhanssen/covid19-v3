# 
# 
# 
# 
# 
library(tidyverse)
library(lubridate)
library(zoo)
source("config.r")

# main data is called global_casesdeaths
load("Rdata/global_casesdeaths")

locations = read_csv("sources/countryinformation.csv")

# summarize country info
global_casesdeaths %>% rename(province="Province/State", country="Country/Region") %>% 
                       group_by(country, date, time) %>% 
                       summarize(deaths=sum(deaths), cases=sum(cases)) %>%
                       full_join(locations, by=c(country="region")) -> casesdeaths


# world cases and deaths growth

worldpop = locations %>% group_by(continent) %>% summarize(worldpop=sum(population))

correction = 50

casesdeaths %>% 
                group_by(continent, date,time) %>%
                summarize(  cases=sum(cases), 
                            deaths=sum(deaths),
                            pop = sum(population),
                            casesper100k = cases/pop * 1e5,
                            deathsper100k = deaths/pop * 1e5                         
                            ) %>%
                            filter(casesper100k < 400) %>%
                ggplot() + aes(x=date, y=casesper100k) + geom_line(color="blue",lty=2) + facet_wrap(~continent) + 
                geom_line(aes(date,deathsper100k*correction), color="red", lty=2)