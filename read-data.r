#
# R script to analyze spread of COVID-19, data from Johns Hopkids via github
# Github: https://github.com/CSSEGISandData/COVID-19
#

#
# load the required libraries
#
library(tidyverse)
library(lubridate)
source("config.r")

#
# import global infections via web API
#

infocolumnnames = c("Province/State","Country/Region", "Lat","Long")
infocols = length(infocolumnnames)

global_cases_raw <- read_csv(global_infections_file)

global_cases <- global_cases_raw %>% 
                     tidydata(., dataname="cases", exclusionvector=infocolumnnames) 

global_cases_growth <- global_cases_raw %>% 
                     dailydifference(., infocols) %>% 
                     tidydata(., dataname="cases", exclusionvector=infocolumnnames) %>% 
                     mutate(time=time+1)

#
# import global deaths via web API
#

global_deaths_raw <- read_csv(global_deaths_file)

global_deaths <- global_deaths_raw %>% 
                     tidydata(., dataname="deaths", exclusionvector=infocolumnnames) 


global_deaths_growth <- global_deaths_raw %>% 
                     dailydifference(., infocols) %>%
                     tidydata(., dataname="deaths", exclusionvector=infocolumnnames) %>% 
                     mutate(time=time+1)

# combining all global data
global_casesdeaths <- global_cases %>% 
                     inner_join(global_deaths)

save(global_casesdeaths, file="Rdata/global_casesdeaths.Rdata")

#
# import US cases via web API
#

infocolumnnames = c("UID","iso2","iso3","code3","FIPS","Admin2","Province_State","Country_Region","Lat","Long_","Combined_Key")
infocols = length(infocolumnnames)

us_cases_raw <- read_csv(us_cases_file)

us_cases <- us_cases_raw %>%
                     tidydata(., dataname="cases", exclusionvector=infocolumnnames) 

us_cases_growth <- us_cases_raw %>%
                     dailydifference(., infocols) %>% 
                     tidydata(., dataname="cases", exclusionvector=infocolumnnames) %>%
                     mutate(time = time + 1)
# 
# import US deaths via web API
# 

infocolumnnames = c("UID","iso2","iso3","code3","FIPS","Admin2","Province_State","Country_Region","Lat","Long_","Combined_Key", "Population")
infocols = length(infocolumnnames)

us_deaths_raw <- read_csv(us_deaths_file) 

us_deaths <-  us_deaths_raw %>% 
                     tidydata(., dataname="deaths", exclusionvector=infocolumnnames) 

us_deaths_growth <- us_deaths_raw %>% 
                     dailydifference(., infocols) %>%
                     tidydata(., dataname="deaths", exclusionvector=infocolumnnames) %>%
                     mutate(time = time + 1)



# combining all US data
us_casesdeaths = us_cases_growth %>% 
                     inner_join(us_deaths_growth, by=c("UID","date", "Province_State")) %>%
                     select("Admin2.x", "Province_State",date,time.x,Population,cases,deaths) %>% 
                     rename(county="Admin2.x", state="Province_State",time="time.x",population=Population)

# View(us_casesdeaths %>% head(100))

save(us_casesdeaths, file="Rdata/us_casesdeaths.Rdata")