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
load("Rdata/global_casesdeaths.Rdata") 

locations = read_csv("sources/countryinformation.csv")

# summarize country info
global_casesdeaths %>% rename(province="Province/State", country="Country/Region") %>% 
                       group_by(country, date, time) %>% 
                       summarize(deaths=sum(deaths), cases=sum(cases)) %>%
                       full_join(locations, by=c(country="region")) -> casesdeaths
correction = 50
avdays=7
totalcasecomment=""
capt="insert caption here"
# world cases and deaths growth
casesdeaths %>% group_by(date,time) %>%
                summarize(cases=sum(cases), 
                          deaths=sum(deaths),
                          population=sum(population, na.rm=T),
                          casesper100k = cases/population * 1e5,
                          deathsper100k = deaths/population * 1e5
                          ) %>% 
                ggplot + aes(date, casesper100k) + geom_line(color="blue", linetype="dotted") + geom_line(aes(y=rollmean(casesper100k,avdays, na.pad=TRUE)), size=2, color="blue") + 
                        scale_y_continuous(sec.axis = sec_axis(~ ./correction, breaks=seq(0,5,1))) + #scale_y_log10(limit=c(10,100000))+ 
                        scale_x_date(date_breaks="3 months", date_labels = "%b %d") + #facet_wrap(~location) + 
                        labs(caption=capt) + xlab("Date") + ylab("Daily incremental number of confirmed cases or deaths") +
                        ggtitle(paste("US daily cases and deaths with", avdays,"days average line")) + 
                        geom_line(aes(date, correction*deathsper100k), color="red", linetype="dotted")  + geom_line(aes(y=rollmean(correction*deathsper100k,avdays,na.pad=TRUE)), size=2, color="red") +
                        #annotate("text",x=as.Date("2020-03-15", format="%Y-%m-%d"),y=20000,label="cases\n<-----", color="blue") + 
                        #annotate("text",x=as.Date("2020-04-10", format="%Y-%m-%d"),y=10000,label="deaths\n------>", color="red") +
                        annotate("text",x=as.Date("2020-03-28", format="%Y-%m-%d"),y=20,label=totalcasecomment, color="black")


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