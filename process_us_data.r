# 
# 
# 
# 
# 
library(tidyverse)
library(lubridate)
library(zoo)
source("config.r")

load("Rdata/us_casesdeaths.Rdata")

# assign the region in the US to all locations
locations = read_csv("sources/USstateslist.csv")
us_casesdeaths <- us_casesdeaths %>% 
                        full_join(locations) %>%
                        mutate(location = ifelse(is.na(location), "Other", location))




correction = 60
avdays = 7
capt="insert caption here"

us_casesdeaths %>% group_by(date) %>%
                        summarize(population = sum(population),
                                  cases = sum(cases),
                                  deaths = sum(deaths),
                                  casesper100k = cases / population * 1e5,
                                  deathsper100k = deaths / population * 1e5
                                  ) %>% ungroup() -> casesdeaths

totalcases = sum(casesdeaths$cases, na.rm=T)
totaldeaths = sum(casesdeaths$deaths, na.rm=T)
totalcasecomment = paste("Total US cases:",totalcases,"\nTotal casualties:", totaldeaths)


casesdeaths %>% filter( date>="2020-02-01") %>%
                ggplot + aes(date, casesper100k) + geom_line(color="blue", linetype="dotted") + geom_line(aes(y=rollmean(casesper100k,avdays, na.pad=TRUE)), size=2, color="blue") + 
                        scale_y_continuous(sec.axis = sec_axis(~ ./correction, breaks=seq(0,5,1))) + #scale_y_log10(limit=c(10,100000))+ 
                        scale_x_date(date_breaks="3 months", date_labels = "%b %d") + #facet_wrap(~location) + 
                        labs(caption=capt) + xlab("Date") + ylab("Daily incremental number of confirmed cases or deaths") +
                        ggtitle(paste("US daily cases and deaths with", avdays,"days average line")) + 
                        geom_line(aes(date, correction*deathsper100k), color="red", linetype="dotted")  + geom_line(aes(y=rollmean(correction*deathsper100k,avdays,na.pad=TRUE)), size=2, color="red") +
                        #annotate("text",x=as.Date("2020-03-15", format="%Y-%m-%d"),y=20000,label="cases\n<-----", color="blue") + 
                        #annotate("text",x=as.Date("2020-04-10", format="%Y-%m-%d"),y=10000,label="deaths\n------>", color="red") +
                        annotate("text",x=as.Date("2020-03-28", format="%Y-%m-%d"),y=70,label=totalcasecomment, color="black")

ggsave(paste0("graphs/covid19-us-cases-and-death.pdf"))

# 
# 
# data per US region 
# 
# 

us_casesdeaths %>% group_by(date,location) %>%
                        summarize(population = sum(population),
                                  cases = sum(cases),
                                  deaths = sum(deaths),
                                  casesper100k = cases / population * 1e5,
                                  deathsper100k = deaths / population * 1e5
                                  ) %>% ungroup() -> casesdeathsbylocation


regionlist = locations %>% distinct(location)

for(region in regionlist$location) 
{
        casesdeathsbylocation %>% filter( location == region) %>%
                ggplot + aes(date, casesper100k) + geom_line(color="blue", linetype="dotted") + 
                        geom_line(aes(y=rollmean(casesper100k,avdays, na.pad=TRUE)), size=2, color="blue") + 
                        scale_y_continuous(sec.axis = sec_axis(~ ./correction, breaks=seq(0,5,1))) + 
                        scale_x_date(date_breaks="3 months", date_labels = "%b %d") + 
                        labs(caption=capt, x="Date", y="Daily incremental number of confirmed cases or deaths") + 
                        ggtitle(paste(region, "daily cases and deaths with", avdays,"days average line")) + 
                        geom_line(aes(date, correction*deathsper100k), color="red", linetype="dotted")  + 
                        geom_line(aes(y=rollmean(correction*deathsper100k,avdays,na.pad=TRUE)), size=2, color="red") 

        ggsave(paste0("graphs/covid19-",region,"cases-and-deaths.pdf"))
        
}



# 
# 
# data per for South Carolina
# 
# 
us_casesdeaths %>% filter(state=="South Carolina") %>%
                        group_by(date) %>%
                        summarize(population = sum(population),
                                  cases = sum(cases),
                                  deaths = sum(deaths),
                                  casesper100k = cases / population * 1e5,
                                  deathsper100k = deaths / population * 1e5
                                  ) %>% ungroup() -> casesdeathsbylocation


casesdeathsbylocation %>% filter(date > today() - months(6)) %>%
                ggplot + aes(date, casesper100k) + geom_line(color="blue", linetype="dotted") + 
                        geom_line(aes(y=rollmean(casesper100k,avdays, na.pad=TRUE)), size=2, color="blue") + 
                        scale_y_continuous(limit=c(0,150), sec.axis = sec_axis(~ ./correction, breaks=seq(0,5,1))) + 
                        scale_x_date(date_breaks="3 months", date_labels = "%b %d") + 
                        labs(caption=capt, x="Date", y="Daily incremental number of confirmed cases or deaths") + 
                        ggtitle(paste("South Carolina daily cases and deaths with", avdays,"days average line")) + 
                        geom_line(aes(date, correction*deathsper100k), color="red", linetype="dotted")  + 
                        geom_line(aes(y=rollmean(correction*deathsper100k,avdays,na.pad=TRUE)), size=2, color="red") 

ggsave("graphs/covid19-SC-cases-and-deaths.pdf")
        

# 
# 
# data per for South Carolina (Spartanburg/Greenville)
# 
# 
us_casesdeaths %>% filter(state=="South Carolina") %>%
                        filter(county=="Spartanburg" | county=="Greenville") %>%
                        filter(date > today() - months(6)) %>%
                        group_by(date) %>%
                        summarize(population = sum(population),
                                  cases = sum(cases),
                                  deaths = sum(deaths),
                                  casesper100k = cases / population * 1e5,
                                  deathsper100k = deaths / population * 1e5
                                  ) %>% ungroup() -> casesdeathsbylocation


casesdeathsbylocation %>% #filter( location == region) %>%
                ggplot + aes(date, casesper100k) + geom_line(color="blue", linetype="dotted") + 
                        geom_line(aes(y=rollmean(casesper100k,avdays, na.pad=TRUE)), size=2, color="blue") + 
                        scale_y_continuous(sec.axis = sec_axis(~ ./correction, breaks=seq(0,5,1))) + 
                        scale_x_date(date_breaks="1 months", date_labels = "%b %d") + 
                        labs(caption=capt, x="Date", y="Daily incremental number of confirmed cases or deaths") + 
                        ggtitle(paste("Greenville/Spartanburg daily cases and deaths with", avdays,"days average line")) + 
                        geom_line(aes(date, correction*deathsper100k), color="red", linetype="dotted")  + 
                        geom_line(aes(y=rollmean(correction*deathsper100k,avdays,na.pad=TRUE)), size=2, color="red") 

ggsave("graphs/covid19-SCGSP-cases-and-deaths.pdf")
