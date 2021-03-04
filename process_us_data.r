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
#  data per state
# 
# 

colorset = c(  "Safe: 0-2 per 100k" = "darkgreen", 
               "Impacted: 2-5 per 100k" = "lightgreen", 
               "Moderate: 5-10 per 100k"="yellow", 
               "Severe: 10-20 per 100k"="orange",
               "Critical: >20 per 100k"="red", 
               "Supercritical: >50 per 100k" ="purple",
               "Grim Reaper: >100 per 100k" ="black")

us_casesdeaths %>% group_by(date,state) %>%
                        summarize(population = sum(population),
                                  cases = sum(cases),
                                  deaths = sum(deaths),
                                  casesper100k = cases / population * 1e5,
                                  deathsper100k = deaths / population * 1e5
                                  ) %>% ungroup() -> casesdeathsbystate

datecutoff = today() - days(7)

daterange = paste0("Data from ", format(datecutoff, format="%b %d"), " to ", format(today(), format="%b %d"))

caption = paste0(source,"\n",daterange)

casesdeathsbystate %>% filter(date > datecutoff) %>% group_by(state) %>%
                       summarize(casesper100k = mean(casesper100k, na.rm=TRUE),
                                 deathsper100k = mean(deathsper100k, na.rm=TRUE)
                                 ) %>%
                        filter(!is.na(casesper100k), !is.na(deathsper100k)) %>%
                        mutate(level = cut(casesper100k, 
                                           breaks=c(-1,2,5,10,20, 50, 100, 1e5),
                                           labels=c("Safe: 0-2 per 100k",
                                                     "Impacted: 2-5 per 100k",
                                                     "Moderate: 5-10 per 100k",
                                                     "Severe: 10-20 per 100k",
                                                     "Critical: >20 per 100k", 
                                                     "Supercritical: >50 per 100k",
                                                     "Grim Reaper: >100 per 100k"
                                                     )
                                            )
                                ) -> ratesbystate7days

ratesbystate7days %>% ggplot + aes(x=fct_reorder(state,casesper100k), y=casesper100k, fill=level) + 
                               scale_y_continuous(breaks=c(2,5,10,20,50,100)) +
                               geom_bar(stat="identity") + 
                               labs(x="State", y="Daily new infection per 100,000 population", caption=caption) +
                               coord_flip() + 
                               scale_fill_manual(values=colorset)

ggsave(paste0("graphs/covid19-casesbystate_ranking.pdf"))


statemapdata <- as_tibble(map_data("state")) %>% 
                        rename(state=region) %>%
                        inner_join(ratesbystate7days %>% mutate(state=tolower(state))) 

ggplot(data = statemapdata) + 
  geom_polygon(aes(x = long, y = lat, fill = level, group = group), color = "white") + scale_fill_manual(values=colorset) + 
  ggtitle("Week-average daily infection rate across the United States (in new infections/day)") + 
  labs(fill="Infection level", caption=caption) +
  coord_fixed(1.4) 

ggsave("graphs/covid19_usmap_infections.pdf", width=11, height=8)

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

write_csv(casesdeathsbylocation, "data/sc-casesdeath.csv")
ggsave("graphs/covid19-SC-cases-and-deaths.pdf")
        
casesdeathsbylocation %>% filter(date > as.Date("2021-01-21")) %>% 
                        ggplot + aes(x=date, y=casesper100k) + geom_point() + geom_smooth(method="lm", fullrange=TRUE) + 
                        scale_y_continuous(limit=c(-50,100), breaks=seq(0,100,10)) + 
                        scale_x_date(breaks="2 weeks", date_labels="%b %d", limit=as.Date(c("2021-01-21","2021-04-07"))) +
                        labs(x="Date", y="Cases per 100k population", title="Cases in South Carolina", subtitle="Cases per 100,000") +
                        geom_hline(yintercept=10, lty=2) +
                        geom_hline(yintercept=5, lty=3) +
                        geom_hline(yintercept=0, lty=1) +
                        geom_vline(xintercept=as.Date("2021-03-09"),lty=2) + geom_vline(xintercept=as.Date("2021-04-02"),lty=2) 
ggsave("graphs/covid19-SC-casesextrapolation.pdf")
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
