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
                        ggtitle(paste("Global daily cases and deaths with", avdays,"days average line")) + 
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


datecutoff = today() - days(7)

casesdeaths %>% filter(date > datecutoff, population > 5e6) %>% group_by(country) %>%
                summarize(cases=sum(cases), 
                          deaths=sum(deaths),
                          population=sum(population, na.rm=T),
                          casesper100k = mean(cases, na.rm=TRUE)/population * 1e5,
                          deathsper100k = mean(deaths, na.rm=TRUE)/population * 1e5
                          ) %>% top_n(30, casesper100k) -> casesdeath_top30

casesdeath_top30 %>% ggplot + aes(x=fct_reorder(country,casesper100k), y=casesper100k) + 
                        geom_bar(stat="identity") + 
                        labs(x="Country with population > 5 million", y="Average cases per day per 100k") +
                        coord_flip()


colorset = c(  "Safe: 0-2 per 100k" = "darkgreen", 
               "Impacted: 2-5 per 100k" = "lightgreen", 
               "Moderate: 5-10 per 100k"="yellow", 
               "Severe: 10-20 per 100k"="orange",
               "Critical: >20 per 100k"="red", 
               "Supercritical: >50 per 100k" ="purple",
               "Grim Reaper: >100 per 100k" ="black")

daterange = paste0("Data from ", format(datecutoff, format="%b %d"), " to ", format(today(), format="%b %d"))

caption = paste0(source,"\n",daterange)

casesdeaths %>% filter(date > datecutoff) %>% group_by(country) %>%
                summarize(cases=sum(cases), 
                          deaths=sum(deaths),
                          population=sum(population, na.rm=T),
                          casesper100k = mean(cases, na.rm=TRUE)/population * 1e5,
                          deathsper100k = mean(deaths, na.rm=TRUE)/population * 1e5
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
                                ) -> ratesbycountry7days

ratesbycountry7days %>% filter(!is.na(level), population > 5e6) %>% top_n(30,casesper100k) %>%
                        ggplot + aes(x=fct_reorder(country,casesper100k), y=casesper100k, fill=level) + 
                               scale_y_continuous(breaks=c(2,5,10,20,50,100)) +
                               geom_bar(stat="identity") + 
                               labs(x="Countries with population over 5 million", y="Daily new infection per 100,000 population", caption=caption) +
                               coord_flip() + 
                               scale_fill_manual(values=colorset)

top20countrylist <- ratesbycountry7days %>% 
                        filter(!is.na(level), population > 5e6) %>% 
                        arrange(-casesper100k) %>% 
                        mutate(rank = row_number()) %>%
                        top_n(20,casesper100k) 

ggsave(paste0("graphs/covid19-casesbycountry_ranking.pdf"), width=8, height=11)

ratesbycountry7days$country[ratesbycountry7days$country=="US"] = "USA"
ratesbycountry7days$country[ratesbycountry7days$country=="Czechia"] = "Czech Republic"
ratesbycountry7days$country[ratesbycountry7days$country=="Congo (Kinshasa)"] = "Democratic Republic of the Congo"
ratesbycountry7days$country[ratesbycountry7days$country=="United Kingdom"] = "UK"


worldmapdata <- as_tibble(map_data("world")) %>% 
                        rename(country=region) %>%
                        inner_join(ratesbycountry7days %>% mutate(state=tolower(country))) 

worldmapdata$country[worldmapdata$country=="usa"] = "USA"

ggplot(data = worldmapdata) + 
  geom_polygon(aes(x = long, y = lat, fill = level, group = group), color = "white") + scale_fill_manual(values=colorset) + 
  ggtitle("Week-average daily infection rate across the world (in new infections/day)") + 
  labs(fill="Infection level", caption=caption) +
  coord_fixed(1.3) 

ggsave(paste0("graphs/covid19-worldmap.pdf"), width=11, height=8)


countrylist = top20countrylist$country

for (selected_country in countrylist)
{
        casesdeaths %>% filter(country==selected_country) %>%
                                group_by(date) %>%
                                summarize(population = sum(population),
                                          cases = sum(cases),
                                          deaths = sum(deaths),
                                          casesper100k = cases / population * 1e5,
                                          deathsper100k = deaths / population * 1e5
                                          ) %>% ungroup() -> casesdeathsbylocation


        casesdeathsbylocation %>% filter(date > today() - months(12)) %>%
                        ggplot + aes(date, casesper100k) + geom_line(color="blue", linetype="dotted") + 
                                geom_line(aes(y=rollmean(casesper100k,avdays, na.pad=TRUE)), size=2, color="blue") + 
                                scale_y_continuous(limit=c(0,150), breaks=c(0,2,5,10,20,50,100,150), sec.axis = sec_axis(~ ./correction, breaks=seq(0,5,1))) + 
                                scale_x_date(date_breaks="3 months", date_labels = "%b %d") + 
                                labs(caption=capt, x="Date", y="Daily incremental number of confirmed cases or deaths") + 
                                ggtitle(paste(selected_country, "daily cases and deaths with", avdays,"days average line")) + 
                                geom_line(aes(date, correction*deathsper100k), color="red", linetype="dotted")  + 
                                geom_line(aes(y=rollmean(correction*deathsper100k,avdays,na.pad=TRUE)), size=2, color="red") 

        rank = top20countrylist$rank[top20countrylist$country==selected_country]
        fname = paste0("countrygraphs/", rank,"-cases-and-deaths.pdf")
        ggsave(fname, width=11, height=8)

}


continentlist = sort(unique(na.omit(casesdeaths$continent)))

for (selected_continent in continentlist)
{
        casesdeaths %>% filter(continent==selected_continent) %>%
                                group_by(date) %>%
                                summarize(population = sum(population),
                                          cases = sum(cases),
                                          deaths = sum(deaths),
                                          casesper100k = cases / population * 1e5,
                                          deathsper100k = deaths / population * 1e5
                                          ) %>% ungroup() -> casesdeathsbylocation


        casesdeathsbylocation %>% filter(date > today() - months(12)) %>%
                        ggplot + aes(date, casesper100k) + geom_line(color="blue", linetype="dotted") + 
                                geom_line(aes(y=rollmean(casesper100k,avdays, na.pad=TRUE)), size=2, color="blue") + 
                                scale_y_continuous(limit=c(0,100), breaks=c(0,2,5,10,20,50,100,150), sec.axis = sec_axis(~ ./correction, breaks=seq(0,5,1))) + 
                                scale_x_date(date_breaks="3 months", date_labels = "%b %d") + 
                                labs(caption=capt, x="Date", y="Daily incremental number of confirmed cases or deaths") + 
                                ggtitle(paste(selected_continent, "daily cases and deaths with", avdays,"days average line")) + 
                                geom_line(aes(date, correction*deathsper100k), color="red", linetype="dotted")  + 
                                geom_line(aes(y=rollmean(correction*deathsper100k,avdays,na.pad=TRUE)), size=2, color="red") 

        fname = paste0("continentgraphs/covid19-", selected_continent,"cases-and-deaths.pdf")
        ggsave(fname, width=11, height=8)

}

