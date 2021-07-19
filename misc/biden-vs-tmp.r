library(tidyverse) 
library(lubridate)
library(zoo)
library(broom)

load("Rdata/us_casesdeaths.Rdata")

# define constants
inauguration = as.Date("2021-01-20")
election = as.Date("2020-11-03")

firstdeath = min(us_casesdeaths[with(us_casesdeaths, which(deaths>0)),]$date)
avdays=7

us_casesdeaths %>% filter(date >= firstdeath) %>%
        mutate(president=ifelse(date< inauguration, "Trump", "Biden"), 
        president=factor(president, levels=c("Trump","Biden"))) %>%
        group_by(president) %>%
        summarize(cases=sum(cases), deaths=sum(deaths) ) %>%
        ggplot + aes(x=president, y=deaths, fill=president) + geom_bar(stat="identity") + 
        scale_y_continuous(label=scales::comma_format()) + 
        scale_fill_manual(values= c("Trump" = "red", "Biden"="blue"))

us_casesdeaths %>% filter(date >= firstdeath) %>% 
            group_by(date) %>%
            summarize(cases=sum(cases), deaths=sum(deaths)) %>%
            mutate(president=ifelse(date< inauguration, "Trump", "Biden"), 
            president=factor(president, levels=c("Trump","Biden"))) %>%
            ggplot + aes(x=date, y=deaths, color=president) + #geom_point() +
            scale_color_manual(values= c("Trump" = "red", "Biden"="blue"))  + 
            scale_y_continuous(label=scales::comma_format()) +             
            scale_x_date(breaks="3 months") +
            theme_light() + 
            labs(   x="Date", 
                    y="Deaths per day", 
                    color="President", 
                    title=paste0(avdays,"-day rolling average COVID-19 deaths per day in the United States"),
                    caption="Vertical lines indicate Election Day 2020 (red) and Inauguration Day 2021 (blue)") + 
            geom_vline(xintercept=inauguration, lty=2, color="blue") +
            geom_vline(xintercept=election, lty=2, color="red") +
            #geom_vline(xintercept=as.Date("2021-01-06"), lty=2, color="purple") +            
            geom_line(aes(y=rollmean(deaths,avdays,na.pad=TRUE)))

ggsave("misc/covid19deaths-under-president.png")

us_casesdeaths %>% filter(date >= firstdeath) %>% 
            group_by(date) %>%
            summarize(cases=sum(cases), deaths=sum(deaths)) %>%
            mutate(president=ifelse(date< inauguration, "Trump", "Biden"), 
            president=factor(president, levels=c("Trump","Biden"))) %>%
            ggplot + aes(x=date, y=cases, color=president) + #geom_point() +
            scale_y_continuous(label=scales::comma_format()) + 
            scale_color_manual(values= c("Trump" = "red", "Biden"="blue"))  + 
            theme_light() + 
            labs(x="Date", y="Cases per day", color="President", title=paste0(avdays,"-day rolling average cases per day in the United States")) + 
            geom_vline(xintercept=inauguration, lty=2, color="black") +
            geom_line(aes(y=rollmean(cases,avdays,na.pad=TRUE)))            





# avdays=7
us_casesdeaths %>%  group_by(date) %>% 
                    summarize(deaths=sum(deaths)) %>%  
                    summarize(date=date, cdeaths=cumsum(deaths)) %>% 
                    mutate(president=ifelse(date< inauguration, "Trump", "Biden"), 
                            president=factor(president, levels=c("Trump","Biden"))) %>%
                    ggplot + aes(x=date, y=cdeaths, color=president) + #geom_point() +
                    scale_x_date(breaks="3 months") + 
                    scale_y_continuous(label=scales::comma_format()) + 
                    scale_color_manual(values= c("Trump" = "red", "Biden"="blue"))  + 
                    theme_light() + 
                    labs(x="Date", 
                        y="Cumulative deaths", 
                        color="President", 
                        caption="Vertical lines indicate Election Day 2020 (red) and Inauguration Day 2021 (blue)",
                        title=paste0(avdays,"-day rolling average cumulative deaths in the United States")) + 
                    geom_vline(xintercept=inauguration, lty=2, color="blue") +
                    geom_vline(xintercept=election, lty=2, color="red") +
                    geom_line(aes(y=rollmean(cdeaths,avdays,na.pad=TRUE)))            
ggsave("misc/cumulative-covid19deaths-under-president.png")                    


us_casesdeaths %>%  group_by(date) %>% 
                    summarize(cases=sum(cases)) %>%  
                    summarize(date=date, ccases=cumsum(cases)) %>% 
                    mutate(president=ifelse(date< inauguration, "Trump", "Biden"), 
                            president=factor(president, levels=c("Trump","Biden"))) %>%
                    ggplot + aes(x=date, y=ccases, color=president) + #geom_point() +
                    scale_x_date(date_breaks="3 months", date_label="%b %Y") + 
                    scale_y_continuous(label=scales::comma_format()) + 
                    scale_color_manual(values= c("Trump" = "red", "Biden"="blue"))  + 
                    theme_light() + 
                    labs(x="Date", 
                        y="Cumulative cases", 
                        color="President", 
                        caption="Vertical lines indicate Election Day 2020 (red) and Inauguration Day 2021 (blue)",
                        title=paste0(avdays,"-day rolling average cumulative cases in the United States")) + 
                    geom_vline(xintercept=inauguration, lty=2, color="blue") +
                    geom_vline(xintercept=election, lty=2, color="red") +
                    geom_line(aes(y=rollmean(ccases,avdays,na.pad=TRUE)))            
ggsave("misc/cumulative-covid19cases-under-president.png")                    


us_casesdeaths %>%  group_by(date) %>% 
                    summarize(cases=sum(cases)) %>%  
                    summarize(date=date, ccases=cumsum(cases)) %>% 
                    mutate(president=ifelse(date< inauguration, "Trump", "Biden"), 
                            president=factor(president, levels=c("Trump","Biden"))) %>% 
                    mutate(firstdiv = ccases - lag(ccases)) %>% mutate(secdiff = firstdiv - lag(firstdiv)) %>% #write_csv("sigmoid.csv")
                    ggplot + aes(x=date, y=secdiff, color=president) + #geom_point() +
                    scale_x_date(date_breaks="3 months", date_label="%b %Y") + 
                    scale_y_continuous(label=scales::comma_format()) + 
                    scale_color_manual(values= c("Trump" = "red", "Biden"="blue"))  + 
                    theme_light() + 
                    labs(x="Date", 
                        y="Cumulative cases", 
                        color="President", 
                        caption="Vertical lines indicate Election Day 2020 (red) and Inauguration Day 2021 (blue)",
                        title=paste0(avdays,"-day rolling second derivative of cumulative cases in the United States")) + 
                    geom_vline(xintercept=inauguration, lty=2, color="blue") +
                    geom_vline(xintercept=election, lty=2, color="red") +
                    geom_line(aes(y=rollmean(secdiff,2*avdays,na.pad=TRUE)))            

