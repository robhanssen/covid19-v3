library(tidyverse)
library(lubridate)
library(zoo)
library(broom)

load("Rdata/us_casesdeaths.Rdata")

stategov <- read_csv("sources/stategov.csv")

# define constants
inauguration = as.Date("2021-01-20")
election = as.Date("2020-11-03")

firstdeath <- min(us_casesdeaths[with(us_casesdeaths, which(deaths>0)),]$date)
avdays <- 7
colorset <- c("D" = "blue", "R" = "red")

casesperday <- us_casesdeaths %>%
        inner_join(stategov) %>%
        select(-governor) %>%
        group_by(party, date) %>%
        summarize(cases = sum(cases), 
                  deaths = sum(deaths), 
                  population = sum(population),
                  casesper100k = cases / population * 1e5,
                  deathsper100k = deaths / population * 1e5
                  ) %>%
        ungroup() %>%
        filter(date > firstdeath)

casesperday_D <- casesperday %>%
        arrange(date) %>%
        filter(party == "D") %>%
        mutate(wave = rollmean(casesper100k, 7, na.pad=TRUE),
                wave_deaths = rollmean(deathsper100k, 7, na.pad=TRUE)
        )


casesperday_R <- casesperday %>%
        arrange(date) %>%
        filter(party == "R") %>%
        mutate(wave = rollmean(casesper100k, 7, na.pad=TRUE),
                wave_deaths = rollmean(deathsper100k, 7, na.pad=TRUE)
        )


cumulative <- casesperday %>% arrange(party) %>%
                group_by(party)  %>%
                mutate(cumcases = cumsum(cases),
                       cumdeath = cumsum(deaths),
               ) %>% 
               ungroup()




casesperday %>%
        ggplot +
        aes(x = date, y = casesper100k, color = party) +
        # geom_point() +
        #geom_line(aes(y = rollmean(casesper100k, 7, na.pad=TRUE))) + 
        geom_line(aes(y=wave), data = casesperday_D, color = "blue") +
        geom_line(aes(y=wave), data = casesperday_R, color = "red") +
        theme_light() + 
        scale_color_manual(values = colorset) + 
        scale_x_date(date_breaks = "3 months", date_label = "%b\n%Y") + 
        scale_y_continuous(labels = scales::comma_format()) + 
        labs(x = "Date", y = "New cases per 100k per day", color = "Governor\nparty affiliation")

casesperday %>% 
        ggplot +
        aes(x = date, y = cases, color = party) +
        # geom_point() +
#        geom_line(aes(y = rollmean(cases, avdays, na.pad=TRUE))) + 
        theme_light() + 
        scale_color_manual(values = colorset) + 
        scale_x_date(date_breaks = "3 months", date_label = "%b %Y") +
        scale_y_continuous(labels = scales::comma_format()) + 
        labs(x = "Date", y = "New cases per day", color = "Governor\nparty affiliation")

casesperday %>% 
        ggplot +
        aes(x = date, y = deathsper100k, color = party) +
        # geom_point() +
        geom_line(aes(y=wave_deaths), data = casesperday_D, color = "blue") +
        geom_line(aes(y=wave_deaths), data = casesperday_R, color = "red") +

        # geom_line(aes(y = rollmean(deathsper100k, avdays, na.pad=TRUE))) + 
        theme_light() + 
        scale_color_manual(values = colorset) + 
        scale_x_date(date_breaks = "3 months", date_label = "%b %Y", limit = c(as.Date("2021-04-01"),NA))  +
        scale_y_continuous(labels = scales::comma_format()) + 
        labs(x = "Date", y = "New deaths per 100k per day", color = "Governor\nparty affiliation")
ggsave("misc/deaths-per-day-by-governor.png")

casesperday %>%
        ggplot +
        aes(x = date, y = deaths, color = party) +
        # geom_point() +
        geom_line(aes(y = rollmean(deaths, avdays, na.pad=TRUE))) + 
        theme_light() + 
        scale_color_manual(values = colorset) + 
        scale_x_date(date_breaks = "3 months", date_label = "%b %Y") +
        scale_x_date(date_breaks = "3 months", date_label = "%b %Y") +
        scale_y_continuous(labels = scales::comma_format()) + 
        labs(x = "Date", y = "New deaths per day", color = "Governor\nparty affiliation")

cumulative %>%
        ggplot +
        aes(x = date, y = cumdeath, color = party) +
        geom_point() +
        # geom_line(aes(y = rollmean(deaths, avdays, na.pad=TRUE))) + 
        theme_light() + 
        scale_color_manual(values = colorset) + 
        scale_x_date(date_breaks = "3 months", date_label = "%b %Y") +
        scale_x_date(date_breaks = "3 months", date_label = "%b %Y") +
        scale_y_continuous(labels = scales::comma_format()) + 
        labs(x = "Date", y = "Cumulative deaths", color = "Governor\nparty affiliation")

cumulative %>%
        ggplot +
        aes(x = date, y = cumcases, color = party) +
        geom_point() +
        # geom_line(aes(y = rollmean(deaths, avdays, na.pad=TRUE))) + 
        theme_light() + 
        scale_color_manual(values = colorset) + 
        scale_x_date(date_breaks = "3 months", date_label = "%b %Y") +
        scale_x_date(date_breaks = "3 months", date_label = "%b %Y") +
        scale_y_continuous(labels = scales::comma_format()) + 
        labs(x = "Date", y = "Cumulative cases", color = "Governor\nparty affiliation")
