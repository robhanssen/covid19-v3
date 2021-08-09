library(tidyverse)
library(lubridate)
library(zoo)
library(broom)

load("Rdata/us_casesdeaths.Rdata")

stategov <- read_csv("sources/stategov.csv")

# define constants
inauguration = as.Date("2021-01-20")
election = as.Date("2020-11-03")

firstdeath = min(us_casesdeaths[with(us_casesdeaths, which(deaths>0)),]$date)
avdays <- 7

casesperday <- us_casesdeaths %>%
        inner_join(stategov) %>%
        select(-governor) %>%
        group_by(date, party) %>%
        summarize(cases = sum(cases), 
                  deaths = sum(deaths), 
                  population = sum(population),
                  casesper100k = cases / population * 1e5,
                  deathsper100k = deaths / population * 1e5
                  ) %>% ungroup() %>% group_by(party)  %>%
        mutate(cumcases = cumsum(cases),
               cumdeath = cumsum(deaths),
               ) %>% ungroup() %>%
        filter(date >= as.Date("2020-03-01"))

colorset = c("D" = "blue", "R" = "red")


casesperday %>%
        ggplot +
        aes(x = date, y = casesper100k, color = party) +
        # geom_point() +
        geom_line(aes(y = rollmean(casesper100k, 7, na.pad=TRUE))) + 
        theme_light() + 
        scale_color_manual(values = colorset) + 
        scale_x_date(date_breaks = "3 months", date_label = "%b\n%Y") + 
        scale_y_continuous(labels = scales::comma_format()) + 
        labs(x = "Date", y = "New cases per 100k per day", color = "Governor\nparty affiliation")

casesperday %>% 
        ggplot +
        aes(x = date, y = cases, color = party) +
        # geom_point() +
        geom_line(aes(y = rollmean(cases, avdays, na.pad=TRUE))) + 
        theme_light() + 
        scale_color_manual(values = colorset) + 
        scale_x_date(date_breaks = "3 months", date_label = "%b %Y") +
        scale_y_continuous(labels = scales::comma_format()) + 
        labs(x = "Date", y = "New cases per day", color = "Governor\nparty affiliation")

casesperday %>%
        ggplot +
        aes(x = date, y = deathsper100k, color = party) +
        # geom_point() +
        geom_line(aes(y = rollmean(deathsper100k, avdays, na.pad=TRUE))) + 
        theme_light() + 
        scale_color_manual(values = colorset) + 
        scale_x_date(date_breaks = "3 months", date_label = "%b %Y")  +
        scale_y_continuous(labels = scales::comma_format()) + 
        labs(x = "Date", y = "New deaths per 100k per day", color = "Governor\nparty affiliation")


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

casesperday %>%
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

casesperday %>%
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
