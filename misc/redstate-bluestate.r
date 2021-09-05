library(tidyverse)
library(lubridate)
library(zoo)
library(broom)

load("Rdata/us_casesdeaths.Rdata")

stategov <- read_csv("sources/stategov1.csv") %>% rename(govparty = "party", houseparty = "legis")

# define constants
inauguration = as.Date("2021-01-20")
election = as.Date("2020-11-03")

firstdeath <- min(us_casesdeaths[with(us_casesdeaths, which(deaths>0)),]$date)
avdays <- 7
colorset <- c("D" = "blue", "R" = "red")

casesperday <- us_casesdeaths %>%
        inner_join(stategov) %>%
        select(-governor) %>%
        group_by(houseparty, date) %>%
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
        filter(houseparty == "D") %>%
        mutate(wave = rollmean(casesper100k, 7, na.pad=TRUE),
                wave_deaths = rollmean(deathsper100k, 7, na.pad=TRUE)
        )


casesperday_R <- casesperday %>%
        arrange(date) %>%
        filter(houseparty == "R") %>%
        mutate(wave = rollmean(casesper100k, 7, na.pad=TRUE),
                wave_deaths = rollmean(deathsper100k, 7, na.pad=TRUE)
        )


cumulative <- casesperday %>% arrange(houseparty) %>%
                group_by(houseparty)  %>%
                mutate(cumcases = cumsum(cases),
                       cumdeath = cumsum(deaths),
               ) %>% 
               ungroup()




casesperday %>%
        ggplot +
        aes(x = date, y = casesper100k, color = houseparty) +
        # geom_point() +
        #geom_line(aes(y = rollmean(casesper100k, 7, na.pad=TRUE))) + 
        geom_line(aes(y=wave), data = casesperday_D, color = "blue") +
        geom_line(aes(y=wave), data = casesperday_R, color = "red") +
        theme_light() + 
        scale_color_manual(values = colorset) + 
        scale_x_date(date_breaks = "3 months", date_label = "%b\n%Y") + 
        scale_y_continuous(labels = scales::comma_format()) + 
        labs(x = "Date", y = "New cases per 100k per day", color = "House of Rep\nparty affiliation")


casesperday %>% 
        ggplot +
        aes(x = date, y = deathsper100k, color = houseparty) +
        # geom_point() +
        geom_line(aes(y=wave_deaths), data = casesperday_D, color = "blue") +
        geom_line(aes(y=wave_deaths), data = casesperday_R, color = "red") +

        # geom_line(aes(y = rollmean(deathsper100k, avdays, na.pad=TRUE))) + 
        theme_light() + 
        scale_color_manual(values = colorset) + 
        scale_x_date(date_breaks = "3 months", date_label = "%b %Y", limit = c(as.Date("2021-04-01"),NA))  +
        scale_y_continuous(labels = scales::comma_format()) + 
        labs(x = "Date", y = "New deaths per 100k per day", color = "House of Rep\nparty affiliation")
ggsave("misc/deaths-per-day-by-governor.png")

casesperday %>%
        ggplot +
        aes(x = date, y = deaths, color = houseparty) +
        # geom_point() +
        geom_line(aes(y = rollmean(deaths, avdays, na.pad=TRUE))) + 
        theme_light() + 
        scale_color_manual(values = colorset) + 
        scale_x_date(date_breaks = "3 months", date_label = "%b %Y") +
        scale_x_date(date_breaks = "3 months", date_label = "%b %Y") +
        scale_y_continuous(labels = scales::comma_format()) + 
        labs(x = "Date", y = "New deaths per day", color = "House of Rep\nparty affiliation")

cumulative %>%
        ggplot +
        aes(x = date, y = cumdeath, color = houseparty) +
        geom_point() +
        # geom_line(aes(y = rollmean(deaths, avdays, na.pad=TRUE))) + 
        theme_light() + 
        scale_color_manual(values = colorset) + 
        scale_x_date(date_breaks = "3 months", date_label = "%b %Y") +
        scale_x_date(date_breaks = "3 months", date_label = "%b %Y") +
        scale_y_continuous(labels = scales::comma_format()) + 
        labs(x = "Date", y = "Cumulative deaths", color = "House of Rep\nparty affiliation")

cumulative %>%
        ggplot +
        aes(x = date, y = cumcases, color = houseparty) +
        geom_point() +
        # geom_line(aes(y = rollmean(deaths, avdays, na.pad=TRUE))) + 
        theme_light() + 
        scale_color_manual(values = colorset) + 
#        scale_x_date(date_breaks = "3 months", date_label = "%b %Y") +
        scale_x_date(date_breaks = "3 months", date_label = "%b %Y") +
        scale_y_continuous(labels = scales::comma_format()) + 
        labs(x = "Date", y = "Cumulative cases", color = "House of Rep\nparty affiliation")


cases_by_house <- us_casesdeaths %>%
        filter(date > as.Date("2021-06-01")) %>%
        inner_join(stategov) %>%
        select(-governor)

casesperday_april1 <-
        cases_by_house %>%
        group_by(houseparty, date) %>%
        summarize(cases = sum(cases), 
                  deaths = sum(deaths), 
                  population = sum(population),
                  .groups = "drop") %>%
        arrange(houseparty, date) %>%
        group_by(houseparty) %>%
        mutate(ccases = cumsum(cases),
               ccasesper100k = ccases / population * 1e5, 
               cdeaths = cumsum(deaths),
               cdeathsper100k = cdeaths / population * 1e5
                )

casesperday_april1 %>% 
        filter(date == max(casesperday_april1$date)) %>%
        arrange(-cdeathsper100k) %>%
        select(houseparty, population, ccases, ccasesper100k, cdeaths, cdeathsper100k)

totaldeaths <- casesperday_april1 %>% 
                group_by(date) %>%
                summarize(deaths = sum(deaths), .groups = "drop") %>%
                mutate(cdeaths = cumsum(deaths))


casesperday_april1 %>% 
        ggplot + 
                aes(x = date, y = cdeaths, color = houseparty) + 
                geom_line() + 
                geom_line(data = totaldeaths, lty = 2, color = "gray50") +
                scale_color_manual(values = colorset) + 
                scale_y_continuous(label = scales::comma_format(), limit = c(0, 1e5))

casesperday_april1 %>% 
        ggplot + 
                aes(x = date, y = cdeathsper100k, color = houseparty) + 
                geom_line() + 
                scale_color_manual(values = colorset) + 
                scale_y_continuous(label = scales::comma_format(), limit = c(0, 30))
