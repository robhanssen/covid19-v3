library(tidyverse)
library(lubridate)
library(zoo)
library(broom)

load("Rdata/us_casesdeaths.Rdata")

electionresults <- read_csv("sources/2020electionresults.csv") %>% 
        mutate(trumpvictory = cut(per_gop, c(0,0.1 * 1:10), 
                                           c("<10%", "10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90",">90%"))) %>%
        mutate(trumpvictory = factor(trumpvictory)) %>%
        select(state, county, trumpvictory, per_gop)

# statepop <- us_casesdeaths %>%
#                 select(state, county, population) %>%
#                 unique() %>% 
#                 group_by(state, county) %>%
#                 summarize(population = sum(population))

statepop <- us_casesdeaths %>%
                pivot_wider(c(county, state,population))


# define constants
inauguration = as.Date("2021-01-20")
election = as.Date("2020-11-03")

firstdeath <- min(us_casesdeaths[with(us_casesdeaths, which(deaths>0)),]$date)
avdays <- 7
colorset <- c("D" = "blue", "R" = "red")

casesperelection <- us_casesdeaths %>% filter(date > election) %>%
        inner_join(electionresults) %>%
        group_by(county, state, trumpvictory) %>%
        summarize(cases = sum(cases), 
                  deaths = sum(deaths)) %>%
        inner_join((statepop)) %>%
        ungroup() %>%
        group_by(trumpvictory) %>%
        summarize(cases = sum(cases), deaths = sum(deaths), population = sum(population)) %>%
                mutate(
                  casesper100k = cases / population * 1e5,
                  deathsper100k = deaths / population * 1e5
                  )

colors = c("<25%" = "blue", "25-50%" = "#4d20f0", "50-75%" = "#e300f8", ">75%" = "red")

casesperelection %>%
    ggplot +
        aes(trumpvictory, deathsper100k, fill = trumpvictory) +
        geom_col() +
        labs(x = "Percentage of votes for Trump in 2020 elections",
             y = "Cumulative COVID-19 deaths per 100,000 after Election Day 2020") +
        theme_light() +
        theme(legend.position = "none")

ggsave("misc/deathsbyelectionresults.png", width = 6, height = 6)

casesperelection %>%
    ggplot + 
        aes(trumpvictory, casesper100k, fill = trumpvictory) + 
        geom_col() +
        labs(x = "Percentage of votes for Trump in 2020 elections", 
             y = "Cumulative cases per 100,000 after Election Day 2020") + 
        theme_light() + 
        theme(legend.position = "none")

############################

casespercounty <- us_casesdeaths %>% filter(date > election) %>%
        group_by(county, state) %>%
        summarize(cases = sum(cases), 
                  deaths = sum(deaths)) %>%
        inner_join((statepop)) %>%
        ungroup() %>%
                inner_join(electionresults) %>%
        mutate(
                casesper100k = cases / population * 1e5,
                deathsper100k = deaths / population * 1e5
                  )



casespercounty %>% 
        ggplot + 
        aes(x = per_gop, y = deathsper100k) + geom_point() + 
        geom_smooth(method = "lm")

casespercounty %>% 
        ggplot + 
        aes(x = population, y = per_gop) + geom_point() + 
        scale_x_log10(labels = scales::comma_format()) + 
        geom_smooth(method = "lm")

casespercounty %>% mutate(counter = per_gop * population) %>% summarize(voters = sum(counter))

casespercounty %>% 
        ggplot + 
        aes(x = per_gop, y = casesper100k) + geom_point() + 
        geom_smooth(method = "lm")