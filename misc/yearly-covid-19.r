library(tidyverse)
library(lubridate)
load("Rdata/us_casesdeaths.Rdata")


statepop <-
    us_casesdeaths %>%
    pivot_wider(c(county, state, population)) %>%
    group_by(state) %>%
    summarize(population = sum(population)) %>%
    filter(population != 0)

countrypop <- sum(statepop$population)
totalcases <- sum(us_casesdeaths$cases)
avcases <- totalcases / countrypop

lastdate <- format(max(us_casesdeaths$date), format = "%b %d, %Y")
caption_text <- paste0("JHU data, up until ",
                       lastdate,
                       ". Vertical line is country average")


us_casesdeaths %>%
    mutate(year = year(date),
           quarter = quarter(date),
           yq = paste0(year,"Q",quarter)) %>%
    group_by(year) %>%
    summarize(cases = sum(cases),
              deaths = sum(deaths),
              .groups = "drop") %>%
     mutate(across(.cols = cases:deaths, .fns = scales::comma_format())) %>%
     knitr::kable()

us_casesdeaths %>%
    group_by(date) %>%
    summarize(cases = sum(cases),
              deaths = sum(deaths),
              .groups = "drop") %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    mutate(deathscum = cumsum(deaths),
           casescum = cumsum(cases)
           ) %>%
    ggplot + 
    aes(date, cases) + 
    geom_point() +
    # scale_y_log10(labels = scales::comma_format())
    scale_y_continuous(labels = scales::comma_format())