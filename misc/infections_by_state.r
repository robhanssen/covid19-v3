library(tidyverse)
load("Rdata/us_casesdeaths.Rdata")

us_casesdeaths

statepop <-
    us_casesdeaths %>%
    pivot_wider(c(county,state,population)) %>%
    group_by(state) %>%
    summarize(population = sum(population)) %>%
    filter(population != 0)

countrypop = sum(statepop$population)
totalcases = sum(us_casesdeaths$cases)
avcases = totalcases / countrypop

us_casesdeaths %>%
    group_by(state) %>%
    summarize(cases = sum(cases), .groups = "drop") %>%
    inner_join(statepop, by = "state") %>%
    mutate(color = case_when(state == "South Carolina" ~ "red", TRUE ~ "gray50")) %>%
    mutate(bystate = cases/population) %>%
    ggplot + 
    aes(fct_reorder(state, bystate), bystate, fill = color) + 
    geom_col() + 
    scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
    labs(y = "Diagnosed cases", x = "state", caption = "JHU data, up until Dec 6, 2021. Vertical line is country average") + 
    geom_hline(yintercept = avcases, lty = 2, color = "black") +
    scale_fill_manual(values = c("red" = "red", "gray50" = "gray50")) +
    coord_flip() +
    theme_light() + 
    theme(legend.position = "none") 

ggsave("misc/cumulative-infections-by-state.png", width = 6, height = 10)


totalcases = sum(us_casesdeaths$deaths)
avcases = totalcases / countrypop


us_casesdeaths %>%
    group_by(state) %>%
    summarize(cases = sum(deaths), .groups = "drop") %>%
    inner_join(statepop, by = "state") %>%
    mutate(color = case_when(state == "South Carolina" ~ "red", TRUE ~ "gray50")) %>%
    mutate(bystate = cases/population) %>%
    ggplot + 
    aes(fct_reorder(state, bystate), bystate, fill = color) + 
    geom_col() + 
    scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
    labs(y = "Casualties", x = "state", caption = "JHU data, up until Dec 6, 2021. Vertical line is country average") + 
    geom_hline(yintercept = avcases, lty = 2, color = "black") +
    scale_fill_manual(values = c("red" = "red", "gray50" = "gray50")) +
    coord_flip() +
    theme_light() + 
    theme(legend.position = "none") 

ggsave("misc/cumulative-deaths-by-state.png", width = 6, height = 10)