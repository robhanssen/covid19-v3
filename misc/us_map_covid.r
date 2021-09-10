library(tidyverse) 
library(lubridate)
library(zoo)
library(broom)

load("Rdata/us_casesdeaths.Rdata")

lowdate <- as.Date("2020-08-01")
highdate <- as.Date("2023-08-01")

statepop <- us_casesdeaths %>%
                filter(date > lowdate, date < highdate) %>%
                select(state, county, population) %>%
                unique() %>% 
                group_by(state) %>%
                summarize(population = sum(population))

totalcovidbycounty <- us_casesdeaths %>% 
                group_by(state) %>%
                summarize(cases = sum(cases),
                          deaths = sum(deaths)
                          ) %>%
                inner_join(statepop) %>%
                mutate(casesper100k = cases / population * 1e5,
                        deathsper100k = deaths / population * 1e5, 
                        # countyname = paste(county, state)
                        ) %>%
                filter(population != 0)

totalcovidbycounty %>% 
        arrange(-casesper100k) %>%
        top_n(20)


totalcovidbycounty %>% 
        arrange(-deathsper100k) %>%
        top_n(20)        

countylevelmap <- 
        as_tibble(map_data("state")) %>%
        inner_join(totalcovidbycounty %>%
                        mutate(state = tolower(state)),
                        by = c(region = "state")
                ) %>%
        mutate(deathsper100k = ifelse(deathsper100k <= 0, 1, deathsper100k),
                casesper100k = ifelse(casesper100k <= 0, 1, casesper100k))


totalcovidbycounty %>% filter(deaths < 0)

deaths_midlevel = mean(totalcovidbycounty$deathsper100k)
cases_midlevel = mean(totalcovidbycounty$casesper100k)

ggplot(data = countylevelmap) + 
        geom_polygon(aes(x = long, y = lat, fill = deathsper100k, group = group), color = "white") + 
        coord_fixed(1.4) +
        scale_fill_gradient2(low = "darkgreen", mid = "yellow", high = "red", midpoint = deaths_midlevel)

ggsave("misc/usmap_coviddeaths.png")

ggplot(data = countylevelmap) + 
        geom_polygon(aes(x = long, y = lat, fill = casesper100k, group = group), color = "white") + 
        coord_fixed(1.4) +
        scale_fill_gradient2(low = "darkgreen", mid = "yellow", high = "red", midpoint = cases_midlevel)

ggsave("misc/usmap_covidcases.png")

