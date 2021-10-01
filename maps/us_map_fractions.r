library(tidyverse)
library(lubridate)
library(zoo)
library(broom)

load("Rdata/us_casesdeaths.Rdata")

colorset <- c("TRUE" = "darkgreen", "FALSE" = "red")

counties <- us_casesdeaths %>%
        select(state, county, population) %>%
        pivot_wider(c(state, county, population)) %>%
        mutate(state = toupper(state), county = toupper(county))

countymapdata <- as_tibble(map_data("county")) %>%
                        rename(state = region, county = subregion) %>%
                        mutate(state = toupper(state),
                               county = toupper(county)) %>%
                        inner_join(counties)

totalpop <- sum(counties$population)

orderedcounties <- counties %>%
        arrange(-population) %>%
        mutate(cumpop = cumsum(population)) %>%
        select(state, county, cumpop)

orderedcountymapdata <- as_tibble(map_data("county")) %>%
                        rename(state = region, county = subregion) %>%
                        mutate(state = toupper(state),
                               county = toupper(county)) %>%
                        inner_join(orderedcounties)

percentlist <- c(0.33, 0.5, 0.66, 0.80, 0.90)

for (poppercentage in percentlist) {

        orderedcountymapdata_fraction <-
                orderedcountymapdata %>%
                mutate(halfway = ifelse(cumpop > totalpop * poppercentage,
                                        TRUE,
                                        FALSE))

        pctname <- floor(poppercentage * 100)
        fname <- paste0("maps/map_us_", pctname, ".png")
        title <- paste0(pctname, "% of Americans live in the red zone.")

        ggplot(data = orderedcountymapdata_fraction) +
                geom_polygon(aes(x = long,
                                 y = lat,
                                 fill = halfway,
                                 group = group),
                             color = "white",
                             size = 0) +
                coord_fixed(1.4) +
                scale_fill_manual(values = colorset) +
                labs(x = "Long",
                     y = "Lat", title = title) +
                theme(legend.position = "none")

        ggsave(fname, width = 6, height = 6)
}

# 80/20 graph of landarea vs population

countyarea <- read_csv("maps/countyarea.csv") %>%
                          mutate(state = toupper(state),
                                 county = toupper(county))

totalarea <- sum(countyarea$area)

orderedcounties_area <- counties %>% inner_join(countyarea) %>%
        arrange(-population) %>%
        mutate(cumpop = cumsum(population), cumarea = cumsum(area)) %>%
        mutate(pop_frac = cumpop / totalpop, area_frac = cumarea / totalarea)


orderedcounties_area %>%
        ggplot +
          aes(y = pop_frac, x = area_frac) +
          geom_line() + 
          theme_light() +
          labs(title = "Distribution of population over the land area",
               subtitle = "The distribution follows the 80/20 rule almost perfectly",
               y = "Fraction of population",
               x = "Fraction of land area") +
          scale_x_continuous(labels = scales::percent, breaks = .2 * 0:5) +
          scale_y_continuous(labels = scales::percent, breaks = .2 * 0:5) +
          geom_vline(xintercept = .2, lty = 2) +
          geom_hline(yintercept = .8, lty = 2)

ggsave("maps/US_population_over_land_area.png", width = 6, height = 6)