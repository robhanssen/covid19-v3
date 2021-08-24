library(tidyverse) 
library(lubridate)
library(zoo)
library(broom)

load("Rdata/us_casesdeaths.Rdata")

counties <- us_casesdeaths %>%
        select(state, county, population) %>%
        unique() %>%
        mutate(state = toupper(state), county = toupper(county))

countymapdata <- as_tibble(map_data("county")) %>% 
                        rename(state = region, county = subregion) %>%
                        mutate(state = toupper(state), county = toupper(county)) %>%
                        inner_join(counties)


ggplot(data = countymapdata) + 
  geom_polygon(aes(x = long, y = lat, fill = log10(population), group = group), color = "white") + 
  coord_fixed(1.4) + scale_fill_gradient2(low="red", high="blue", midpoint = 5)

ggsave("misc/map_us.png")

totalpop = sum(counties$population)

orderedcounties <- counties %>%
        arrange(-population) %>%
        mutate(cumpop = cumsum(population)) %>%
        mutate(halfway = ifelse(cumpop > totalpop / 2, TRUE, FALSE)) %>%
        select(state, county, halfway)

orderedcountymapdata <- as_tibble(map_data("county")) %>% 
                        rename(state = region, county = subregion) %>%
                        mutate(state = toupper(state), county = toupper(county)) %>%
                        inner_join(orderedcounties)


colorset = c("TRUE" = "darkgreen", "FALSE" = "red")

ggplot(data = orderedcountymapdata) + 
  geom_polygon(aes(x = long, y = lat, fill = halfway, group = group), color = "white") + 
  coord_fixed(1.4) + scale_fill_manual(values = colorset)

ggsave("misc/map_us_half.png")

orderedcounties <- counties %>%
        arrange(-population) %>%
        mutate(cumpop = cumsum(population)) %>%
        mutate(halfway = ifelse(cumpop > totalpop * .80, TRUE, FALSE)) %>%
        select(state, county, halfway)


orderedcountymapdata <- as_tibble(map_data("county")) %>% 
                        rename(state = region, county = subregion) %>%
                        mutate(state = toupper(state), county = toupper(county)) %>%
                        inner_join(orderedcounties)


colorset = c("TRUE" = "darkgreen", "FALSE" = "red")

ggplot(data = orderedcountymapdata) + 
  geom_polygon(aes(x = long, y = lat, fill = halfway, group = group), color = "white", size=0) + 
  coord_fixed(1.4) + scale_fill_manual(values = colorset) +
  labs(x = "Long", y = "Lat", title = "80% of Americans live in the red zone.") +
  theme(legend.position = "none")
  
  ggsave("misc/map_us_80.png")



  ########################

countyarea <- read_csv("misc/countyarea.csv") %>%
                          mutate(state = toupper(state), county = toupper(county))

totalarea <- sum(countyarea$area)



orderedcounties <- counties %>% inner_join(countyarea) %>%
        arrange(-population) %>%
        mutate(cumpop = cumsum(population), cumarea = cumsum(area)) %>%
        mutate(pop_frac = cumpop / totalpop, area_frac = cumarea / totalarea)


orderedcounties %>%
        ggplot +
          aes(y = pop_frac, x = area_frac) +
          geom_line() + 
          theme_light() +
          labs(y = "Fraction of population", x = "Fraction of land area") + 
          scale_x_continuous(labels = scales::percent, breaks = .2 * 0:5) + 
          scale_y_continuous(labels = scales::percent, breaks = .2 * 0:5) +
          geom_vline(xintercept = .2, lty = 2) +
          geom_hline(yintercept = .8, lty = 2)