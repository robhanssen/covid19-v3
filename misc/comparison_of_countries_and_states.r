
countries <- ratesbycountry7days %>%
                filter(population > 5e6) %>%
                select(country, casesper100k, deathsper100k, level)

states <- ratesbystate7days %>%
                #filter(population > 5e6) %>%
                rename(country = "state") %>%
                select(country, casesper100k, deathsper100k, level)

states$country[states$country == "Georgia"] <- "Georgia (US)"

combined_countryanstates <- bind_rows(countries, states)


combined_countryanstates %>% top_n(30,casesper100k) %>%
                        arrange(-casesper100k) %>% bind_cols(rank=1:30) %>% mutate(country=paste0(country," (",rank,")")) %>%
                        ggplot + aes(x=fct_reorder(country,casesper100k), y=casesper100k, fill=level) + 
                               scale_y_continuous(breaks=c(2,5,10,20,50,100)) +
                               geom_bar(stat="identity") + 
                               labs(x="Countries with population over 5 million and US states", y="Daily new infection per 100,000 population", caption=caption) +
                               coord_flip() + 
                               scale_fill_manual(values=colorset)

ggsave("misc/comparison-of-countries-and-states.pdf", height = 11, width = 8)