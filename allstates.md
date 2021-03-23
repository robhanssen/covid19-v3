---
title: "Covid data in the US"
output: html_document
---





```r
load("Rdata/us_casesdeaths.Rdata")

# assign the region in the US to all locations
locations = read_csv("sources/USstateslist.csv")
```

```
## Parsed with column specification:
## cols(
##   state = col_character(),
##   location = col_character()
## )
```

```r
us_casesdeaths <- us_casesdeaths %>% 
                        full_join(locations) %>%
                        mutate(location = ifelse(is.na(location), "Other", location))
```

```
## Joining, by = "state"
```

```r
correction = 60
avdays = 7
capt="insert caption here"

us_casesdeaths %>% group_by(date,location) %>%
                        summarize(population = sum(population),
                                  cases = sum(cases),
                                  deaths = sum(deaths),
                                  casesper100k = cases / population * 1e5,
                                  deathsper100k = deaths / population * 1e5
                                  ) %>% ungroup() -> casesdeathsbylocation
```

```
## `summarise()` regrouping output by 'date' (override with `.groups` argument)
```

```r
regionlist = locations %>% distinct(location)

for(region in regionlist$location) 
{
        casesdeathsbylocation %>% filter( location == region) %>%
                ggplot + aes(date, casesper100k) + geom_line(color="blue", linetype="dotted") + 
                        geom_line(aes(y=rollmean(casesper100k,avdays, na.pad=TRUE)), size=2, color="blue") + 
                        scale_y_continuous(sec.axis = sec_axis(~ ./correction, breaks=seq(0,5,1))) + 
                        scale_x_date(date_breaks="3 months", date_labels = "%b %d") + 
                        labs(caption=capt, x="Date", y="Daily incremental number of confirmed cases or deaths") + 
                        ggtitle(paste(region, "daily cases and deaths with", avdays,"days average line")) + 
                        geom_line(aes(date, correction*deathsper100k), color="red", linetype="dotted")  + 
                        geom_line(aes(y=rollmean(correction*deathsper100k,avdays,na.pad=TRUE)), size=2, color="red") 

        ##ggsave(paste0("graphs/covid19-",region,"cases-and-deaths.pdf"))
        
}
```
