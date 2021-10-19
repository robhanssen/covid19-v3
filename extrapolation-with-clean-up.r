library(broom)
library(tidyverse)
library(ggfortify)
library(lubridate)

theme_set(theme_light())

load("Rdata/us_casesdeaths.Rdata")

find_value <- function(x,y,target=c(0, 2,5,10)) {
    aa <- approx(y,x,xout=target)$y
    as.Date(aa,origin="1970-01-01")  ## convert back to a date (ugh)
}

# 
#  extrapolocation for SC data
# 

FILTERDATE = as.Date("2021-09-01")

cdbl <-
    us_casesdeaths %>% 
        filter(state=="South Carolina", date>FILTERDATE) %>%
        group_by(date) %>%
        summarize(population = sum(population),
                                cases = sum(cases),
                                deaths = sum(deaths),
                                casesper100k = cases / population * 1e5,
                                deathsper100k = deaths / population * 1e5,
                                .groups = "drop")


cdbl_cleanup <-
    cdbl %>%
        mutate(dow = weekdays(date)) %>%
        filter(!dow %in% c("Saturday", "Sunday")) %>%
        mutate(ccasesper100k = ifelse(dow == "Monday", casesper100k / 3, casesper100k))

# 
#  extrapolocation for GSP data
# 

us_casesdeaths %>% filter(state=="South Carolina", county=="Greenville" | county=="Spartanburg", date>FILTERDATE) %>%
                        group_by(date) %>%
                        summarize(population = sum(population),
                                  cases = sum(cases),
                                  deaths = sum(deaths),
                                  casesper100k = cases / population * 1e5,
                                  deathsper100k = deaths / population * 1e5
                                  ) %>% ungroup() -> cdbl_gsp

cdbl_gsp_cleanup <-
    cdbl_gsp %>%
        mutate(dow = weekdays(date)) %>%
        filter(!dow %in% c("Saturday", "Sunday")) %>%
        mutate(ccasesper100k = ifelse(dow == "Monday", casesper100k / 3, casesper100k))


#
# 14 day trends in GSP
#

twoweeksago <- today() - weeks(3)
twoweeksfromnow <- today() + weeks(4)

next2weeks <- tibble(date = seq.Date(from = twoweeksago,
                                    to = twoweeksfromnow,
                                    by = "1 day"))

predict_gsp <- cdbl_gsp_cleanup %>%
        filter(date >= twoweeksago) %>%
        lm(ccasesper100k ~ date, data=.) %>%
        augment(newdata = next2weeks, interval = "confidence")

date10 = find_value(predict_gsp$date, predict_gsp$.fitted, target=10)

ggplot(data = cdbl_gsp_cleanup) +
    aes(x = date, y = ccasesper100k) +
    geom_point() +
    geom_line(data = cdbl_gsp, aes(y = zoo::rollmean(casesper100k, 14, na.pad = TRUE, align = "right")), color = "red") +
    geom_line(data = cdbl_gsp, aes(y = zoo::rollmean(casesper100k, 14, na.pad = TRUE, align = "center")), color = "red", lty = 2) +        
    expand_limits(x = max(cdbl_gsp$date + 20)) +
    scale_y_continuous(limit = c(0, NA), breaks = seq(0, 200, 10)) +
    scale_x_date(breaks = "2 weeks", date_labels = "%b %d") +
    geom_line(data = predict_gsp, aes(y = .fitted), color = "darkgreen") + 
    geom_ribbon(data = predict_gsp, aes(y = .fitted, ymin = .lower, ymax = .upper), fill = "green", alpha = 0.4) +
    labs(x = "Date",
        y = "Cases per 100k population",
        title = "Cases in GSP Area (South Carolina)",
        subtitle = "Cases per 100,000",
        caption = "Red line: rolling mean (14 days)\nGreen line: prediction based on 14 previous days") + 
    geom_vline(xintercept =  date10, color = "red", lty = 2) + 
    geom_hline(yintercept = 10, color = "black", lty = 2)

#ggsave("projections/covid19-SCGSP-linearpredict14days.pdf", width=11, height=8)

#
# 14 day trends in SC
#

predict_sc <- cdbl_cleanup %>%
        filter(date >= twoweeksago) %>%
        lm(ccasesper100k ~ date, data=.) %>%
        augment(newdata = next2weeks, interval = "confidence")

date10 = find_value(predict_sc$date, predict_sc$.fitted, target=10)

ggplot(data = cdbl_cleanup) +
    aes(x = date, y = ccasesper100k) +
    geom_point() +
    geom_line(data = cdbl, aes(y = zoo::rollmean(casesper100k, 14, na.pad = TRUE, align = "right")), color = "red") +
    geom_line(data = cdbl, aes(y = zoo::rollmean(casesper100k, 14, na.pad = TRUE, align = "center")), color = "red", lty = 2) +        
    expand_limits(x = max(cdbl$date + 20)) +
    scale_y_continuous(limit = c(0, NA), breaks = seq(0, 200, 10)) +
    scale_x_date(breaks = "2 weeks", date_labels = "%b %d") +
    geom_line(data = predict_sc, aes(y = .fitted), color = "darkgreen") + 
    geom_ribbon(data = predict_sc, aes(y = .fitted, ymin = .lower, ymax = .upper), fill = "green", alpha = 0.4) +
    labs(x = "Date",
        y = "Cases per 100k population",
        title = "Cases in South Carolina",
        subtitle = "Cases per 100,000",
        caption = "Red line: rolling mean (14 days)\nGreen line: prediction based on 14 previous days") + 
    geom_vline(xintercept =  date10, color = "red", lty = 2) + 
    geom_hline(yintercept = 10, color = "black", lty = 2)

#ggsave("projections/covid19-SC-linearpredict14days.pdf", width=11, height=8)