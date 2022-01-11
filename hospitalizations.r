library(tidyverse)
library(lubridate)
library(zoo)
library(scales)
theme_set(theme_light())

cutoff_date <- as.Date("2021-02-01")

load("Rdata/us_casesdeaths.Rdata")

sources = paste0("COVID-19 deaths data from JHU. Hospitalization data from Our World In Data")

casesdeaths <-
    us_casesdeaths %>%
    group_by(date) %>%
    summarize(cases = sum(cases),
              deaths = sum(deaths),
              .groups = "drop") %>%
    filter(date > cutoff_date)

hosp_file <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/hospitalizations/covid-hospitalizations.csv"

hospitalization <-
    read_csv(hosp_file) %>%
    pivot_wider(names_from = "indicator", values_from = "value") %>%
    janitor::clean_names() %>%
    rename(country = "entity") %>%
    select(-iso_code, -ends_with("per_million"))

reg_hosp <-
    hospitalization %>%
    filter(date > cutoff_date) %>%
    filter(country == "United States")

ts <- 1:50
rsc <- rep(0, length(ts))

for (time_shift in ts) {
    reg_death <-
        us_casesdeaths %>%
        group_by(date) %>%
        summarize(cases = sum(cases),
                deaths = sum(deaths),
                .groups = "drop") %>%
        filter(date > cutoff_date) %>%
        mutate(date = date - time_shift) %>%
        mutate(deaths_av = zoo::rollmean(deaths, 7, na.pad = TRUE))

    reg_data <-
        inner_join(reg_hosp, reg_death, by = "date")

    rs  <- lm(deaths_av ~ daily_icu_occupancy + 0,
              data = reg_data %>% filter(date > as.Date("2021-02-01"), date < as.Date("2021-11-01"))) %>%
            broom::glance() %>%
            pull(r.squared)

    rsc[time_shift] <- rs
}

optimal_ts <- which(rsc == max(rsc))

reg_death <-
    us_casesdeaths %>%
    group_by(date) %>%
    summarize(cases = sum(cases),
            deaths = sum(deaths),
            .groups = "drop") %>%
    filter(date > cutoff_date) %>%
    mutate(date = date - optimal_ts) %>%
    mutate(deaths_av = zoo::rollmean(deaths, 7, na.pad = TRUE))

reg_data <-
    inner_join(reg_hosp, reg_death, by = "date")

fct  <- lm(deaths_av ~ daily_icu_occupancy + 0,
              data = reg_data %>% filter(date > as.Date("2021-02-01"), date < as.Date("2021-11-01"))) %>%
            broom::tidy() %>%
            filter(term == "daily_icu_occupancy") %>%
            pull(estimate)

fct <- 1 / fct

rsq  <- lm(deaths_av ~ daily_icu_occupancy + 0,
              data = reg_data %>% filter(date > as.Date("2021-02-01"), date < as.Date("2021-11-01"))) %>%
            broom::glance() %>%
            pull(r.squared)


fit_summary <- paste0("Date shift: ", optimal_ts, "\nFactor:", round(fct, 3), "\nr.squared: ", round(rsq, 3))


datafit <-
    reg_data %>%
    mutate(color = case_when(date > as.Date("2021-11-02") ~ "test", TRUE ~ "training")) %>%
    ggplot +
    aes(daily_icu_occupancy, deaths_av, color = color) +
    geom_point() +
    geom_abline(slope = 1/fct, intercept = 0) + 
    scale_color_manual(values = c("training" = "blue", "test" = "red")) +
    labs(caption = "Data before Nov 1, 2021 (in blue) is used as training data.\nData after Nov 1, 2021 (in red) is test data") + 
    annotate("label", x = 5000, y = 1800, label = fit_summary, hjust = 0) +
    theme(legend.position = "none")

hd_conversion <- fct
deaths_shift <- optimal_ts
averaging_window <- 7

timeplot <-
    hospitalization %>%
    filter(date > cutoff_date) %>%
    filter(country == "United States") %>%
    ggplot +
    aes(x = date, y = daily_icu_occupancy) +
    geom_point() +
    scale_x_date(date_breaks = "2 month", date_labels = "%b\n%Y") +
    scale_y_continuous(labels = comma_format(),
                       breaks = 1e4 * 0:100,
                       limits = c(0, NA),
                       sec.axis = sec_axis(~ . / hd_conversion,
                                           name = "COVID-19 deaths",
                                           breaks = 500 * 0:100,
                                           labels = scales::comma_format())
                       ) +
    labs(x = "Date",
         y = "Hospital ICU beds in use for COVID-19",
         title = "COVID-19 ICU hospitalization and deaths in the United States",
         caption = paste0("Deaths (in red; 7-day daily mean) are shifted back by ", deaths_shift, " days and peak height is increased by factor ", round(hd_conversion, 3),"\n",sources)) + 
    geom_point(data = casesdeaths,
               aes(x = date - days(deaths_shift),
                   y = hd_conversion * zoo::rollmean(deaths, averaging_window, na.pad = TRUE)),
               color = "red") + 
    geom_vline(xintercept = as.Date("2021-11-01"), lty = 2, color = "gray50")

ggsave("hospitalizations/us-hosp-vs-deaths.png", width = 8, height = 6, plot = timeplot)

library(patchwork)

combined <- timeplot / datafit

ggsave("hospitalizations/us-modelfit.png", width = 8, height = 10, plot = combined)
