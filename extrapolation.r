library(broom)
library(sweep)
library(forecast)
library(timetk)
library(tidyverse)
library(ggfortify)

theme_set(theme_light())


find_value <- function(x,y,target=c(0, 2,5,10)) {
    aa <- approx(y,x,xout=target)$y
    as.Date(aa,origin="1970-01-01")  ## convert back to a date (ugh)
}

# 
#  extrapolocation for SC data
# 

FILTERDATE = as.Date("2021-03-15")

us_casesdeaths %>% filter(state=="South Carolina", date>FILTERDATE) %>%
                        group_by(date) %>%
                        summarize(population = sum(population),
                                  cases = sum(cases),
                                  deaths = sum(deaths),
                                  casesper100k = cases / population * 1e5,
                                  deathsper100k = deaths / population * 1e5
                                  ) %>% ungroup() -> cdbl
     

timeseries <- cdbl %>% select(date, casesper100k) %>% tk_ts() #start=2021, frequency=365)

etsmodel <- timeseries %>% ets() 
etsdata <- etsmodel %>% sw_augment() 

fcast <- etsmodel %>% forecast(h=45)

# autoplot(timeseries) +
#         autolayer(fcast, series="Forecast", PI = FALSE) +
#   geom_point(data=etsdata, aes(x=index, y=.actual)) + 
#   theme(legend.position = "none")  +
#   expand_limits(y=0) + 
#   labs( x="Date", 
#         y="Cases", 
#         color="Account Type", 
#         title="SC cases per 100k" 
#         )
# 
# 


fcast_tib <- as_tibble(fortify(fcast), ts.connect=TRUE) %>% 
                    mutate( dy = as.integer(Index), 
                            date = FILTERDATE + days(dy)) %>%  
                    rename(pointforecast = "Point Forecast", 
                            Lo80 = "Lo 80", 
                            Hi80 = "Hi 80", 
                            Lo95 = "Lo 95",
                            Hi95 = "Hi 95")

ts <- timeseries %>% ets() %>% sw_augment() %>% bind_cols(date=cdbl$date)

ggplot(data=cdbl) + 
        aes(x=date, y=casesper100k) + 
        geom_point() + 
        geom_line(data=fcast_tib, aes(y=Fitted)) + 
        geom_ribbon(data=fcast_tib, aes(y=pointforecast, ymin=Lo95, ymax=Hi95), fill="blue", alpha=.3, lty=2, color="black") +
        geom_ribbon(data=fcast_tib, aes(y=pointforecast, ymin=Lo80, ymax=Hi80), fill="lightblue", alpha=.3, lty=2, color="black") +
        geom_line(data=fcast_tib, aes(y=pointforecast), color="yellow")  +
        scale_x_date(breaks="1 week", date_labels="%b %d", limit=c(today()-weeks(4), today()+weeks(4))) + 
        scale_y_continuous(breaks=10*0:10) + 
        expand_limits(y=0) + 
        labs( x="Date", 
            y="New daily cases per 100,000", 
            color="", 
            title="SC cases per 100k" 
        )


ggsave("projections/covid19-SC-timeseries.pdf", width=11, height=8)


linmod <- cdbl %>%
    ## fit linear model
    lm(formula=casesper100k~date) %>%
    ## predict/add confidence intervals
    augment(interval="confidence",
                newdata=data.frame(date=
                 seq.Date(from=min(cdbl$date),to=max(cdbl$date)+120,
                              by="1 day"))) 
                              
lims <- linmod %>%
    select(date,.lower,.upper) %>%
    ## interpolate to find date corresponding to target value (10)
    ## should use across() but I can't get it working
    summarise(lwr=find_value(date,.upper),
                  upr=find_value(date,.upper)) %>%
    ## convert to useful data frame for ggplot
    pivot_longer(cols=everything(),names_to="limit",values_to="date") %>% filter(limit=="upr")




(ggplot(cdbl)
    + aes(x=date, y=casesper100k)
    + geom_point()
    + expand_limits(x=max(cdbl$date+60))
    + geom_smooth(method="lm", fullrange=TRUE)
    # + geom_line(data=linmod, aes(y=.fitted), color="blue", lty=2)
    + scale_y_continuous(limit=c(-10,40), breaks=seq(0,100,10))
    + scale_x_date(breaks="2 weeks", date_labels="%b %d")
    + geom_hline(yintercept=10,lty=2)
    + geom_vline(data=lims,aes(xintercept=date),lty=2)
    + geom_vline(xintercept=today(),lty=2, color="red")      
    + labs(x="Date", y="Cases per 100k population", title="Cases in South Carolina", subtitle="Cases per 100,000")
)

ggsave("projections/covid19-SC-casesextrapolation.pdf")



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
     
lims_gsp <- (cdbl_gsp
    ## fit linear model
    %>% lm(formula=casesper100k~date)
    ## predict/add confidence intervals
    %>% augment(interval="confidence",
                newdata=data.frame(date=
                 seq.Date(from=min(cdbl$date),to=max(cdbl$date)+120,
                              by="1 day")))
    %>% select(date,.lower,.upper)
    ## interpolate to find date corresponding to target value (10)
    ## should use across() but I can't get it working
    %>% summarise(lwr=find_value(date,.lower),
                  upr=find_value(date,.upper))
    ## convert to useful data frame for ggplot
    %>% pivot_longer(cols=everything(),names_to="limit",values_to="date") %>% filter(limit=="upr")
)


(ggplot(cdbl_gsp)
    + aes(x=date, y=casesper100k)
    + geom_point()
    + expand_limits(x=max(cdbl$date+20))
    + geom_smooth(method="lm", fullrange=TRUE)
    + scale_y_continuous(limit=c(-15,40), breaks=seq(0,100,10))
    + scale_x_date(breaks="2 weeks", date_labels="%b %d")
    + geom_hline(yintercept=10,lty=2)
    + geom_vline(data=lims_gsp,aes(xintercept=date),lty=2)  
    + geom_vline(xintercept=today(),lty=2, color="red")  
    + labs(x="Date", y="Cases per 100k population", title="Cases in GSP Area (South Carolina)", subtitle="Cases per 100,000")
)

ggsave("projections/covid19-SCGSP-casesextrapolation.pdf")

# 
# Timeseries modelling
# 

timeseries <- cdbl_gsp %>% select(date, casesper100k) %>% tk_ts() #start=2021, frequency=365)

etsmodel <- timeseries %>% ets() 
etsdata <- etsmodel %>% sw_augment() 

fcast <- etsmodel %>% forecast(h=45)

fcast_tib <- as_tibble(fortify(fcast), ts.connect=TRUE) %>% 
                    mutate( dy = as.integer(Index), 
                            date = FILTERDATE + days(dy)) %>%  
                    rename(pointforecast = "Point Forecast", 
                            Lo80 = "Lo 80", 
                            Hi80 = "Hi 80", 
                            Lo95 = "Lo 95",
                            Hi95 = "Hi 95")

ts <- timeseries %>% ets() %>% sw_augment() %>% bind_cols(date=cdbl$date)

ggplot(data=cdbl_gsp) + 
        aes(x=date, y=casesper100k) + 
        geom_point() + 
        geom_line(data=fcast_tib, aes(y=Fitted)) + 
        geom_ribbon(data=fcast_tib, aes(y=pointforecast, ymin=Lo95, ymax=Hi95), fill="blue", alpha=.3, lty=2, color="black") +
        geom_ribbon(data=fcast_tib, aes(y=pointforecast, ymin=Lo80, ymax=Hi80), fill="lightblue", alpha=.3, lty=2, color="black") +
        geom_line(data=fcast_tib, aes(y=pointforecast), color="yellow")  +
        scale_x_date(breaks="1 week", date_labels="%b %d", limit=c(today()-weeks(4), today()+weeks(4))) + 
        scale_y_continuous(breaks=10*0:10) + 
        expand_limits(y=0) + 
        labs( x="Date", 
            y="New daily cases per 100,000", 
            color="", 
            title="SC/GSP cases per 100k" 
        )


ggsave("projections/covid19-SCGSP-timeseries.pdf", width=11, height=8)
