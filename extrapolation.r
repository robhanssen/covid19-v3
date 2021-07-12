library(broom)
library(sweep)
library(forecast)
library(timetk)


theme_set(theme_light())


find_value <- function(x,y,target=c(0, 2,5,10)) {
    aa <- approx(y,x,xout=target)$y
    as.Date(aa,origin="1970-01-01")  ## convert back to a date (ugh)
}

# 
#  extrapolocation for SC data
# 

us_casesdeaths %>% filter(state=="South Carolina", date>as.Date("2021-03-15")) %>%
                        group_by(date) %>%
                        summarize(population = sum(population),
                                  cases = sum(cases),
                                  deaths = sum(deaths),
                                  casesper100k = cases / population * 1e5,
                                  deathsper100k = deaths / population * 1e5
                                  ) %>% ungroup() -> cdbl
     

timeseries <- cdbl %>% select(date, casesper100k) %>% tk_ts()

etsmodel <- timeseries %>% ets() 
etsdata <- etsmodel %>% sw_augment() 

fcast <- etsmodel %>% forecast(h=60)

autoplot(timeseries) +
        autolayer(fcast, series="Forecast", PI = FALSE) +
  geom_point(data=etsdata, aes(x=index, y=.actual)) + 
  theme(legend.position = "none")  +
  expand_limits(y=0) + 
  labs( x="Date", 
        y="Cases", 
        color="Account Type", 
        title="SC cases per 100k" 
        )



ggsave("projections/covid19-SC-timeseries.pdf")


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

us_casesdeaths %>% filter(state=="South Carolina", county=="Greenville" | county=="Spartanburg", date>as.Date("2021-03-15")) %>%
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


timeseries <- cdbl_gsp %>% select(date, casesper100k) %>% tk_ts()

etsmodel <- timeseries %>% ets() 
etsdata <- etsmodel %>% sw_augment() 

fcast <- etsmodel %>% forecast(h=60)

autoplot(timeseries) +
        autolayer(fcast, series="Forecast", PI = FALSE) +
  geom_point(data=etsdata, aes(x=index, y=.actual)) + 
  theme(legend.position = "none")  +
  expand_limits(y=0) + 
  labs( x="Date", 
        y="Cases", 
        color="Account Type", 
        title="SC/GSP cases per 100k" 
        )


ggsave("projections/covid19-SCGSP-timeseries.pdf")