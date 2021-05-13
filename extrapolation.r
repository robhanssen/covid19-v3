find_value <- function(x,y,target=10) {
    aa <- approx(y,x,xout=target)$y
    as.Date(aa,origin="1970-01-01")  ## convert back to a date (ugh)
}

# 
#  extrapolocation for SC data
# 

us_casesdeaths %>% filter(state=="South Carolina", date>as.Date("2021-03-01")) %>%
                        group_by(date) %>%
                        summarize(population = sum(population),
                                  cases = sum(cases),
                                  deaths = sum(deaths),
                                  casesper100k = cases / population * 1e5,
                                  deathsper100k = deaths / population * 1e5
                                  ) %>% ungroup() -> cdbl
     
# lims <- (cdbl 
#     ## fit linear model
#     %>% lm(formula=casesper100k~date)
#     ## predict/add confidence intervals
#     %>% augment(interval="confidence",
#                 newdata=data.frame(date=
#                  seq.Date(from=min(cdbl$date),to=max(cdbl$date)+120,
#                               by="1 day"))) 
#     %>% select(date,.lower,.upper) 
#     ## interpolate to find date corresponding to target value (10)
#     ## should use across() but I can't get it working
#     %>% summarise(lwr=find_value(date,.lower),
#                   upr=find_value(date,.upper))
#     ## convert to useful data frame for ggplot
#     %>% pivot_longer(cols=everything(),names_to="limit",values_to="date")
# )

lims <- cdbl %>%
    ## fit linear model
    lm(formula=casesper100k~date) %>%
    ## predict/add confidence intervals
    augment(interval="confidence",
                newdata=data.frame(date=
                 seq.Date(from=min(cdbl$date),to=max(cdbl$date)+120,
                              by="1 day"))) %>%
    select(date,.lower,.upper) %>%
    ## interpolate to find date corresponding to target value (10)
    ## should use across() but I can't get it working
    summarise(lwr=find_value(date,.lower),
                  upr=find_value(date,.upper)) %>%
    ## convert to useful data frame for ggplot
    pivot_longer(cols=everything(),names_to="limit",values_to="date")




(ggplot(cdbl)
    + aes(x=date, y=casesper100k)
    + geom_point()
    + expand_limits(x=max(cdbl$date+60))
    + geom_smooth(method="lm", fullrange=TRUE)
    + scale_y_continuous(limit=c(-50,100), breaks=seq(0,100,10))
    + scale_x_date(breaks="2 weeks", date_labels="%b %d")
    + geom_hline(yintercept=10,lty=2)
    + geom_vline(data=lims,aes(xintercept=date),lty=2)
    + geom_vline(xintercept=today(),lty=2, color="red")      
    + labs(x="Date", y="Cases per 100k population", title="Cases in South Carolina", subtitle="Cases per 100,000")
)

ggsave("graphs/covid19-SC-casesextrapolation.pdf")



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
                                  ) %>% ungroup() -> cdbl
     
lims <- (cdbl
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
    %>% pivot_longer(cols=everything(),names_to="limit",values_to="date")
)


(ggplot(cdbl)
    + aes(x=date, y=casesper100k)
    + geom_point()
    + expand_limits(x=max(cdbl$date+60))
    + geom_smooth(method="lm", fullrange=TRUE)
    + scale_y_continuous(limit=c(-50,100), breaks=seq(0,100,10))
    + scale_x_date(breaks="2 weeks", date_labels="%b %d")
    + geom_hline(yintercept=10,lty=2)
    + geom_vline(data=lims,aes(xintercept=date),lty=2)  
    + geom_vline(xintercept=today(),lty=2, color="red")  
    + labs(x="Date", y="Cases per 100k population", title="Cases in GSP Area (South Carolina)", subtitle="Cases per 100,000")
)

ggsave("graphs/covid19-SCGSP-casesextrapolation.pdf")