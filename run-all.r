library(lubridate)

load("Rdata/us_casesdeaths.Rdata")
if(lastreadus != today())
{
        print("READING NEW DATA FOR US")
        source("import-us-data.r")

}
source("process_us_data.r")
source("extrapolation.r")        

load("Rdata/global_casesdeaths.Rdata")
if (lastreadglobal != today())
{
        print("READING NEW DATA FOR GLOBAL")
        source("import-global-data.r")
}
source("process_global_data.r")



