require(DBI)
require(chron)
require(RJDBC)
require(rJava)

setwd("~/Desktop/Ongoing Projects/GFW Shark Sanctuary/working_dir")

# first I define the function
connectGFW = function (dbuser, dbpass) 
{
  require(RPostgreSQL)
  require(RH2)
  dbname = "fishingwatch"
  dbhost <- "sharkpulse.cnre.vt.edu"
  dbport <- 5432
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, host = dbhost, port = dbport, dbname = dbname, 
                   user = dbuser, password = dbpass)
}

# Next I pull data at both 5x5 and 0.25x0.25 res
# 5x5 for GAMM to define hooks~hours relationship
Reslat = Reslon = 5 #define res
year = 2019 # not sure if this is necessary
vesselType = "drifting_longlines" #define vessel
require(sCPUEdb)
cgfw = connectGFW("brendan","SharkSanct") #access creds

#pull data and save
testdat = selectData(cgfw, paste("select substring(to_char(date,'YYYY-MM-DD'),1,4) as year, floor(cell_ll_lat/",Reslat,")*",Reslat," as lat, floor(cell_ll_lon/",Reslat,")*",Reslat," as lon, flag as fleet, sum(fishing_hours) fishing_hours from gfw where geartype = '",vesselType,"' and date > '2019-01-01' and date < '2020-01-01' group by year, lat, lon, fleet;", sep = "")) # in the psql consolle it takes 1 minute and 10 seconds - within R it takes alomst 2 minutes
write_csv(testdat, "2019effort_5x5.csv")

# 0.25 x 0.25 Res for projecting hooks at fine scale
Reslat = Reslon = 0.25 #res
require(sCPUEdb)

#pull data and save
testdat2 = selectData(cgfw, paste("select substring(to_char(date,'YYYY-MM-DD'),1,4) as year, floor(cell_ll_lat/",Reslat,")*",Reslat," as lat, floor(cell_ll_lon/",Reslat,")*",Reslat," as lon, flag as fleet, sum(fishing_hours) fishing_hours from gfw where geartype = '",vesselType,"' and date > '2019-01-01' and date < '2020-01-01' group by year, lat, lon, fleet;", sep = "")) # in the psql consolle it takes 1 minute and 10 seconds - within R it takes alomst 2 minutes
write_csv(testdat2, "2019effort_*25x*25.csv")
