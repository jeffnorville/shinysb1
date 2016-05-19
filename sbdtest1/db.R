#ini file
readRenviron("~/R/shinysb1/.Renviron")
REdbname = Sys.getenv('pgdb')
REuser = Sys.getenv('api_user')
RElanguage = Sys.getenv('api_language')
REpassword = Sys.getenv('pgpassword')

#db connections
library(dplyr)
library(ggplot2)
source("global.R")

if (is.null(RElanguage) || RElanguage=="")  {
  language = 1
  } else {
  language = RElanguage
}

  
db <- src_postgres('postgres',
                   host = 'localhost',
                   port = 5432,
                   user = REuser,
                   password = REpassword)
tbl_scores <- tbl(db, "tblScores")

# db "windows?

# done in server.R (NON-REACTIVE QUERY):
# ? vs ??

remote <- filter(tbl_scores, 
                   forecastType  == "Seasonal_EDMD_month" &
                   modelVariable == "Streamflow" &
                   # scoreType    == "CRPS" &
                   leadtimeValue %in% 1:30
  )
getit <- structure(collect(remote))

#move to REACTIVE section so this can be datamined "live"
reduced <- filter(getit, locationID %in% c('S2242510', 'L4411710') & scoreType == "CRPS")
local <- collect(reduced)

by_lt <- local %>% group_by(leadtimeValue)
by_lt %>% summarise()

  summarise(local, 
#           count=n(), 
#           dist = avg(scoreValue),
#           )

#  as.POSIXlt(date1)$mon
# season.winter <- c(12, 1, 2) # december, january, february
# lclwintr <- filter(local, as.POSIXlt(dateValue)$mon+1 %in% season.winter )

# needs debussing to work with dplyr
#lcsmry <- summarySE(local, measurevar = "scoreValue", groupvars = c("locationID","leadtimeValue"))



ggplot(local,aes(x = leadtimeValue, y = mean(scoreValue) ) ) +
  # stat_summary(fun.y="mean", geom = "bar") +
  geom_line(aes(color = scoreValue), size=1) +
  geom_hline(aes(yintercept=0), colour="black", linetype="dashed") # colour="#990000"


ggplot(local,aes(x = leadtimeValue, y = (scoreValue - mean(scoreValue)))) +
  # stat_summary(fun.y="mean", geom = "bar") +
  geom_line(aes(color = scoreValue), size=1) +
  geom_hline(aes(yintercept=0), colour="black", linetype="dashed") # colour="#990000"


ggplot(local,aes(x = leadtimeValue, y = (scoreValue - mean(scoreValue)))) +
  # stat_summary(fun.y="mean", geom = "bar") +
  geom_line(aes(color = scoreValue), size=1) +
  geom_hline(aes(yintercept=0), colour="black", linetype="dashed") # colour="#990000"


#
# for (LeadTime in 1:90)
#  {
#local <- collect(filter(reduced, leadtimeValue==LeadTime))
 # local <- collect(reduced)
 #  ggp <- ggplot(local,aes(x = dateValue , y = (scoreValue - mean(scoreValue)))) +
 #    geom_line(aes(color = leadtimeValue), size=1) 
  
  # +
  #   scale_x_date("Lead Time (weeks)") + scale_y_continuous("CRPS for Lead Time ", leadtimeValue)  
    #print(ggp)
# }

  # ggp + facet_grid(scoreValue ~ leadtimeValue)


# # sm2 <- filter(sm, leadtimeValue==LeadTime)
# for (LeadTime in 1:9)
# { ggp <- ggplot(sm,aes(x = dateValue , y = (scoreValue - mean(scoreValue)))) +
#     geom_point(aes(color = 'red'), size=1) +
#     scale_x_date("Lead Time (weeks)") + scale_y_continuous("CRPS for Lead Time ")  
#   #print(ggp)
#   ggp + facet_grid(. ~ leadtimeValue)
# }