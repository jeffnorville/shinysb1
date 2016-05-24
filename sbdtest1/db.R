#ini file
readRenviron("~/R/shinysb1/.Renviron")
REdbname = Sys.getenv('pgdb')
REuser = Sys.getenv('api_user')
RElanguage = Sys.getenv('api_language')
REpassword = Sys.getenv('pgpassword')

#db connections
library(plyr); library(dplyr)
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

# broken into 2 
remote <- filter(tbl_scores, 
                   forecastType  == "Seasonal_EDMD_month" &
                   modelVariable == "Streamflow" &
                   # scoreType    == "CRPS" &
                   leadtimeValue %in% 1:90
  )
getit <- structure(collect(remote))

# move to REACTIVE section so this can be datamined "live"
# reduced <- filter(getit, locationID %in% c('S2242510', 'L4411710') & scoreType == "CRPS")
reduced <- filter(getit, scoreType == "CRPS")
local <- collect(reduced)

#  as.POSIXlt(date1)$mon
# season.winter <- c(12, 1, 2) # december, january, february
# lclwintr <- filter(local, as.POSIXlt(dateValue)$mon+1 %in% season.winter )


#lcsmry <- summarySE(local, measurevar = "scoreValue", groupvars = c("locationID","leadtimeValue"))
# summarySE(local, measurevar="scoreValue", groupvars="leadtimeValue", na.rm = TRUE)
loc.sum <- summarySE(local, measurevar="scoreValue", groupvars=c("locationID", "leadtimeValue"), na.rm=TRUE)

loc.sum$locationID <- as.factor(loc.sum$locationID)
plot(loc.sum$leadtimeValue, loc.sum$scoreValue, col=loc.sum$locationID)




mse(local$scoreValue[[1]], c(local$scoreValue, na.rm=TRUE))

summary(c(local$scoreValue, na.rm=TRUE))

ggplot(loc.sum, aes(leadtimeValue, scoreValue)) +
  geom_point(aes(color = locationID), size=3)

(local$scoreValue - loc.sum$scoreValue) / loc.sum$N



ggplot(local,aes(x = leadtimeValue, y = (local$scoreValue - loc.sum$scoreValue) / loc.sum$N ) ) +
  # stat_summary(fun.y="mean", geom = "bar") +
  geom_line(aes(color = "blue"), size=1) +
  geom_hline(aes(yintercept=0), colour="black", linetype="dashed") # colour="#990000"



ss <- (local$scoreValue - loc.sum$scoreValue) / loc.sum$N
ggplot(local, aes(x=leadtimeValue, y= ss, colour=leadtimeValue)) + 
  geom_errorbar(aes(ymin=ss-loc.sum$ci, ymax=ss+loc.sum$ci), width=.1) +
  geom_line() +
  geom_point()



ggplot(local,aes(x = leadtimeValue, y = (scoreValue - mean(scoreValue)))) +
  # stat_summary(fun.y="mean", geom = "bar") +
  geom_line(aes(color = scoreValue), size=1) +
  geom_hline(aes(yintercept=0), colour="black", linetype="dashed") # colour="#990000"

ggplot(local,aes(x = leadtimeValue, y = (scoreValue - mean(scoreValue)))) +
  # stat_summary(fun.y="mean", geom = "bar") +
  geom_line(aes(color = scoreValue), size=1) +
  geom_hline(aes(yintercept=0), colour="black", linetype="dashed") # colour="#990000"


# function for running average -- improved to allow partial=FALSE 
ma <- function(x, n=2, partial=TRUE){
  res = x #set the first value
  for(i in 1:length(x)){
    t<-max(i-n+1,1)
    res[i] = mean(x[t:i])
  }
  if (partial==TRUE) res
  else {
    res[-c(seq(1,n-1,1))] #remove the n-1 first,i.e., res[c(-3,-4,...)]
  }
}
  # ggp + facet_grid(scoreValue ~ leadtimeValue)

