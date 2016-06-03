#ini file
readRenviron("~/R/shinysb1/.Renviron")
REhost =     Sys.getenv('pgserver')
REport =     Sys.getenv('pgport')
REdbname =   Sys.getenv('pgdb')
REuser =     Sys.getenv('api_user')
RElanguage = Sys.getenv('api_language')
REpassword = Sys.getenv('pgpassword')


source("global.R")

#db connections
library(plyr); library(dplyr)
library(ggplot2)

if (is.null(RElanguage) || RElanguage=="")  {
  language = 1
  } else {
  language = RElanguage
}

db <- src_postgres(dbname = REdbname,
                   host = REhost,
                   port = REport,
                   user = REuser,
                   password = REpassword)
tbl_scores <- tbl(db, "tblScores")



# db "windows?

# done in server.R (NON-REACTIVE QUERY):
# ? vs ??

#  TODO time the influence of breaking this into two calls

# broken into 2 
remote <- filter(tbl_scores, 
                   scoreNA == FALSE &&
                   locationID %in% c('S2242510', 'L4411710') &&
                   modelVariable == "Streamflow" &&
                   forecastType  == "Seasonal_EDMD_month" &&
                   # scoreType    == "CRPS" &&
                   leadtimeValue %in% 1:15
  )
getit <- structure(collect(remote))

# move to REACTIVE section so this can be datamined "live"
# reduced <- filter(getit, locationID %in% c('S2242510', 'L4411710') & scoreType == "CRPS")
reduced <- filter(getit, scoreType == "CRPS")
local <- collect(reduced)

#  as.POSIXlt(date1)$mon
# season.winter <- c(12, 1, 2) # december, january, february
# lclwintr <- filter(local, as.POSIXlt(dateValue)$mon+1 %in% season.winter )


# lcsmry <- summarySE(local, measurevar = "scoreValue", groupvars = c("locationID","leadtimeValue"))
# summarySE(local, measurevar="scoreValue", groupvars=c("locationID", "leadtimeValue"), na.rm=TRUE)
loc.sum <- summarySE(local, measurevar="scoreValue", groupvars=c("locationID", "leadtimeValue"), na.rm=TRUE)

loc.sum$locationID <- as.factor(loc.sum$locationID)

plot(loc.sum$leadtimeValue, loc.sum$scoreValue, col=loc.sum$locationID, 
     xlab = "Lead Times", ylab = "Score")

#averages
ggplot(loc.sum, aes(x = leadtimeValue, y = scoreValue ) ) +
  geom_point(aes(color = locationID)) +
  geom_hline(aes(yintercept=0), colour="black", linetype="dashed") + # colour="#990000"
  xlab("Lead Times") + ylab("Score")


#with CIs
pd <- position_dodge(0.1)

ggplot(loc.sum, aes(x = leadtimeValue, y = scoreValue ) ) +
  geom_point(aes(color = locationID)) +
  geom_errorbar(aes(ymin=scoreValue-ci, ymax=scoreValue+ci),width=.1, position=pd) +
  geom_line(position=pd) +
  geom_hline(aes(yintercept=0), colour="black", linetype="dashed") + # colour="#990000"
  xlab("Lead Times") + ylab("Score") 




#raw scores
ggplot(local, aes(x = leadtimeValue, y = scoreValue ) ) +
  geom_point(aes(color = locationID)) +
  geom_hline(aes(yintercept=0), colour="black", linetype="dashed") # colour="#990000"




# plot(loc.sum$leadtimeValue, loc.sum$scoreValue, col=loc.sum$locationID)
# unique(loc.sum$locationID)
# 
# mse(local$scoreValue[[1]], c(local$scoreValue, na.rm=TRUE))
# 
# summary(c(local$scoreValue, na.rm=TRUE))
# browser()
# 
# ggplot(loc.sum, aes(leadtimeValue, scoreValue)) +
#   geom_point(aes(color = locationID), size=3)
# 
# (local$scoreValue - loc.sum$scoreValue) / loc.sum$N
# 
# lcl2 <- filter(local, locationID %in% c("A1080330", "H7401010"), leadtimeValue < 25)

# GETTING THERE
ggplot(lcl2, aes(x = leadtimeValue, y = scoreValue ) ) +
  geom_point(aes(color = locationID)) +
  geom_hline(aes(yintercept=0), colour="black", linetype="dashed") # colour="#990000"




ss <- (local$scoreValue - loc.sum$scoreValue) / loc.sum$N
axismin <- ss-loc.sum$ci
axismax <- ss+loc.sum$ci


ggplot(local, aes(x = subset(leadtimeValue, leadtimeValue == 5)), y = ss, colour = leadtimeValue) + 
  geom_errorbar( aes (ymin = axismin, ymax = axismax), width=.1) +
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

loc.sum <- summarySE(local, measurevar="scoreValue", groupvars=c("locationID", "leadtimeValue"), na.rm=TRUE)



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

