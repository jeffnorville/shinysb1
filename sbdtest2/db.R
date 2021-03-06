# Scoreboard 2
#ini file
setwd("~/R/shinysb1/sbdtest1")
readRenviron("~/R/shinysb1/.Renviron")
REhost =     Sys.getenv('pgserver')
REport =     Sys.getenv('pgport')
REdbname =   Sys.getenv('pgdb')
REuser =     Sys.getenv('api_user')
RElanguage = Sys.getenv('api_language')
REpassword = Sys.getenv('pgpassword')

setwd("~/R/shinysb1/sbdtest1")
source("global.R")

#db connections
# library(plyr); library(dplyr)
library(dplyr)
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
tbl.scores <- tbl(db, "tblScores")



# db "windows?

# done in server.R (NON-REACTIVE QUERY):
# ? vs ??

#  TODO time the influence of breaking this into two calls
int.list <- c(1:15)
# toto <- as.numeric(int.list)
# all.lead.times <- as.integer(unlist(int.list))  # strsplit(input$lead.times, split = ":"))
# 
# if (all.lead.times[1] == all.lead.times[2]) {
#   toto = toto
# } else {
#   toto = all.lead.times[1]:all.lead.times[2]
# }
# doesn't make sense here, only with slider where [2] is the MAX of the series
toto <- int.list

# broken into 2 
remote <- filter(tbl.scores, 
                   scoreNA == FALSE &&
                   locationID %in% c('8000100','9771083') &&
                   modelVariable == "Streamflow" &&
                   forecastType  == "Seasonal_EDMD_month" &&
                   # summarizeByTime == "All" &&
                   # scoreType    == "CRPS" &&
                   leadtimeValue %in% toto
  )


getit <- structure(collect(remote))


# move to REACTIVE section so this can be datamined "live"
# reduced <- filter(getit, locationID %in% c('S2242510', 'L4411710') & scoreType == "CRPS")

# ADD DATE, SEASONAL FILTER  ... dateValue
# if (summarizeByTime == "All"){
#   summarize.by <- "Month"
# } else if (summarizeByTime == "Month"){
#   summarize.by <- "Month"
# } else if (summarizeByTime == "Spring (MAM)"){
#   
# } else if (summarizeByTime == "Winter (DJF)"){
# } else if (summarizeByTime == "Monsoon (JJAS)"){
# } else if (summarizeByTime == "Year"){
# } else {
#   
# }

# CRPSS, CRPSS
reduced <- filter(getit, scoreType == "RMSES")
# reduced <- filter(getit, Month(dateValue) == 2)

local <- collect(reduced)

# daily, monthly (should vectorize but doesn't matter since df here will be consistent)
if(local$leadtimeUnit == "day") {
  # local$month <- format(local$dateValue, "%m")
  # getit$months <- months(getit$dateValue) # "février"
  local$months <- format.Date(local$dateValue, "%m")
}

#  as.POSIXlt(date1)$mon
# season.winter <- c(12, 1, 2) # december, january, february
# lclwintr <- filter(local, as.POSIXlt(dateValue)$mon+1 %in% season.winter )


# lcsmry <- summarySE(local, measurevar = "scoreValue", groupvars = c("locationID","leadtimeValue"))
# summarySE(local, measurevar="scoreValue", groupvars=c("locationID", "leadtimeValue"), na.rm=TRUE)
loc.sum <- summarySE(local, measurevar="scoreValue", groupvars=c("locationID", "leadtimeValue"), na.rm=TRUE)
loc.sum$locationID <- as.factor(loc.sum$locationID)

# this doesn't really do it
# group <- c(1:length(loc.sum$locationID))
# group <- factor(c(loc.sum$locationID))

# base plot
plot(loc.sum$leadtimeValue, loc.sum$scoreValue, col=loc.sum$locationID, 
     xlab = "Lead Times", ylab = "Score")

# get fancier
pd <- position_dodge(0.2)
min.LT <- min(loc.sum$leadtimeValue)
max.LT <- max(loc.sum$leadtimeValue)

ggplot(loc.sum, aes(color = locationID, x = leadtimeValue, y = scoreValue )) +
  geom_errorbar(aes(ymin=scoreValue-ci, ymax=scoreValue+ci), position = pd) + # , color="grey"
  geom_line() +
  geom_point(aes(color = locationID), position = pd) +
  geom_hline(aes(yintercept=0), color="blue", linetype="dashed") + 
  scale_y_continuous(breaks=c(min.LT:max.LT)) +
  xlab("Lead Times") + ylab("Score")


# geom_errorbar(aes(ymin=scoreValue-se, ymax=scoreValue+se), position = pd, color="grey") + # 


#with CIs
pd <- position_dodge(0.1)

# CONTINUE WORKING HERE -- DEFINE COLOR RAMP
# geom_point ... factors? howto tie color of geom_errorbar to geom_point? 
ggplot(loc.sum, aes(x = leadtimeValue, y = scoreValue ) ) +
  geom_point(aes(color = locationID)) +
  geom_errorbar(aes(ymin=scoreValue-ci, ymax=scoreValue+ci),width=.1, position=pd) +
  # geom_line(position=pd) +
  geom_hline(aes(yintercept=0), colour="black", linetype="dashed") + # colour="#990000"
  xlab("Lead Times") + ylab("Score") 


ggplot(loc.sum, aes(x = leadtimeValue, y = scoreValue ) ) +
  geom_point(aes(color = locationID, size=3)) +
  # geom_line(position=pd) +
  geom_errorbar(aes(ymin=scoreValue-ci, ymax=scoreValue+ci), width=.1, color = group, position=pd) + # color="grey",
  geom_hline(aes(yintercept=0), colour="black", linetype="dashed") + # colour="#990000"
  # theme(legend.position="none") +
  xlab("Lead Times") + ylab("Score") # "Score"

# mmm ... nope
ggplot(aes(x = leadtimeValue, y = scoreValue, ymin=scoreValue-1, ymax=scoreValue+1), data=loc.sum ) +
  geom_area(position="stack") +
  geom_point(color="red")


ggplot(loc.sum, aes(x = leadtimeValue, y = scoreValue, ymin=scoreValue-ci, ymax=scoreValue+ci, fill = locationID)) +
  geom_boxplot()


ggplot(local, aes(x = leadtimeValue, y = scoreValue, fill  = locationID)) +
  geom_boxplot()


  geom_point(aes(color = locationID)) +
  geom_errorbar(aes(ymin=scoreValue-ci, ymax=scoreValue+ci),width=.1, position=pd) +
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

