#ini file
setwd("~/R/shinysb1/sbdtest1")
readRenviron("~/R/.Renviron")
REhost =     Sys.getenv('pgserver')
REport =     Sys.getenv('pgport')
REdbname =   Sys.getenv('pgdb')
REuser =     Sys.getenv('api_user')
RElanguage = Sys.getenv('api_language')
REpassword = Sys.getenv('pgpassword')

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
tbl.interface <- tbl(db, "tblInterface")

list.lots.basins.ehype <- c(
  '8000100',
  '8000133',
  '8000179',
  '8000190',
  '9783018',
  '9787525',
  '9787915'
)

remote <- filter(tbl.scores, 
                 scoreNA == FALSE &
                   # locationID %in% list.lots.basins.ehype &
                   locationID %in% c('8000179','8000190') &
                   modelVariable == "Streamflow" &
                   forecastType  == "Bias Correction 1" &
                 # forecastType  == "Seasonal_EDMD_month" &
                   scoreType    == "CRPS"
                 # leadtimeValue %in% toto
)
getit <- structure(collect(remote))


system1 <- getit$forecastSystem
  
system2 <- getit$forecastSystem
  

loc.sum <- summarySE(
      getit, 
      measurevar="scoreValue", 
      groupvars=c("locationID", "leadtimeValue", "scoreType", "forecastType"), 
      na.rm=TRUE)

loc.sum$locationID <- as.factor(loc.sum$locationID)


















# db$con <- NULL

if (!is.null(db$con)){
  paste("Connected to ", db$info$host, " as ", db$info$user)
} else {
  "database not connected, loading localdefault RDS file"
}
  


db$info$user
db_list_tables(db)

toto <- select(tbl.interface,
               ObjectName, ObjectItemName, ObjectInteger)

toto <- filter(tbl.interface,
               ObjectName=="Case Study") # , ObjectItemName, ObjectInteger

toto <- collect(toto)

a <- print(tbl_df(tbl.interface))

tmpForecastSetup <-
  select(tbl.scores, forecastSystem)
ctlForecastSetup <- arrange_(distinct(collect(tmpForecastSetup, n=Inf)))



tmpModelVariable <-
  select(tbl.scores, modelVariable)
ctlModelVariable <- arrange_(distinct(collect(tmpModelVariable, n=Inf)))

ctlModelVariable$modelVariable



list.lots.basins.ehype <- c(
  '8000100',
  '8000133',
  '8000179',
  '8000190',
  '9783018',
  '9787525',
  '9787915'
)


# broken into 2 
remote <- filter(tbl.scores, 
                   scoreNA == FALSE &
                   locationID %in% list.lots.basins.ehype &
                   # locationID %in% c('S2242510', 'L4411710') &
                   modelVariable == "Streamflow"
                   # forecastType  == "Linear Scaling (Seasonal_LS_month)" &
                   # forecastType  == "Seasonal_EDMD_month" &
                   # summarizeByTime == "All" &
                   # scoreType    == "CRPS" &
                   # leadtimeValue %in% toto
  )


getit <- structure(collect(remote))




pd <- position_dodge(0.2)
# min.LT <- min(loc.sum$leadtimeValue)
# max.LT <- max(loc.sum$leadtimeValue)

ggplot(loc.sum,
       aes(color = locationID, x = leadtimeValue, y = scoreValue)) +
  # geom_errorbar(aes(ymin = scoreValue - ci, ymax = scoreValue + ci), position = pd) + # , color="grey"
  geom_line() +
  geom_point(aes(color = locationID), position = pd) +
  # geom_hline(aes(yintercept=0), color="blue", linetype="dashed") +
  #   # if (do.facets == TRUE){facet_wrap(~ locationID) } +
  # scale_y_discrete() +
  # scale_y_continuous(breaks = c(min.LT:max.LT)) +
  xlab("Lead Times") + ylab(paste(input$rtnScoreType))


# move to REACTIVE section so this can be datamined "live"
# reduced <- filter(getit, locationID %in% c('S2242510', 'L4411710') & scoreType == "CRPS")


# CRPSS, CRPSS
# reduced <- filter(getit, scoreType == "RMSES")
reduced <- filter(getit, scoreType == "CRPS")
# reduced <- filter(getit, Month(dateValue) == 2)

local <- collect(reduced)

# # daily, monthly (should vectorize but doesn't matter since df here will be consistent)
# if(local$leadtimeUnit == "day") {
#   # local$month <- format(local$dateValue, "%m")
#   # getit$months <- months(getit$dateValue) # "février"
#   local$months <- format.Date(local$dateValue, "%m")
# }

#  as.POSIXlt(date1)$mon
# season.winter <- c(12, 1, 2) # december, january, february
# lclwintr <- filter(local, as.POSIXlt(dateValue)$mon+1 %in% season.winter )


# lcsmry <- summarySE(local, measurevar = "scoreValue", groupvars = c("locationID","leadtimeValue"))
# summarySE(local, measurevar="scoreValue", groupvars=c("locationID", "leadtimeValue"), na.rm=TRUE)
loc.sum <- summarySE(getit, measurevar="scoreValue", groupvars=c("locationID", "leadtimeValue", "scoreType", "forecastType"), na.rm=TRUE)
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

# geom_point ... factors? howto tie color of geom_errorbar to geom_point? 
ggplot(local, aes(x = leadtimeValue, y = scoreValue ) ) +
  geom_point(aes(color = locationID)) +
  geom_errorbar(aes(ymin=scoreValue-ci, ymax=scoreValue+ci),width=.1, position=pd) +
  # geom_line(position=pd) +
  geom_hline(aes(yintercept=0), colour="black", linetype="dashed") + # colour="#990000"
  xlab("Lead Times") + ylab("Score") 




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

