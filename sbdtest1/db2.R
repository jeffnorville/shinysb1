#ini file
# setwd("~/R/shinysb1/sbdtest1")
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
tbl.locations <- tbl(db, "tblLocation")


# db "windows?

# done in server.R (NON-REACTIVE QUERY):
# ? vs ??

#  TODO time the influence of breaking this into two calls
# int.list <- c(1:15) # was leadtimes, not used
# toto <- int.list

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
                 # scoreNA == FALSE &
                 # locationID %in% list.lots.basins.ehype &
                 # locationID %in% c('S2242510', 'L4411710') &
                 # dataPackageGUID == "" &
                 modelVariable == "Streamflow" &
                   forecastType  == "Linear Scaling (Seasonal_LS_month)" &
                   scoreType %in% c("CRPS Skill Score", "CRPSS", "RMSE Skill Score", "RMSES", "Brier Skill Score")
                 # "Linear Scaling (Seasonal_LS_month)"
                 # forecastType  == "Seasonal_EDMD_month" &
                 # summarizeByTime == "All" &
                 # scoreType    == "CRPS" &
                 # leadtimeValue %in% toto
)

remote <- filter(remote, dataPackageGUID == "SMHI2222")

remote <- filter(remote, locationID %in% list.lots.basins.ehype)

getit <- structure(collect(remote, n = Inf))

# getit <- filter(getit, dataPackageGUID == "SMHI2222")



# move to REACTIVE section so this can be datamined "live"
# reduced <- filter(getit, locationID %in% c('S2242510', 'L4411710') & scoreType == "CRPS")


# base plot "all skill scores"
# getit <- filter(getit, leadtimeValue %in% 1:2)
unique(getit$dataPackageGUID)
unique(getit$scoreType)
unique(getit$forecastType)
unique(getit$datePartValue)
unique(getit$modelVariable)



# average scores by locn, LT and scoreType (builds stats based on month)
mngetit <- summarySE(
  getit,
  measurevar = "scoreValue",
  groupvars = c("locationID", "leadtimeValue", "scoreType", "forecastType"),
  na.rm = TRUE
)


# average scores by locn, LT and scoreType AND MONTH
mngetit <- summarySE(
  getit,
  measurevar = "scoreValue",
  groupvars = c("locationID", "leadtimeValue", "scoreType", "forecastType", "datePartValue"),
  na.rm = TRUE
)


length(unique(mngetit$locationID))

# SkillScoreDashboard
ggplot(mngetit, aes(x = leadtimeValue,  y = scoreValue, na.rm = TRUE, colour = locationID) ) +
  geom_line() +
  geom_point() +
  scale_shape_identity() +
  facet_grid(scoreType ~.)


unique(playdata$scoreType)
unique(playdata$locationID)

playdata <- mngetit

### Working on error trend
ggplot(playdata, aes(x = leadtimeValue,  y = scoreValue, na.rm = TRUE, colour = locationID) ) +
  scale_y_continuous(name="") +
  scale_x_continuous(name="") +
  scale_shape_identity() +
  geom_point(shape=24, size=5) + # color=
  facet_grid(locationID ~ scoreType)
# locationID
# for production: scoreType



lapply(X = a, function(x) mean(x), simplify = "array")




ggplot(playdata, aes(x = leadtimeValue,  y = scoreValue, na.rm = TRUE, colour = locationID) ) +
  scale_y_continuous(name="") +
  scale_x_continuous(name="") +
  scale_shape_identity() +
  if (playdata$scoreValue[[1]] < playdata$scoreValue[[2]]) {
    geom_point(shape=24) 
  } else {
    geom_point(shape=25) 
  } +
  facet_grid(locationID ~.)


# ColorBrewer for color ramps


# B&W faceted, not bad
qplot(factor(leadtimeValue), 
      y = scoreValue, 
      data = mngetit, 
      facets = scoreType ~ locationID)

ggplot(data = mngetit,
       aes(x = leadtimeValue, 
           y = scoreValue, 
           facets = scoreType ~ locationID))




# this shows all months
ggplot(getit, aes(x = leadtimeValue, y = scoreValue, na.rm = TRUE, colour = locationID)) +
  geom_point() +
  facet_grid(~ scoreType)

# avgd scores by month
# this shows all months
weedsubset(mngetit, locationID %in% list.lots.basins.ehype)

ggplot(mngetit, aes(x = leadtimeValue, y = scoreValue, na.rm = TRUE, colour = locationID)) +
  if (mngetit$scoreValue[[1]] < mngetit$scoreValue[[2]]) {
    geom_point(shape=24)
  } else {
    geom_point(shape=25)
  } +
  facet_grid(locationID ~ .)

# facet_grid(scoreType ~ .)

unique(mngetit$locationID[1:40])
length(mngetit$locationID==8000133)

ggplot(subset(mngetit, locationID %in% locationID[1:42]), aes(x = leadtimeValue, y = scoreValue, na.rm = TRUE, colour = scoreType)) +
  geom_line() +
  geom_point() +
  facet_grid(locationID ~ .)


# this shows all months with CI bars
pd <- position_dodge(0.2)
ggplot(mngetit, aes(x = leadtimeValue, y = scoreValue, na.rm = TRUE, colour = locationID)) +
  # geom_line() +
  geom_point(aes(color = locationID), position = pd, size = 2) +
  geom_errorbar(aes(ymin = scoreValue - ci, ymax = scoreValue + ci), position = pd) +
  facet_grid(scoreType ~ .)


# all skill scores - attempt 3
pd <- position_dodge(0.2)
ggplot(mngetit[], aes(x = leadtimeValue, y = scoreValue, na.rm = TRUE, colour = se)) +
  # geom_line() +
  # geom_point(aes(color = locationID), position = pd, size = 2) +
  # geom_errorbar(aes(ymin = scoreValue - ci, ymax = scoreValue + ci), position = pd) +
  facet_grid(locationID ~ .)

mngetit['locationID'=="8000277"]
length(mngetit$locationID)

# ALL SKILL SCORES



geom_linerange()
geom_qq()
geom_dotplot()


?pch
plot -- adding points to a plot  


plot(getit$leadtimeValue, getit$scoreValue, col=getit$locationID)


# xlab = "Lead Times", ylab = "Score")




# CRPSS, CRPSS
# reduced <- filter(getit, scoreType == "RMSES")
reduced <- filter(getit, scoreType == "CRPS")
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

