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
tbl.forecastsetup <- tbl(db, "tblForecastSetup")


######################### plot skill scores
compare.ss.plot <- filter(tbl.scores, 
                            # locationID %in% list.lots.basins.ehype &
                            caseStudy == 1 &
                            locationID %in% c('9565063','9000963','9509300', '9563711') &
                            modelVariable == "Streamflow" &
                            # forecastType  == "Bias Correction 1" & # can be both
                            scoreType    == "CRPS"
                          # leadtimeValue %in% toto
)
toto1 <- structure(collect(compare.ss.plot))
unique(toto1$forecastSystem)
unique(toto1$locationID)

#ERR - dff sizes df...
unique(toto1$leadtimeValue) #voila, le probleme avec leadtimes 
# TODO howto join these in R prod code??
toto1 <- filter(toto1, leadtimeValue %in% c(1,2,3,4,5,6))

# ex. EHYPE is ref System, EFAS is "new"

#step 0.5, build new column
#n'oublie pas "ref"!
toto1$reference = NA
toto1$reference[toto1$forecastType=="Bias Correction 2"] = "ref"
toto1$reference[toto1$forecastType!="Bias Correction 2"] = "new"
# toto1 <- distinct(toto1)
# length(toto1)

  unique(toto1$forecastType) # =="Bias Correction 2"
  
  # c <- table(unlist(toto1$ref))
  # new ref 
  # 576 288
  
#step 1, aggregate dataet
agg <- c("forecastSetup", "forecastSystem", "forecastType", "locationID", "leadtimeValue", "scoreType", "reference")
# fifi <- summarySE(tot_system, "scoreValue", agg, na.rm = T)
toto2 <- summarySE(data = toto1, "scoreValue", agg, na.rm = T)
glimpse(toto2)
head(toto2)
tail(toto2)
unique(toto2$locationID)

#missed the LT filter, got
# Warning message:
#   In xx[ref == "new", col]/xx[ref == "ref", col] :
#   longer object length is not a multiple of shorter object length
# toto2 <- filter(toto2, leadtimeValue %in% c(1,2,3,4,5,6))
#step 2, run thru new function

for (loc in unique(toto2$locationID)) {
  print(paste("loc:", loc))
  loc.out <- skillScore(toto2[toto2$locationID==loc, ])
}

toto3 <- skillScore(toto2)




skillScore <- function(dl) {
  data <- as.list(split(dl[ , c("reference", "scoreValue")], f=as.factor(dl$locationID)) )
  list.out  <- lapply(data, function(x){  
    ss   = 1 - (x[x$reference == "new", "scoreValue"] / x[x$reference == "ref", "scoreValue"])
  })
  df <- as.data.frame(list.out)
  #xformed to factors, drop the leading X
  names(df) <- sub(pattern = "X", replacement = "", colnames(df))
  df <- stack(df)
  colnames(df) <- c("scoreValue", "locationID")
  df$leadtimeValue <- rep(unique(dl$leadtimeValue), times = length(unique(dl$locationID)))
  return(df)
}


# skillScore <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
# NOTE avoid "/ by 0" errors

#3, run toto1 thru something like this to frame up
toto5 = data.frame(LocationID = rep(unique(toto2$locationID), each = length(unique(toto2$leadtimeValue))), 
                   leadtimeValue = rep(unique(toto2$leadtimeValue), times = length(unique(toto2$locationID))), 
                   ScoreValue = toto3[2:25]) # 25 - length(toto3)


rep(1:6, times = length(locationID))
    
# works
data <- as.list(split(toto2[ , c("reference", "scoreValue")], f=as.factor(toto2$locationID)) )
list.out  <- lapply(data, function(x){  
  ss   = 1 - (x[x$reference == "new", "scoreValue"] / x[x$reference == "ref", "scoreValue"])
} )


toto7 <- as.data.frame(list.out)
#xformed to factors, drop the leading X
names(toto7) <- sub(pattern = "X", replacement = "", colnames(toto7))


colnames(toto7)[1:4]


toto8 <- data.frame(LocationID = rep(colnames(toto7), each = length((rownames(toto7)))), 
           leadtimeValue = rep(rownames(toto7), times = length(toto7)),
           scoreValue = toto7[,1])

toto9 <- stack(toto7, select(c(row.names(toto7))))

colnames(toto9) <- c("scoreValue", "locationID")

library(ggplot2)
qplot(toto8, leadtimeValue, scoreValue, col = LocationID)

ggplot(toto8, aes(x = toto8$leadtimeValue, y = toto8$scoreValue, col = toto8$LocationID))
ggplot(toto8, aes(x = leadtimeValue, y = scoreValue))



plot(toto8$leadtimeValue, toto8$scoreValue, group_by(toto8$LocationID))

cbind(toto8, t(toto7))

,
           scoreValue = toto7[,colnames(toto7)[1:4]])


colnames(toto7)
rownames(toto7)

toto8 = data.frame(LocationID = colnames(toto7),
                   each = rownames(toto7)), 
                   leadtimeValue = rep(unique(rownames(toto7)), 
                                       times = length(unique(toto2$locationID))), 
                   ScoreValue = toto3[2:25]) # 25 - length(toto3)


qplot(toto7, x = )




plotInput <- 
  ggplot(toto5,  aes(color = locationID, x = leadtimeValue, y = scoreValue ))

toto5 <- data.frame(LocationID = rep(unique(toto2$locationID), each = 6),
                    leadtimeValue = rep(unique(toto2$leadtimeValue), times = length(unique(toto2$locationID))), 
                    ScoreValue = toto3[2:25]
                    )




# toto5 = data.frame(LocationID = rep(unique(toto1$locationID), each = length(unique(toto1$leadtimeValue))), 
#                    leadtimeValue = rep(unique(toto1$leadtimeValue), times = length(unique(toto1$locationID))), 
#                    ScoreValue = toto4[3:26])

# measurevar = "scoreValue",
skillScore <- function(data, measurevar = "scoreValue", groupvars=NULL, na.rm=FALSE, .drop=TRUE) {
  library(dplyr)
  # check for "ref"
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else length(x)
  }
  
  datac <- plyr::ddply(data, groupvars, .drop=.drop,
                       .fun = function(xx, col, ref) {
                        print(xx[[col]])
                        c(
                           # N    = length2(xx[[col]], na.rm=na.rm),
                           ss   = 1 - (xx[ref == "new", col] / xx[ref == "ref", col]) #,
                           # mean = mean   (xx[[col]], na.rm=na.rm)
                           # sd   = sd     (xx[[col]], na.rm=na.rm)
                         )
                       }, measurevar, data$ref 
  )

  return(datac)
}


qplot(list.out, x = )


datac <- plyr::ddply(data, groupvars, .drop=.drop,
                     .fun = function(xx, col, ref) {
                       print(xx[[col]])
                       c(
                         # N    = length2(xx[[col]], na.rm=na.rm),
                         ss   = 1 - (xx[ref == "new", col] / xx[ref == "ref", col]) #,
                         # mean = mean   (xx[[col]], na.rm=na.rm)
                         # sd   = sd     (xx[[col]], na.rm=na.rm)
                       )
                     }, measurevar, data$ref 
)


######################### plot skill scores

tmpLocationName <-
  distinct(select(tbl.scores, locationID, caseStudy))
ctlLocationName <- collect(tmpLocationName)
ctlLocationName <-
  arrange_(ctlLocationName, "caseStudy", "locationID")

glimpse(ctlLocationName)

ctlLocationName["locationID"]

# if(is.null(ctlCaseStudy))
#   return()
# CaseStudy <- setNames(ctlCaseStudy$ObjectInteger, ctlCaseStudy$ObjectItemName)
# # CaseStudy = paste(ctlCaseStudy$ObjectItemName, "\"=\"", ctlCaseStudy$ObjectInteger ) # not the way
# selectInput("rtnCaseStudy", 
#             paste("Case Study: (",length(unique(ctlCaseStudy$ObjectItemName)), ")"), choices = CaseStudy, multiple = F)

# enc2utf8(ctlCaseStudy$ObjectItemName)
ctlCaseStudy$ObjectItemName <- encodeString(ctlCaseStudy$ObjectItemName)

dbListConnections(RPostgreSQL()) list()

library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(PostgreSQL(), user=REuser, password=REpassword, dbname=REdbname) 
# after working awhile...
for(con in dbListConnections(drv)){
   dbGetStatement(dbListResults(con))
}

dbGetQuery(credentials, "show variables like 'character_set%'")

dbGetInfo(con)


  Sys.getlocale()[1]

list.lots.basins.ehype <- c(
  '8000100',
  '8000133',
  '8000179',
  '8000190',
  '9783018',
  '9787525',
  '9787915'
)

compare.ss.plot <- filter(tbl.scores, 
                 scoreNA == FALSE &
                   # locationID %in% list.lots.basins.ehype &
                   locationID %in% c('8000179','8000190') &
                   modelVariable == "Streamflow" &
                   # forecastType  == "Bias Correction 1" & # can be both
                   # forecastType  == "Seasonal_EDMD_month" &
                   scoreType    == "CRPS"
                 # leadtimeValue %in% toto
)
getit <- structure(collect(remote))


  

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





# from server.R, prod app

tmpCaseStudy <-
  filter(tbl.interface,
         ObjectName == "Case Study" & LanguageID == RElanguage)
ctlCaseStudy <- collect(tmpCaseStudy)

tmpSystem <-
  filter(tbl.interface,
         ObjectName == "System" & LanguageID == RElanguage)
ctlSystem <- collect(tmpSystem)

# t.forecast.setup <- collect(db, tbl.forecastsetup)

tmpSetup <- select(tbl.forecastsetup, ID, forecastSetup)
ctlSetup <- collect(tmpSetup)

# tmpForecastSetup <-
#   select(tbl.scores, forecastSystem)
# ctlForecastSetup <- arrange_(distinct(collect(tmpForecastSetup, n=Inf)))

# directly from the score table
tmpScoreType <-
  select(tbl.scores, scoreType)
ctlScoreType <- arrange_(distinct(collect(tmpScoreType, n=Inf)))

tmpModelVariable <-
  select(tbl.scores, modelVariable)
ctlModelVariable <- arrange_(distinct(collect(tmpModelVariable, n=Inf)))

# was filtering by multiple datapackageGUIDs before, not necessary now?
tmpLocationName <-
  distinct(select(tbl.scores, locationID, caseStudy))
ctlLocationName <- collect(tmpLocationName)
ctlLocationName <-
  arrange_(ctlLocationName, "caseStudy", "locationID")




fifi <- select(tbl.scores, c(forecastSystem, forecastSetup))
# fifi2 <- select(tbl.forecastsetup, c(ID, forecastSetup))
fifi <- filter(fifi, forecastSystem == "E-HYPE")
fifi <- unique(collect(fifi, n=Inf))
# fifi <- cbind(fifi, tbl.forecastsetup)

# Locations <- NULL
# Locations <- select(tbl.scores, c(locationID, caseStudy, forecastSystem, forecastType))
# Locations <- filter(Locations, caseStudy == "1" & forecastSystem == "E-HYPE" & forecastType == "Bias Correction 1")
# Locations <- select(Locations, locationID)
# Locations <- distinct(Locations)
# Locations <- collect(Locations, n=Inf)
# Locations <- structure(Locations)
# # Locations <- data.frame(unique(Locations$locationID))


# ### playing w Setup control(s)
# Setup <- NULL
# Setup <- select(tbl.scores, c(caseStudy, forecastSystem, forecastSetup, forecastType))
# Setup <- filter(Setup, forecastSystem=="E-HYPE" & caseStudy=="1")
# Setup <- unique(collect(Setup, n=Inf))
# 
# Setup <- Setup[Setup$forecastSetup == 1]
# # Setup <- Setup[Setup$forecastSetup == Setup$forecastSetup]
# 
# # Setup <- filter(tbl.forecastsetup, ID==Setup$forecastSetup)
# # Setup <- filter(tbl.forecastsetup, ID==1)
# Setup <- collect(Setup)
# Setup$forecastSetup
# # Setup <- cbind(fifi, tbl.forecastsetup)
# str(Setup)


