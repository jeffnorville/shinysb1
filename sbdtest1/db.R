#ini file
readRenviron("~/R/shinysb1/.Renviron")
REdbname = Sys.getenv('pgdb')
REuser = Sys.getenv('api_user')
RElanguage = Sys.getenv('api_language')
REpassword = Sys.getenv('pgpassword')

#db connections
library(dplyr)
library(ggplot2)

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


# #selectInput boxes
# tmpScoreType <- filter(tbl(db, "tblInterface"),ObjectName=="Score Type" & LanguageID == "1")
# ctlScoreType <- collect(tmpScoreType)
# 
# tmpModelVariable <- filter(tbl(db, "tblInterface"),ObjectName=="Model Variable" & LanguageID == "1")
# ctlModelVariable <- collect(tmpModelVariable)
# 
# tmpLocationName <- filter(tbl(db, "tblInterface"),ObjectName=="Location Name" & LanguageID == "1")
# ctlLocationName <- collect(tmpLocationName)
# 
# tmpCaseStudy <- filter(tbl(db, "tblInterface"),ObjectName=="Case Study" & LanguageID == "1")
# ctlCaseStudy <- collect(tmpCaseStudy)
# 
# tmpCaseStudy <- filter(tbl(db, "tblInterface"),ObjectName=="Case Study" & LanguageID == "1")
# ctlCaseStudy <- collect(tmpCaseStudy)

# sm <- subset(lcldb, locationID %in% c('S2242510') & scoreType == "Seasonal_LS_month")
#sm <- subset(lcldb, locationID %in% c('S2242510'))
#sm2 <- subset(sm, dateValue > "2005-01-01" & dateValue < "2005-12-31")
# reduced <- filter(tbl_scores, locationID %in% c('S2242510') & dateValue > "2005-01-01" & dateValue < "2005-12-31" ) #%in% works, except this is a multi-cond qry... 
# reduced <- filter(tbl_scores, locationID == c('S2242510') & dateValue > "2005-01-01" & dateValue < "2005-12-31" )
reduced <- filter(tbl_scores, locationID == c('S2242510') & leadtimeValue == 5 )

local <- collect(reduced)
#  as.POSIXlt(date1)$mon
season.winter <- c(12, 1, 2) # december, january, february
lclwintr <- filter(local, as.POSIXlt(dateValue)$mon+1 %in% season.winter & forecastType == "Seasonal_EDMD_month")

#non-POSIX
# season.winter <- c('décembre', 'janvier', 'février')
# local <- filter(local, months(dateValue) %in% season.winter & forecastType == "Seasonal_EDMD_month")
# ggplot(local,aes(x = leadtimeValue, y = (scoreValue - mean(scoreValue)))) +
  
ggplot(local,aes(x = date, y = (scoreValue - mean(scoreValue)))) +
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