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

#UI buildup
#dates

######################################
# 
# print("Starting: ")
# Sys.time()
# # #allDates <- filter(distinct(tbl_scores, dateValue), rank(dateValue)==1)
# # allDates <- filter(tbl_scores, !is.null(dateValue))
# 
# # qry <- tbl_scores %>% 
# #   filter(locationID %in% c("1","2","3") %>% 
# #   group_by()  %>% 
# #   summarise(count = n()) %>%
# #   collect()
# 
# print("Collecting: ")
# Sys.time()
# 
# system.time(allDates <- collect(allDates, n=Inf))
# 
# print("Filtering: ")
# Sys.time()
# system.time(firstDate <- filter(allDates, rank(dateValue)==1))
# 
# print("Ending: ")
# Sys.time()
# 
######################################

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

#this is dumb
#fulldb <- collect(tbl_scores, n=Inf)
#db2005 <- subset(fulldb, dateValue > "2005-01-01" & dateValue < "2005-12-31")



# sm <- subset(lcldb, locationID %in% c('S2242510') & scoreType == "Seasonal_LS_month")
#sm <- subset(lcldb, locationID %in% c('S2242510'))
#sm2 <- subset(sm, dateValue > "2005-01-01" & dateValue < "2005-12-31")
# reduced <- filter(tbl_scores, locationID %in% c('S2242510') & dateValue > "2005-01-01" & dateValue < "2005-12-31" ) #%in% works, except this is a multi-cond qry... 
reduced <- filter(tbl_scores, locationID == c('S2242510') & dateValue > "2005-01-01" & dateValue < "2005-12-31" )

#
# for (LeadTime in 1:90)
#  {
#local <- collect(filter(reduced, LT==LeadTime))
 local <- collect(reduced)
  ggp <- ggplot(local,aes(x = dateValue , y = (scoreValue - mean(scoreValue)))) +
    geom_line(aes(color = LT), size=1) 
  # +
  #   scale_x_date("Lead Time (weeks)") + scale_y_continuous("CRPS for Lead Time ", LT)  
    #print(ggp)
# }

  ggp + facet_grid(scoreValue ~ LT)


# sm2 <- filter(sm, LT==LeadTime)
for (LeadTime in 1:9)
{ ggp <- ggplot(sm,aes(x = dateValue , y = (scoreValue - mean(scoreValue)))) +
    geom_point(aes(color = 'red'), size=1) +
    scale_x_date("Lead Time (weeks)") + scale_y_continuous("CRPS for Lead Time ")  
  #print(ggp)
  ggp + facet_grid(. ~ LT)
}