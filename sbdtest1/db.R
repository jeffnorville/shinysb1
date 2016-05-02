#ini file
readRenviron("~/R/shinysb1/.Renviron")
REdbname = Sys.getenv('pgdb')
REuser = Sys.getenv('api_user')
REpassword = Sys.getenv('pgpassword')

#db connections
library(dplyr)
library(ggplot2)

db <- src_postgres('postgres',
                   host = 'localhost',
                   port = 5432,
                   user = REuser,
                   password = REpassword)
tbl_scores <- tbl(db, "tblScores")

#this is dumb
#fulldb <- collect(tbl_scores, n=Inf)
#db2005 <- subset(fulldb, dateValue > "2005-01-01" & dateValue < "2005-12-31")

#dbsub <- select(filter(tbl_scores, dateValue > "2005-01-01" & dateValue < "2005-12-31"), locationID:4)
dbsub <- filter(tbl_scores, dateValue > "2005-01-01" & dateValue < "2005-12-31" & scoreType == "Seasonal_LS_month")

lcldb <- collect(dbsub)

# sm <- subset(lcldb, locationID %in% c('S2242510') & scoreType == "Seasonal_LS_month")
sm <- subset(lcldb, locationID %in% c('S2242510'))
#sm2 <- subset(sm, dateValue > "2005-01-01" & dateValue < "2005-12-31")

#
for (LeadTime in 1:90)
 {
  sm2 <- filter(sm, LT==LeadTime)
  ggp <- ggplot(sm2,aes(x = dateValue , y = (scoreValue - mean(scoreValue)))) +
    geom_line(aes(color = LeadTime), size=1) +
    scale_x_date("Lead Time (weeks)") + scale_y_continuous("CRPS for Lead Time ", LeadTime)  
    #print(ggp)
}
ggp + facet_grid(. ~ LT)


# sm2 <- filter(sm, LT==LeadTime)
for (LeadTime in 1:9)
{ ggp <- ggplot(sm,aes(x = dateValue , y = (scoreValue - mean(scoreValue)))) +
    geom_point(aes(color = 'red'), size=1) +
    scale_x_date("Lead Time (weeks)") + scale_y_continuous("CRPS for Lead Time ")  
  #print(ggp)
  ggp + facet_grid(. ~ LT)
}