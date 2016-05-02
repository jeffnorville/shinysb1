#db connections
library(dplyr)

db <- src_postgres('postgres',
                   host = 'localhost',
                   port = 5432,
                   user = 'postgres',
                   password = hiddenpassword)
tbl_scores <- tbl(db, "tblScores")

fulldb <- collect(tbl_scores, n=Inf)
db2005 <- subset(fulldb, dateValue > "2005-01-01" & dateValue < "2005-12-31")

#goal tonight -- dplyr working for me
# when is db queried?
# types of queries?
# best to reduce locally or server-side?
# "windows?


# scoreTypeList <- distinct(tbl_scores$scoreType)
# sctp <- collect(scoreTypeList)

# print(translate_sql( tbl = tbl_scores, window = TRUE))

# flights_postgres <- tbl(src_postgres("nycflights13"), "flights")


# 
#  scores <- tbl(db, "tblScores")
#  lScoreTypes <- unique(scores$scoreType)
#  distinct_df = scores %>% distinct(scoreType)
# tst1 <- select(filter(scores, locationID == 'M0243010', dateValue == '1981-08-01', LT > 1), scoreValue:1)
# 
#  select(filter(scores, dateValue > '01/01/2009'), locationID)
#  
#  select(filter(scores, scoreType), scoreType)
# 
#  rmt1 <- select(filter(scores, scoreType))
#  rmt2 <- collapse(rmt1)
#  
# # collect(filter(scores, scoreType != NULL))
#  
#  stuff = ident(scores, dplyr::sql('SELECT distinct("scoreType") FROM tblScores'))
#  

# 
  #    sm <- subset(fulldb, locationID %in% c(ctlLocid))
  sm <- subset(db2005, locationID %in% c('S2242510') & scoreType == "Seasonal_LS_month")
  #sm2 <- subset(sm, dateValue > "2005-01-01" & dateValue < "2005-12-31")
  ggplot(sm,aes(x = LT / 7, y = dateValue)) +
    geom_point(aes(color = scoreValue), size=3) +
    scale_x_continuous("Lead Time (weeks)") + scale_y_date("Months of 2005 (January omitted)") +
    scale_color_gradient(low="yellow", high="darkgreen")
