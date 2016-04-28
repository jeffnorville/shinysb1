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