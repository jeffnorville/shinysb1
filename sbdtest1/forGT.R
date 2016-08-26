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

toto1 <- filter(toto1, leadtimeValue %in% c(1,2,3,4,5,6))


#step 0.5, build new column
#n'oublie pas "reference"!
toto1$reference = NA
toto1$reference[toto1$forecastType=="Bias Correction 2"] = "ref"
toto1$reference[toto1$forecastType!="Bias Correction 2"] = "new"
# toto1 <- distinct(toto1)
# length(toto1)

unique(toto1$forecastType) # =="Bias Correction 2"

#step 1, aggregate dataet
agg <- c("forecastSetup", "forecastSystem", "forecastType", "locationID", "leadtimeValue", "scoreType", "reference")
# fifi <- summarySE(tot_system, "scoreValue", agg, na.rm = T)
toto2 <- summarySE(data = toto1, "scoreValue", agg, na.rm = T)

#missed the LT filter, got
# Warning message:
#   In xx[ref == "new", col]/xx[ref == "ref", col] :
#   longer object length is not a multiple of shorter object length
# toto2 <- filter(toto2, leadtimeValue %in% c(1,2,3,4,5,6))
#step 2, run thru new function

# for (loc in unique(toto2$locationID)) {
#   print(paste("loc:", loc))
#   loc.out <- skillScore(toto2[toto2$locationID==loc, ])
# }

toto3 <- skillScore(toto2)



#############################""
#
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

