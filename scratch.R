load("~/SMHI Data/fcst.RData")

# as.numeric(as.Date(fcst$Start_date))
# 
# ts.fcst <- ts(fcst, start = as.numeric(as.Date(fcst$Start_date)), end = as.numeric(as.Date(fcst$end_date)))
# by(fcst$rmse[,1:3], )

# 1 reduce data to one test
fifi <- fcst$rmse

# 2 list of basins
basins <- fcst$subids

# 3 rename columns
colnames(fifi) <- as.factor(c(1:6))
#cbind() the basin names to each years' months

#4 add basins to rows, repeating each 12
#4 add months to rows, repeating for columns
basins.expanded <- rep(basins, times = 1, each = 12)

fifi2 <- as.data.frame(fifi)
fifi2$locationID <- basins.expanded
fifi2$months <- rep(1:12, times=825)

library(reshape2)
library(dplyr)
fifi3 <- melt(fifi2, id.vars=c("locationID", "months"))

# colnames(fifi3)[3] <- "newName"
colnames(fifi4)[3] <- "leadtimeValues"
colnames(fifi4)[4] <- "scoreValues"
fifi4 <- arrange(fifi4, months, leadtimeValues, locationID)

# play with df for plots

fifi4 <- filter(fifi3, locationID == 8200208)
# fifi4 <- fifi4[order("locationID", "months")]
require(ggplot2)
ggplot(fifi4, aes(leadtimeValues, scoreValues))


ggplot(fifi4, aes(leadtimeValues, scoreValues)) +                    # basic graphical object
  geom_line(aes(months=1), colour="blue") +  # first layer
  geom_line(aes(months=2), colour="green")  # second layer
