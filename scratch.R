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

# mistake ?? shoud have 6 lead times, 

fifi2 <- as.data.frame(fifi)
fifi2$locationID <- basins.expanded

fifi3 <- melt(fifi2, id.vars=c("locationID"))
# add dates


# gotta convert to df!
require(ggplot2)
ggplot(fifi, aes(fifi[1])) +                    # basic graphical object
  geom_line(aes(y=y1), colour="red") +  # first layer
  geom_line(aes(y=y2), colour="green")  # second layer
