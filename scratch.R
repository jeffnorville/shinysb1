
as.numeric(as.Date(fcst$Start_date))

ts.fcst <- ts(fcst, start = as.numeric(as.Date(fcst$Start_date)), end = as.numeric(as.Date(fcst$end_date)))

by(fcst$rmse[,1:3], )


# 1 reduce data to one test

fifi <- fcst$rmse

# 2 list of basins
basins <- fcst$subids

# 3 rename columns
colnames(fifi) <- c(1:6)

#cbind() the basin names to each years' months

#4 add basins to rows, repeating each 12
#4 add months to rows, repeating for columns
rownames(fifi) <- c(1:12)


# gotta convert to df!
require(ggplot2)
ggplot(fifi, aes(fifi[1])) +                    # basic graphical object
  geom_line(aes(y=y1), colour="red") +  # first layer
  geom_line(aes(y=y2), colour="green")  # second layer
