toto3
toto3$leadtimeValue
toto3$scoreValue

str(toto3)

library(ggplot2)
ggplot(toto3, aes(x = as.factor(leadtimeValue), y = scoreValue, col = locationID))

ggplot()

plot(x = toto3$leadtimeValue, y = toto3$scoreValue)

qplot(toto3, leadtimeValue, scoreValue, col = locationID)


ggplot(toto3, )