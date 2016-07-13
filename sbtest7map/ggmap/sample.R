require("ggmap") # apparently needs rjson

coordinates <- data.frame(place=c("w", "x", "y", "z"), 
                          latitude=c(28.844692, 28.750925, 28.702134, 28.716547),
                          longitude=c(77.103305,  77.1963099, 77.2202666, 77.1704),
                          stringsAsFactors=FALSE)

a <- coordinates[coordinates$place=="y",c("place","latitude","longitude")]
get_constituency <- get_map(c(a$longitude, a$latitude))
ggmap(get_constituency) + 
  geom_point(aes(x = a$longitude, 
                 y = a$latitude, size=10), 
             alpha = .5, col="red") + 
  scale_size(range=c(3,5))
p=ggmap(get_constituency) + 
  geom_point(aes(x = a$longitude,
                 y = a$latitude, size=10), 
             alpha = .5, col="red") + 
  scale_size(range=c(3,5))

print(p, newpage = FALSE)