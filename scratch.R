as.numeric(as.Date(fcst$Start_date))

ts.fcst <- ts(fcst, start = as.numeric(as.Date(fcst$Start_date)), end = as.numeric(as.Date(fcst$end_date)))

