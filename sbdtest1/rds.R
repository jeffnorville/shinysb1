# imported.data <- readRDS(inFile$datapath)
# C:\Users\Jeff Norville\Documents\R\shinysb1\sbdtest3upload\scoreupload

path <- "C:/Users/Jeff Norville/Documents/R/shinysb1/sbdtest3upload/scoreupload/example.RDS"
d <- readRDS(path)

score.type <- "mean.error"
score.matrix <- as.matrx(d$mean.error, nrow = XXXX, ncol = d$lead.times)
# oops, I made the matrix a vector, which is silly
# should be matrix of dims:
# lead times, locs * forecast.frequency ?


colnames(score.matrix) <- as.factor(1:d$lead.times)

loc.ids <- d$location.name

tm.range <- difftime(as.Date(d$end.date), as.Date(d$start.date), units = "days")


d$mean.error
