# imported.data <- readRDS(inFile$datapath)
# C:\Users\Jeff Norville\Documents\R\shinysb1\sbdtest3upload\scoreupload
# saveRDS(d, "C:/Users/Jeff Norville/Documents/R/shinysb1/sbdtest3upload/scoreupload/example.RDS")

path <- "C:/Users/Jeff Norville/Documents/R/shinysb1/sbdtest3upload/scoreupload/example.RDS"
d <- readRDS(path)

# TODO
score.type <- "mean.error"

# this should only be necessary with CSV, text imports
devine.row.count <- length(d$mean.error) / d$lead.times

score.matrix <- matrix(d$mean.error, 
                         nrow = devine.row.count, 
                         ncol = d$lead.times,
                         byrow = TRUE)

colnames(score.matrix) <- as.factor(1:d$lead.times)

loc.ids <- d$location.name

tm.range <- difftime(as.Date(d$end.date), as.Date(d$start.date), units = "days")







d$mean.error
