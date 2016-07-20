# Method 2: applying different string matching methods
#osa Optimal string aligment, (restricted Damerau-Levenshtein distance).
#lv Levenshtein distance (as in R’s native adist).
#dl Full Damerau-Levenshtein distance.
#hamming Hamming distance (a and b must have same nr of characters).
#lcs Longest common substring distance.
#qgram q-gram distance.
#cosine cosine distance between q-gram profiles
#jaccard Jaccard distance between q-gram profiles
#jw Jaro, or Jaro-Winker distance.

#install.packages('stringdist')
library(stringdist)

source1.devices<-read.csv('source1.csv')
source2.devices<-read.csv('source2.csv')
# To make sure we are dealing with charts
source1.devices$name <- as.character(source1.devices$name)
source2.devices$name <- as.character(source2.devices$name)


distance.methods <- c('osa','lv','dl','hamming','lcs','qgram','cosine','jaccard','jw')
dist.methods <- list()
for(m in 1:length(distance.methods))
{
  dist.name.enh<-matrix(NA, ncol = length(source2.devices$name),nrow = length(source1.devices$name))
  for(i in 1:length(source2.devices$name)) {
    for(j in 1:length(source1.devices$name)) { 
      dist.name.enh[j,i]<-stringdist(tolower(source2.devices[i,]$name),tolower(source1.devices[j,]$name),method = distance.methods[m])      
      #adist.enhance(source2.devices[i,]$name,source1.devices[j,]$name)
    }  
  }
  dist.methods[[distance.methods[m]]]<-dist.name.enh
}

match.s1.s2.enh<-NULL
for(m in 1:length(dist.methods))
{
  
  dist.matrix<-as.matrix(dist.methods[[distance.methods[m]]])
  min.name.enh<-apply(dist.matrix, 1, base::min)
  for(i in 1:nrow(dist.matrix))
  {
    s2.i<-match(min.name.enh[i],dist.matrix[i,])
    s1.i<-i
    match.s1.s2.enh<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=source2.devices[s2.i,]$name, s1name=source1.devices[s1.i,]$name, adist=min.name.enh[i],method=distance.methods[m]),match.s1.s2.enh)
  }
}
# Let's have a look at the results
library(reshape2)
matched.names.matrix<-dcast(match.s1.s2.enh,s2.i+s1.i+s2name+s1name~method, value.var = "adist")
View(matched.names.matrix)


# ## this package isn't available for 3.3.1 ... leaving it here for education.
# # Method 1: using the native R adist
# source1.devices<-read.csv('source1.csv')
# source2.devices<-read.csv('source2.csv')
# # To make sure we are dealing with charts
# source1.devices$name <- as.character(source1.devices$name)
# source2.devices$name <- as.character(source2.devices$name)
# 
# # It creates a matrix with the Standard Levenshtein distance between the name fields of both sources
# dist.name<-adist(source1.devices$name,source2.devices$name, partial = TRUE, ignore.case = TRUE)
# 
# # We now take the pairs with the minimum distance
# min.name<-apply(dist.name, 1, min)
# 
# match.s1.s2<-NULL  
# for(i in 1:nrow(dist.name))
# {
#   s2.i<-match(min.name[i],dist.name[i,])
#   s1.i<-i
#   match.s1.s2<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=source2.devices[s2.i,]$name, s1name=source1.devices[s1.i,]$name, adist=min.name[i]),match.s1.s2)
# }
# # and we then can have a look at the results
# View(match.s1.s2)