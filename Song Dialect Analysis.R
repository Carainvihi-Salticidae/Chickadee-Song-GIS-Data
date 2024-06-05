

# clear the R workspace
rm(list = ls())

library(parallel)
library(vegan)
library(bioacoustics)
library(warbleR)
library(ggplot2)
library(Rraven)
library(cluster)
library(Rtsne)
library(randomForest)
library(MASS)
library(fossil)
library(pbapply)
library(adehabitatHR)
library(caret)
library(Sim.DiffProc)
library(data.table)
library(tidyverse)
library(tuneR)
library(seewave)

# set t working directory
curr.dir<- setwd("C:/Users/jenki129/OneDrive - purdue.edu/Songs - all fee notes/New Folder")



# Set sound analysis parameters for the warbleR package
warbleR_options(wav.path = "C:/Users/jenki129/OneDrive - purdue.edu/Songs - all fee notes/New Folder", 
                wl = 600, flim = c(1.5, 9.5), ovlp = 90, bp = c(1.5, 12.0), samp.rate=32000, parallel = parallel::detectCores() - 1)


# put the wav clips into an extended selection table, the format the warbleR package needs

est.file.name<-file.path(curr.dir,"Extended selection table fee new.RDS")

# get wave file info 
wi <- info_wavs()
summary(wi)
  View(wi)


# create extended selection table by reading in wav files
est <- selection_table(whole.recs = T, extended = T, confirm.extended = F)

#  save  extended selection table locally
save(est,file=est.file.name)

# Uncomment this line just load the extended selection table if you previously ran the steps above - handy because those steps can be slow!
# est <- readRDS(est.file.name)


# Create image files of spectrograms if you want them.
if (FALSE){
  specreator(est, propwidth = TRUE, parallel = 1,dest.path ="C:/Users/jenki129/OneDrive - purdue.edu/AI Practice Directory/Output" )
}


# Note that if the sound files are large or if there are lots of them this might be slow
# Measure spectral parameters
sp <- specan(est,parallel=1, bp=)
# Spectrogram cross-correlation
xc <- xcorr(est,bp=c(1,5))
# MDS for cross-correlation 
xc.mds <- cmdscale(1 - xc, k = 5)
# Translate MDS output into 5-D coordinates that we'll use as features
colnames(xc.mds) <- paste0("xc.dim.", 1:5)
# Dynamic time warping of frequency contours
dtw.dist <- dfDTW(est, pb=TRUE, parallel = 1)
# MDS on DTW distance 
dtw.mds <- cmdscale(dtw.dist, k = 5)
# Translate DTW MDS into 5-D coordinates we'll use as features
colnames(dtw.mds) <- paste0("dtw.dim.", 1:5)
# Get cepstral coefficients and descriptive statistics
cps.cf <- mfcc_stats(est)

# put all features together in a single matrix
# keep the file names of each sound (here this is just column 1)
prms <- data.frame(est[, c("sound.files")], sp[, -c(1:4)], xc.mds, dtw.mds, cps.cf[, -c(1:2)])

## save feature measurements so we can just load these later and skip feature extraction
write.csv(prms,file.path(curr.dir,"acoustic parameters fee.csv"),row.names = FALSE)
prms <- read.csv(file.path(curr.dir,"acoustic parameters fee.csv"), stringsAsFactors = FALSE)

# edit columns names for clarity
old.names <- names(prms)
to.change <-which(old.names %like% "sound.files")
new.names <- old.names
new.names[to.change] <- "sound.files"
colnames(prms) <- new.names


# do this if the above steps give errors. Change FALSE to TRUE to run this line
if (TRUE){
  acous.meas <- prms
}


acous.meas <- acous.meas[, colSums(is.na(acous.meas)) !=nrow(acous.meas)]





###################################################################
########=============== Making a Dendrogram ===============########
###################################################################



library(factoextra)
library(FactoMineR)
library(cluster)
library(mclust)




songdata <- read.csv('acoustic parameters fee.csv', sep=",", header=T, row.names = 1)
attach(songdata)


songdata <- songdata[, colSums(is.na(songdata)) !=nrow(songdata)]

#--------------------------------------------------------------------------
#getting a summary of the data, make sure things are listed as anticipated	
#--------------------------------------------------------------------------

head(songdata)				  
names(songdata)					
dim(songdata)		
str(songdata)


###############################################################################################################################
#
#       Agglomerative Hierarchical Clustering
#
###############################################################################################################################

forestdata1 <- subset(forestdata, select = c(X,Y))


diss.matrix<- dist(songdata, method = "euclidean")

m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(diss.matrix, method = x)$ac
}

# get agglomerative coefficient for each linkage method
purrr::map_dbl(m, ac)

library(cluster)

#ward provides the best clustering method (ac = 0.8307)

hc.result<- agnes(diss.matrix, method = "ward")
plot(hc.result)


summary(hc.result)


library(MASS)

write.matrix(diss.matrix, file="dissmatrixfee2forest.csv")

