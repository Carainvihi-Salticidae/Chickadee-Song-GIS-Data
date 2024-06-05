
# clear the R workspace
rm(list = ls())



songdata <- c.data <- read.csv("PCA data.csv", header = T, row.names = 1)
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
#                              creating the PCA
#
###############################################################################################################################

#songdata.forpca <- songdata[,12:40]   			              	#using numerical columns, omitting field name and end date
my.pca <- prcomp(na.omit(songdata, center=T))     #running pca, omitting NA,  and centering

summary(my.pca)
str(my.pca)
screeplot(my.pca, bstick=T, col=colours()[519])

my.pca							
biplot(my.pca)			

sd <- my.pca$sdev
loadings <- my.pca$rotation
rownames(loadings) <- colnames(songdata)
scores <- my.pca$x

loadings
pca.cutoff<- sqrt(1/ncol(songdata)) # cutoff for 'important' loadings
pca.cutoff


library(factoextra)
fviz_pca_biplot(my.pca, repel = TRUE, select.var = list(contrib = 5))
fviz_pca_var(my.pca, col.var = "black")

detach(songdata)

my.lm <- lm(Song.dist.log ~ Can.dist.log, data=songdata)
summary(my.lm)

my.lm2 <- lm(Can.dist.log ~ Euc.dist.log, data=songdata)
summary(my.lm2)


cor.test(songdata$Can.dist.log, songdata$Euc.dist.log, data=songdata)


########===North===####


# clear the R workspace
rm(list = ls())


songdata <- c.data <- read.csv("PCA data north.csv", header = T, row.names = 1)
attach(songdata)

#songdata.forpca <- songdata[,12:40]   			              	#using numerical columns, omitting field name and end date
my.pca <- prcomp(na.omit(songdata, center=T))     #running pca, omitting NA,  and centering

summary(my.pca)
str(my.pca)
screeplot(my.pca, bstick=T, col=colours()[519])

my.pca							
biplot(my.pca)			

sd <- my.pca$sdev
loadings <- my.pca$rotation
rownames(loadings) <- colnames(songdata)
scores <- my.pca$x

loadings
pca.cutoff<- sqrt(1/ncol(songdata)) # cutoff for 'important' loadings
pca.cutoff


fviz_pca_biplot(my.pca, repel = TRUE, select.var = list(contrib = 5))
fviz_pca_var(my.pca, col.var = "black")

detach(songdata)

my.lm <- lm(Song.dist.log ~ Can.dist.log, data=songdata)
summary(my.lm)


my.lm2 <- lm(Can.dist.log ~ Euc.dist.log, data=songdata)
summary(my.lm2)

cor.test(songdata$Can.dist.log, songdata$Euc.dist.log, data=songdata)

########===South===####


# clear the R workspace
rm(list = ls())


songdata <- c.data <- read.csv("PCA data south.csv", header = T, row.names = 1)
attach(songdata)

#songdata.forpca <- songdata[,12:40]   			              	#using numerical columns, omitting field name and end date
my.pca <- prcomp(na.omit(songdata, center=T))     #running pca, omitting NA,  and centering

summary(my.pca)
str(my.pca)
screeplot(my.pca, bstick=T, col=colours()[519])

my.pca							
biplot(my.pca)			

sd <- my.pca$sdev
loadings <- my.pca$rotation
rownames(loadings) <- colnames(songdata)
scores <- my.pca$x

loadings
pca.cutoff<- sqrt(1/ncol(songdata)) # cutoff for 'important' loadings
pca.cutoff



fviz_pca_biplot(my.pca, repel = TRUE, select.var = list(contrib = 5))
fviz_pca_var(my.pca, col.var = "black")

detach(songdata)

my.lm <- lm(Song.dist.log ~ Can.dist.log, data=songdata)
summary(my.lm)

my.lm2 <- lm(Can.dist.log ~ Euc.dist.log, data=songdata)
summary(my.lm2)

cor.test(songdata$Can.dist.log, songdata$Euc.dist.log, data=songdata)

