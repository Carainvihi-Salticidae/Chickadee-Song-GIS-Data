
library(vegan)
library(geosphere)
library(scatterplot3d)

# clear the R workspace
rm(list = ls())

#########======= Whole Site =========#########

c.data <- read.csv("matrix.canopyriver.f20.u50 simplified.csv", header = T, row.names = 1)
c.matrix <- as.dist(c.data)
s.data <- read.csv("Fee Matrix.csv", header = T, row.names = 1)
s.matrix <- as.dist(s.data)
e.data <- read.csv("euc.matrix new.csv", header = T, row.names = 1)
e.matrix <- dist(e.data)



plot(e.matrix,s.matrix,pch=16,cex=0.5,col="black",bty="l",xlab="Spatial distance",ylab="Song dissimilarity") #plot

# We will test if there is a distance decay of similarity using a Mantel test. 
#Note that we will use Pearson's correlation coefficient and 9999 permutations in the test.

m.test1.1 <- mantel(s.matrix,e.matrix, method = "pearson", permutations = 9999, na.rm = TRUE)
m.test1.1



plot(c.matrix,s.matrix,pch=16,cex=0.5,col="black",bty="l",xlab="Habitat connectivity distance",ylab="Song dissimilarity")

# We will test if there is a distance decay of similarity using a Mantel test. 
#Note that we will use Pearson's correlation coefficient and 9999 permutations in the test.

m.test2.1 <- mantel(s.matrix,c.matrix, method = "pearson", permutations = 9999, na.rm = TRUE)
m.test2.1



plot(e.matrix,c.matrix,pch=16,cex=0.5,col="black",bty="l",xlab="Habitat connectivity distance",ylab="Song dissimilarity")

# We will test if there is a distance decay of similarity using a Mantel test. 
#Note that we will use Pearson's correlation coefficient and 9999 permutations in the test.

m.test3.1 <- mantel(c.matrix,e.matrix, method = "pearson", permutations = 9999, na.rm = TRUE)
m.test3.1


### Now, we are going to use a partial mantel test to test if communities have 
#more different song compositions as the habitat composition in the sites get 
#increasingly further. We will do that while removing any possible spatial 
#autocorrelation. So, the third distance matrix in the analysis will be the 
#spatial distance between sites.




scatterplot3d(x=e.matrix,y=c.matrix,z=s.matrix, angle = 50,
              main="3D Scatter Plot",
              xlab = "Spatial Distance",
              ylab = "Song dissimilarity",
              zlab = "Habitat dissimilarity")


m.test4.1 <- mantel.partial(s.matrix,c.matrix,e.matrix, method = "pearson", permutations = 9999, na.rm = TRUE)
m.test4.1



#########======= Ratios =========#########

r.data <- read.csv("diss.ratio2.csv", header = T, row.names = 1)
r.matrix <- as.dist(r.data)



plot(e.matrix,r.matrix,pch=16,cex=0.5,col="black",bty="l",xlab="Spatial distance",ylab="Song dissimilarity")

# We will test if there is a distance decay of similarity using a Mantel test. 
#Note that we will use Pearson's correlation coefficient and 9999 permutations in the test.

m.test.r1 <- mantel(r.matrix,e.matrix, method = "pearson", permutations = 9999, na.rm = TRUE)
m.test.r1



plot(c.matrix,r.matrix,pch=16,cex=0.5,col="black",bty="l",xlab="Habitat connectivity distance",ylab="Song dissimilarity")

# We will test if there is a distance decay of similarity using a Mantel test. 
#Note that we will use Pearson's correlation coefficient and 9999 permutations in the test.

m.test.r2 <- mantel(r.matrix,c.matrix, method = "pearson", permutations = 9999, na.rm = TRUE)
m.test.r2




### Now, we are going to use a partial mantel test to test if communities have 
#more different song compositions as the habitat composition in the sites get 
#increasingly further. We will do that while removing any possible spatial 
#autocorrelation. So, the third distance matrix in the analysis will be the 
#spatial distance between sites.




scatterplot3d(x=e.matrix,y=c.matrix,z=r.matrix, angle = 50,
              main="3D Scatter Plot",
              xlab = "Spatial Distance",
              ylab = "Song dissimilarity",
              zlab = "Habitat dissimilarity")


m.test.r3 <- mantel.partial(r.matrix,c.matrix,e.matrix, method = "pearson", permutations = 9999, na.rm = TRUE)
m.test.r3




#########======= Diverstiy =========#########

d.data <- read.csv("Song Div Diss new.csv", header = T, row.names = 1)
d.matrix <- as.dist(d.data)
cd.data <- read.csv("matrix.canopyriver.f20.u50 div.csv", header = T, row.names = 1)
cd.matrix <- as.dist(cd.data)
ed.data <- read.csv("euc.matrix div.csv", header = T, row.names = 1)
ed.matrix <- dist(ed.data)
d.data <- read.csv("Note Div Diss new.csv", header = T, row.names = 1)
d.matrix <- as.dist(d.data)
d.datan <- read.csv("Note Div Diss north.csv", header = T, row.names = 1)
d.matrix.n <- as.dist(d.datan)
d.datas <- read.csv("Note Div Diss south.csv", header = T, row.names = 1)
d.matrix.s <- as.dist(d.datas)



plot(ed.matrix,d.matrix,pch=16,cex=0.5,col="black",bty="l",xlab="Spatial distance",ylab="Song dissimilarity")

# We will test if there is a distance decay of similarity using a Mantel test. 
#Note that we will use Pearson's correlation coefficient and 9999 permutations in the test.

m.test.d1 <- mantel(d.matrix,ed.matrix, method = "pearson", permutations = 9999, na.rm = TRUE)
m.test.d1



plot(cd.matrix,d.matrix,pch=16,cex=0.5,col="black",bty="l",xlab="Habitat connectivity distance",ylab="Song dissimilarity")

# We will test if there is a distance decay of similarity using a Mantel test. 
#Note that we will use Pearson's correlation coefficient and 9999 permutations in the test.

m.test.d2 <- mantel(d.matrix,cd.matrix, method = "pearson", permutations = 9999, na.rm = TRUE)
m.test.d2



### Now, we are going to use a partial mantel test to test if communities have 
#more different song compositions as the habitat composition in the sites get 
#increasingly further. We will do that while removing any possible spatial 
#autocorrelation. So, the third distance matrix in the analysis will be the 
#spatial distance between sites.




scatterplot3d(x=ed.matrix,y=cd.matrix,z=d.matrix, angle = 50,
              main="3D Scatter Plot",
              xlab = "Spatial Distance",
              ylab = "Song dissimilarity",
              zlab = "Habitat dissimilarity")


m.test.d3 <- mantel.partial(d.matrix,cd.matrix,ed.matrix, method = "pearson", permutations = 9999, na.rm = TRUE)
m.test.d3


#Diversity north and south


m.test.d4 <- mantel(d.matrix.n,d.matrix.s, method = "pearson", permutations = 9999, na.rm = TRUE)
m.test.d4

#########======= North  =========#########


c.data.n1 <- read.csv("matrix.canopyriver.north.csv", header = T, row.names = 1)
c.matrix.n1 <- as.dist(c.data.n1)
s.data.n1 <- read.csv("Fee Matrix north.csv", header = T, row.names = 1)
s.matrix.n1 <- as.dist(s.data.n1)
e.data.n1 <- read.csv("euc.matrix.north.csv", header = T, row.names = 1)
e.matrix.n1 <- as.dist(e.data.n1)



plot(e.matrix.n1,s.matrix.n1,pch=16,cex=0.5,col="black",bty="l",xlab="Spatial distance",ylab="Song dissimilarity")

# We will test if there is a distance decay of similarity using a Mantel test. 
#Note that we will use Pearson's correlation coefficient and 9999 permutations in the test.

m.test1.2 <- mantel(s.matrix.n1,e.matrix.n1, method = "pearson", permutations = 9999, na.rm = TRUE)
m.test1.2
m.test1.1


plot(c.matrix.n1,s.matrix.n1,pch=16,cex=0.5,col="black",bty="l",xlab="Habitat connectivity distance",ylab="Song dissimilarity")

# We will test if there is a distance decay of similarity using a Mantel test. 
#Note that we will use Pearson's correlation coefficient and 9999 permutations in the test.

m.test2.2 <- mantel(s.matrix.n1,c.matrix.n1, method = "spearman", permutations = 9999, na.rm = TRUE)
m.test2.2
m.test2.1


plot(e.matrix.n1,c.matrix.n1,pch=16,cex=0.5,col="black",bty="l",xlab="Habitat connectivity distance",ylab="Song dissimilarity")

# We will test if there is a distance decay of similarity using a Mantel test. 
#Note that we will use Pearson's correlation coefficient and 9999 permutations in the test.

m.test3.2 <- mantel(c.matrix.n1,e.matrix.n1, method = "spearman", permutations = 9999, na.rm = TRUE)
m.test3.2
m.test3.1

### Now, we are going to use a partial mantel test to test if communities have 
#more different song compositions as the habitat composition in the sites get 
#increasingly further. We will do that while removing any possible spatial 
#autocorrelation. So, the third distance matrix in the analysis will be the 
#spatial distance between sites.

scatterplot3d(x=e.matrix.n1,y=c.matrix.n1,z=s.matrix.n1, angle = 50,
              main="3D Scatter Plot",
              xlab = "Spatial Distance",
              ylab = "Song dissimilarity",
              zlab = "Habitat dissimilarity")


m.test4.2 <- mantel.partial(s.matrix.n1,c.matrix.n1,e.matrix.n1, method = "spearman", permutations = 9999, na.rm = TRUE)
m.test4.2
m.test4.1


#########======= South =========#########

c.data.s1 <- read.csv("matrix.canopyriver.south.csv", header = T, row.names = 1)
c.matrix.s1 <- as.dist(c.data.s1)
s.data.s1 <- read.csv("Fee Matrix south.csv", header = T, row.names = 1)
s.matrix.s1 <- as.dist(s.data.s1)
e.data.s1 <- read.csv("euc.matrix.south.csv", header = T, row.names = 1)

e.matrix.s1 <- as.dist(e.data.s1)



plot(e.matrix.s1,s.matrix.s1,pch=16,cex=0.5,col="black",bty="l",xlab="Spatial distance",ylab="Song dissimilarity")

# We will test if there is a distance decay of similarity using a Mantel test. 
#Note that we will use Pearson's correlation coefficient and 9999 permutations in the test.

m.test1.3 <- mantel(s.matrix.s1,e.matrix.s1, method = "spearman", permutations = 9999, na.rm = TRUE)
m.test1.3
m.test1.1


plot(c.matrix.s1,s.matrix.s1,pch=16,cex=0.5,col="black",bty="l",xlab="Habitat connectivity distance",ylab="Song dissimilarity")

# We will test if there is a distance decay of similarity using a Mantel test. 
#Note that we will use Pearson's correlation coefficient and 9999 permutations in the test.

m.test2.3 <- mantel(s.matrix.s1,c.matrix.s1, method = "spearman", permutations = 9999, na.rm = TRUE)
m.test2.3
m.test2.1


plot(e.matrix.s1,c.matrix.s1,pch=16,cex=0.5,col="black",bty="l",xlab="Habitat connectivity distance",ylab="Song dissimilarity")

# We will test if there is a distance decay of similarity using a Mantel test. 
#Note that we will use Pearson's correlation coefficient and 9999 permutations in the test.

m.test3.3 <- mantel(c.matrix.s1,e.matrix.s1, method = "spearman", permutations = 9999, na.rm = TRUE)
m.test3.3
m.test3.1

### Now, we are going to use a partial mantel test to test if communities have 
#more different song compositions as the habitat composition in the sites get 
#increasingly further. We will do that while removing any possible spatial 
#autocorrelation. So, the third distance matrix in the analysis will be the 
#spatial distance between sites.

scatterplot3d(x=e.matrix.s1,y=c.matrix.s1,z=s.matrix.s1, angle = 50,
              main="3D Scatter Plot",
              xlab = "Spatial Distance",
              ylab = "Song dissimilarity",
              zlab = "Habitat dissimilarity")


m.test4.3 <- mantel.partial(s.matrix.s1,c.matrix.s1,e.matrix.s1, method = "spearman", permutations = 9999, na.rm = TRUE)
m.test4.3
m.test4.2
m.test4.1
