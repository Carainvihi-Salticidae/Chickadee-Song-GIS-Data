
library(ggplot2)


# clear the R workspace
rm(list = ls())


####==== Matrix Correlation ====####


matrix.data <- read.csv("Plot data.csv", header = T, row.names = 1)
head(matrix.data)

ggplot(data = matrix.data, aes(x = Can.dist, 
                           y = Song.dist)) + 
  geom_point(size = 2, shape = 15, color = "salmon") + 
  theme_classic() +
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab ("Canopy Relatedness") +
  ylab ("Song Relatedness")

