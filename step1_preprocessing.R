# Preprocessing

### LOAD LIBRARIES - install with:
#install.packages(c("kohonen", "dummies", "ggplot2", "maptools", "sp", "reshape2", "rgeos"))
library(kohonen)
library(dummies)
library(ggplot2)
library(sp)
library(maptools)
library(reshape2)
library(rgeos)

# Colour palette definition
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')

### DATA PREPARATION

# Census data comes in counts of people per area. 
# To compare areas, we will convert a number of the
# stats collected into percentages. Without this, 
# the primary differentiator between the different 
# areas would be population size.

# Load the data into a data frame
# Get the map of these areas and filter for Dublin areas.

data_raw <- read.csv("c:/users/lenovo/desktop/a1/creditworthiness.csv")
x_testdata <- read.csv("c:/users/lenovo/desktop/a1/creditworthiness-test.csv")
x_traindata <- read.csv("c:/users/lenovo/desktop/a1/creditworthiness-train.csv")

idcol="functionary"
names(data_raw)[1] <- "functionary"

library(tidyverse)
str(data_raw)

#Plots histograms
plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

#Plots density plots for numeric variables
plotDen <- function(data_in, i){
  data=data.frame(x=data_in[[i]])
  p <- ggplot(data=data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
  
}            

library(e1071)
library(gridExtra)


doPlots(data_raw, fun = plotDen, ii = 7:11, ncol = 2)

data_raw

# The dataset:
data <- as.matrix(data_raw[, c(2,3,5,7,8,46)])
test <- as.matrix(x_testdata[, c(2,3,5,7,8)])
train<-as.matrix(x_traindata[, c(2,3,5,7,8,46)])

dataset <- data_raw[, c(2,3,5,7,8,46)]


# Default Heatmap
heatmap(train)
heatmap(data, Colv = NA, Rowv = NA, scale="column")
heatmap(data, scale="column", cexRow=1.5, labRow=paste("new_", rownames(data),sep=""), col= colorRampPalette(brewer.pal(8, "Blues"))(25))
head(data)
