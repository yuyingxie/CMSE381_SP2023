library(ggplot2)
library(GGally)
library(class)
library(MASS)
library(pROC)

wine = read.csv("Wine.csv")
wine$class <- as.factor(wine$class-1)
colnames(wine) <- c("y","x1","x2")
# plot to see the overall pattern
ggpairs(wine, ggplot2::aes(color = y))

# Split the data into training set and testing set
n <- dim(wine)[1]
set.seed(36) # to get the same order if you rerun - but you change this to your favorite number
ord <- sample(1:n) #shuffle 
test <- wine[ord[1:(n/2)], ]
train <- wine[ord[((n/2) + 1):n], ]


##########################################################
## For visualization you can use the following function###
##########################################################
ggplot(train, aes(x=x1, y=x2, color=y)) + geom_point()