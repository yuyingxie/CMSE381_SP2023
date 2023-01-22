
KNN_XYY = function(K, TrainX, TrainY, x){
  distX <- abs(TrainX - x)
  id    <- rank(distX) <= K
  Yhat  <- median(TrainY[id])
  return(Yhat)
} 

X <- seq(-2, 2, 0.1)
Result <- matrix(0, length(X), 2 )
Result[, 1] <- X
dat <- read.csv('Regression.csv')

for(i in 1:length(X)){
  Result[i, 2] <- KNN_XYY(K = 1, TrainX = dat[, 1], TrainY = dat[, 2], X[i])
}

plot(dat[, 1], dat[, 2])
lines(Result[, 1], Result[, 2], col = 'blue')


KNN_XYY = function(K, TrainX, TrainY, x){
  p <- dim(TrainX)[2]
  n <- dim(TrainX)[1]
  Dif <- TrainX - rep(1, n) %*% matrix(x, 1, p)
  distX <- sqrt(diag(Dif %*% t(Dif)))
  id <- rank(distX) <= K
  Yhat <- median(TrainY[id])
  
  
  return(Yhat)
}

dat = as.matrix(read.csv('wine.csv'))

KNN_XYY(K = 5, TrainX = dat[, 2:3], Train = dat[, 1], x = c(10, 4))

PredictY = function(p, h = 10, t = 15){
  Y = p^h *(1 - p)^t
  return(Y)
}

p = seq(0, 1, 0.01)


GetLikelihood = function(h = 36, t = 15, p = 0.1){
  Likelihood <- p^h * (1 - p)^t
  return(Likelihood)
}

p <- seq(0, 1, 0.01)
result = sapply(p, GetLikelihood, h = 36, t = 15)
plot(p, result)
id = which.max(result)
p[id]

id = which(result == sort(result, decreasing = TRUE)[5])
p[id]




