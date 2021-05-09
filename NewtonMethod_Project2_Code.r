#Load library
library(cPCG)

setwd("C:/Users/vinee/PycharmProjects/pythonProject")
options(scipen = 999)

image.feat.lab<-read.csv('all_feat_lab_main_new.csv',header=TRUE, sep=",",)

image.feat.lab<-image.feat.lab[-1]

str(image.feat.lab)
image.feat.lab<-image.feat.lab[complete.cases(image.feat.lab), ]

text_feat_lab<-read.csv('text_feat_lab.csv',header=TRUE, sep=",",)

img.text.feat.lab<-merge(x = image.feat.lab, y = text_feat_lab, by.x = "file",by.y = "ImageFileName", all.x = TRUE)

img.text.feat.lab<-img.text.feat.lab[complete.cases(img.text.feat.lab), ]

str(img.text.feat.lab)

img.txt.tree.labels <- subset(img.text.feat.lab, select=-c(Lable_Mythical,Lable_Animal,file,Label_Tree,Label_Mythical,Label_Animal,file.y))
img.txt.mythical.labels<- subset(img.text.feat.lab, select=-c(Lable_Tree,Lable_Animal,file,Label_Tree,Label_Mythical,Label_Animal,file.y))
img.txt.animal.labels <- subset(img.text.feat.lab, select=-c(Lable_Tree,Lable_Mythical,file,Label_Tree,Label_Mythical,Label_Animal,file.y))

img.txt.tree.labels <- sapply(img.txt.tree.labels, as.numeric)
img.txt.mythical.labels <- sapply(img.txt.mythical.labels, as.numeric)
img.txt.animal.labels <- sapply(img.txt.animal.labels, as.numeric)


#Loss Function
LossFn <- function(X,y,m,th){
  # initialize
  J <- 0;
  #Calculate cost
  predicted_val <- X%*%th;    
  sqrErrors   <- (predicted_val - y)^2; 
  J <- (1/m) * sum(sqrErrors)
}

# calculate Newton for multivariable
Newton_multi <- function(X, y, th, n_iters){
  m <- length(y)
  L_hist <- rep(0, n_iters)
  i=1
  j =1
  n = ncol(X)
  th_n <- th
  for(i in 1:n_iters)
  { 
    L_hist[i]  <- LossFn(X,y,m,th)
    #  -2*x((wx+b) - y) is gradient
    #  -2 * (t(X) %*% ((X%*%theta) - y)) is gradient
    
    #  2*x^2 is hessian
    #  2 * t(x)%*% x is hessian
    
    th_n = th - (inv(t(X)%*%X)%*%(t(X) %*% ((X%*%th) - y)))
    th <- th_n
    L_hist[i]  <- LossFn(X,y,m,th)
    cat('Iteration : ', i , '   Loss : ', L_hist[i], "\n")
    print(t(th))
  }
  list("theta" = th, "L_history" = L_hist) 
  #J_hist
  summary(L_hist)
  
  return(th)
}


########## Newton's Method on Tree Labels #########################################################
X <- subset(img.txt.tree.labels, select = -c(Lable_Tree) ) #Excluding the Target label
X <- as.matrix(X)
y <- subset(img.txt.tree.labels, select = c(Lable_Tree) ) #Target Label
y <- as.matrix(y)


# set theta randomly between 0 and 1
th = runif(ncol(X), 0, 1)
th <- as.matrix(th)
th
n_iters = 10


# Execute Newton Method
result <- Newton_multi(X,y,th,n_iters)
print(t(result))
th <- result
th

#  Measuring Performance
y_pre <- X%*%th
y_pre
true <- y
predicted <- y_pre
df <- nrow(img.txt.tree.labels)
p <- ncol(X) - 1
SSE <- sum((predicted - true)^2)
SST <- sum((true - mean(true))^2)
R_square <- 1 - SSE / SST
RMSE = sqrt(SSE/df)
AR_square = 1 - (1 - R_square)*(df-1)/(df- p - 1)

cat('Calculating Performance Measures for Tree Label')
cat('Adj R_square : ', AR_square, '   R_square : ', R_square , '   RMSE : ', RMSE, "\n")


########## Newton's Method on Mythical Labels #########################################################
X <- subset(img.txt.mythical.labels, select = -c(Lable_Mythical) ) #Excluding the Target label
X <- as.matrix(X)
y <- subset(img.txt.mythical.labels, select = c(Lable_Mythical) ) #Target Label
y <- as.matrix(y)


# set theta randomly between 0 and 1
th = runif(ncol(X), 0, 1)
th <- as.matrix(th)
th
n_iters = 10


# Execute Newton Method
result <- Newton_multi(X,y,th,n_iters)
print(t(result))
th <- result
th

#  Measuring Performance
y_pre <- X%*%th
y_pre
true <- y
predicted <- y_pre
df <- nrow(img.txt.mythical.labels)
p <- ncol(X) - 1
SSE <- sum((predicted - true)^2)
SST <- sum((true - mean(true))^2)
R_square <- 1 - SSE / SST
RMSE = sqrt(SSE/df)
AR_square = 1 - (1 - R_square)*(df-1)/(df- p - 1)

cat('Calculating Performance Measures for Mythical Label')
cat('Adj R_square : ', AR_square, '   R_square : ', R_square , '   RMSE : ', RMSE, "\n")


########## Newton's Method on Animal Labels #########################################################
X <- subset(img.txt.animal.labels, select = -c(Lable_Animal) ) #Excluding the Target label
X <- as.matrix(X)
y <- subset(img.txt.animal.labels, select = c(Lable_Animal) ) #Target Label
y <- as.matrix(y)


# set theta randomly between 0 and 1
th = runif(ncol(X), 0, 1)
th <- as.matrix(th)
th
n_iters = 10


# Execute Newton Method
result <- Newton_multi(X,y,th,n_iters)
print(t(result))
th <- result
th

#  Measuring Performance
y_pre <- X%*%th
y_pre
true <- y
predicted <- y_pre
df <- nrow(img.txt.animal.labels)
p <- ncol(X) - 1
SSE <- sum((predicted - true)^2)
SST <- sum((true - mean(true))^2)
R_square <- 1 - SSE / SST
RMSE = sqrt(SSE/df)
AR_square = 1 - (1 - R_square)*(df-1)/(df- p - 1)

cat('Calculating Performance Measures for Animal Label')
cat('Adj R_square : ', AR_square, '   R_square : ', R_square , '   RMSE : ', RMSE, "\n")
