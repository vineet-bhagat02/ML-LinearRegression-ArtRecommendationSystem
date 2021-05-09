#Load library
library(cPCG)

setwd("C:/Users/vinee/PycharmProjects/pythonProject")
options(scipen = 999)

image.feat.lab<-read.csv('all_feat_lab_main.csv',header=TRUE, sep=",",)

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


#Loss Function
LossFn <- function(X,Y,M,th){
  # initialize
  J <- 0;
  #Calculate cost
  predicted_val <- X%*%th;    
  sqrErrors   <- (predicted_val - Y)^2; 
  J <- (1/m) * sum(sqrErrors)
  #  J <- (1/2)*(1/m) * sum(sqrErrors);
}


#Conjugate Gradient Descent Function
ConjugateDesc <- function(X, Y, th)
{
  #Modifying Loss fn (y-wx)^2 as:
  #1/2 A w^2 - B w + C
  # and using A,B,C values as below
  
  A <- 2 * (t(X) %*% X) # This is 2 X^2
  B <- 2 * (t(X)%*%Y)   # 2XY
  C <- (t(y) %*% y)     # Y^2
  
  th <- cgsolve(A, B, 1e-5, 100) # theta value
  return(th)
}
################################

########## CGD Method on Tree Labels #########################################################
x<-subset(img.txt.tree.labels, select = -c(Lable_Tree) ) #Excluding the Target label
#X <- cbind(1,x)
X <- as.matrix(x)
y <- subset(img.txt.tree.labels, select = c(Lable_Tree) ) #Target Label
y <- as.matrix(y)

Q = runif(ncol(X), 0, 1)
Q <- as.matrix(Q)

result <- ConjugateDesc(X,y,Q)

Q <- result


# Calculating Performance Measures for Tree Labels
y.pred <- X%*%Q
y.true <- y
#predicted_label <- y.pred
n <- nrow(img.txt.tree.labels)
p <- ncol(X) - 1
SSE <- sum((y.pred - y.true)^2)
SST <- sum((y.true - mean(y.true))^2)
R_square <- 1 - SSE / SST
RMSE = sqrt(SSE/n)
Adj_R2 = 1 - (1 - R_square)*(n-1)/(n- p - 1)


cat('Performance Measures for Tree Labels')
cat('Adj R_square : ', Adj_R2, '   R_square : ', R_square , '   RMSE : ', RMSE, "\n")



########## CGD Method on Mythical Labels #########################################################
x<-subset(img.txt.mythical.labels, select = -c(Lable_Mythical) ) #Excluding the Target label
#X <- cbind(1,x)
X <- as.matrix(x)
y <- subset(img.txt.mythical.labels, select = c(Lable_Mythical) ) #Target Label
y <- as.matrix(y)

Q = runif(ncol(X), 0, 1)
Q <- as.matrix(Q)

result <- ConjugateDesc(X,y,Q)

Q <- result


# Calculating Performance Measures for Mythical Labels
y.pred <- X%*%Q
y.true <- y
#predicted_label <- y.pred
n <- nrow(img.txt.mythical.labels)
p <- ncol(X) - 1
SSE <- sum((y.pred - y.true)^2)
SST <- sum((y.true - mean(y.true))^2)
R_square <- 1 - SSE / SST
RMSE = sqrt(SSE/n)
Adj_R2 = 1 - (1 - R_square)*(n-1)/(n- p - 1)


cat('Performance Measures for Mythical Labels')
cat('Adj R_square : ', Adj_R2, '   R_square : ', R_square , '   RMSE : ', RMSE, "\n")


########## CGD Method on Animal Labels #########################################################
x<-subset(img.txt.animal.labels, select = -c(Lable_Animal) ) #Excluding the Target label
#X <- cbind(1,x)
X <- as.matrix(x)
y <- subset(img.txt.animal.labels, select = c(Lable_Animal) ) #Target Label
y <- as.matrix(y)

Q = runif(ncol(X), 0, 1)
Q <- as.matrix(Q)

result <- ConjugateDesc(X,y,Q)

Q <- result


# Calculating Performance Measures for Animal Labels
y.pred <- X%*%Q
y.true <- y
#predicted_label <- y.pred
n <- nrow(img.txt.animal.labels)
p <- ncol(X) - 1
SSE <- sum((y.pred - y.true)^2)
SST <- sum((y.true - mean(y.true))^2)
R_square <- 1 - SSE / SST
RMSE = sqrt(SSE/n)
Adj_R2 = 1 - (1 - R_square)*(n-1)/(n- p - 1)



cat('Performance Measures for Animal Labels')
cat('Adj R_square : ', Adj_R2, '   R_square : ', R_square , '   RMSE : ', RMSE, "\n")