library(reshape)
library(reshape2)
library(dplyr)
# Casting dataset to be recommendation engine friendly
df = read.csv("Ratio_CSV.csv")
df_cast = acast(df, user~game_name)
df_cast_dataframe = data.frame(df_cast)

# Pull only relevant games
columnmeans = colMeans(df_cast_dataframe)
filter_by_name = names(sort(columnmeans, decreasing = TRUE)[0:500])
df_filtered = df_cast_dataframe[,c(filter_by_name)]
#names(df_cast_dataframe)

# Init
numbercol = ncol(df_filtered)
numberrow = nrow(df_filtered)
X = matrix(.1,nrow=ncol(df_filtered),ncol=100)
Theta = matrix(.1,nrow=nrow(df_filtered),ncol=100)
Y = df_filtered
R = (Y!=0)*1

TotalGrad = function(X,Theta,Y,R,lambda,numbercol,numberrow){
  #Gradient for X
  XGradient = function(X,Theta,Y,R,lambda){
    XGrad = t(as.matrix(t(Theta))%*%as.matrix((R*(X%*%t(Theta)-Y)))) +
      lambda*X
    return(XGrad)
  }
  
  #Gradient for Theta
  ThetaGradient = function(X,Theta,Y,R,lambda){
    ThetaGrad = as.matrix(R*(X%*%t(Theta)-Y))%*%as.matrix(X) +
      lambda*Theta
    return(ThetaGrad)
  } 
  XGrad = XGradient(X,Theta,Y,R,lambda)
  ThetaGrad = ThetaGradient(X,Theta,Y,R,lambda)
  Grad = rbind(XGrad,ThetaGrad)
  return(Grad)
}

#Cost function and gradients
CostFunction = function(X,Theta,Y,R,lambda,numbercol,numberrow){
  X = matrix(data = X,nrow = numberrow, ncol = 100)
  initCost = sum(R*(X%*%t(Theta)-Y))^2
  J = .5 *initCost+ 
    .5*lambda*(sum(Theta))^2+
    .5*lambda*(sum(X))^2
  return(J)
}

#run the functions
Cost = CostFunction(X,Theta,Y,R,.5)
GradList = TotalGrad(X,Theta,Y,R,.5)
options(error=recover)
answer = optim(X,Theta,Y,R,.5,numbercol,numberrow,fn = CostFunction, gr = TotalGrad)
dim(xGradient(X,Theta,Y,R,.5))
dim(ThetaGradient(X,Theta,Y,R,.5))