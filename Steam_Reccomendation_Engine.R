library(reshape)
library(reshape2)
library(dplyr)
# Casting dataset to be recommendation engine friendly
df = read.csv("Ratio_CSV.csv")
df_cast = acast(df, user~game_name)
df_cast_dataframe = data.frame(df_cast)
df_sample = sample_n(df_cast_dataframe,10)
#names(df_cast_dataframe)

# Init
X = matrix(.1,nrow=ncol(df_sample),ncol=100)
Theta = matrix(.1,nrow=nrow(df_sample),ncol=100)
Y = df_sample
R = data.frame((Y!=0))

#Gradient for X
xGradient = function(X,Theta,Y,R,lambda){
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

#Cost function and gradients
CostFunction = function(X,Theta,Y,R,lambda){
  initCost = sum(R*(X%*%t(Theta)-Y))^2
  J = .5 *initCost+ 
    .5*lambda*(sum(Theta))^2+
    .5*lambda*(sum(X))^2
  xGrad = xGradient(X,Theta,Y,R,lambda)
  ThetaGrad = ThetaGradient(X,Theta,Y,R,lambda)
  Grad = rbind(xGrad,ThetaGrad)
  newList <- list("JList" = J, "GradList" = Grad)
  return(newList)
}

#run the functions
rm(df, df_cast, df_cast_dataframe,G)
CostGradList = CostFunction(X,Theta,Y,R,.5)
optim(X,Theta,Y,R,.5,fn = CostFunction)
dim(xGradient(X,Theta,Y,R,.5))
dim(ThetaGradient(X,Theta,Y,R,.5))