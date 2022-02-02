#' Perform OLS estimation through SVD decomposition
#'
#' @param X covariate matrix
#' @param Y response 
#' @return  OLS estimates beta
OLS_SVD<-function(X,Y){
  A<-svd(X)
  v<-A$v
  u<-A$u
  Sigma<-diag(1/(A$d))
  beta<-v%*%Sigma%*%(t(u)%*%Y)
  return(beta)
}