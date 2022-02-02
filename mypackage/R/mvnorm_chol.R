#' Generate samples from multivariate normal distribution through Cholesky decomposition
#'
#' @param N a number, the number of samples
#' @param mu a nx1 mean vector 
#' @param Sigma a nxn covariance matrix
#' @return  N realizations from the multivariate normal distribution
#' @examples
#' N<-4
#' mu<-rnorm(6)
#' A<-matrix(rnorm(36),nrow=6)
#' Sigma<-t(A) %*% A
#' sample<-mvnorm_chol(n,mu,Sigma)
mvnorm_chol<-function(n,mu,Sigma){
  p<-ncol(Sigma)
  L<-t(chol(Sigma))
  z<-matrix(rnorm(n*p),nrow=n,ncol=p)
  x<-mu+z%*%L
  return(x)
}
