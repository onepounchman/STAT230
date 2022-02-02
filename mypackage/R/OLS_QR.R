#' Perform OLS estimation through QR decomposition
#'
#' @param X covariate matrix
#' @param Y response
#' @return  OLS estimates beta
OLS_QR<-function(X,Y){
  QR<-qr(X)
  Q<-qr.Q(QR)
  R<-qr.R(QR)
  b<-t(Q)%*%Y
  beta<-backsolve(R, b)
  return(beta)
}