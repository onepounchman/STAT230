#' multiplies two square matrices and a vector
#'
#' @param A a Matrix
#' @param B a Matrix
#' @param x a Vector
#' @param First a number deciding the order of multiplication
#' @return The product of two matrices and a vector
#' @examples
#' A<-matrix(rnorm(36),nrow=6)
#' B<-matrix(rnorm(36),nrow=6)
#' x<-runif(6)
#' product(A,B,x,1)
#' product(A,B,x,2)
product<-function(A,B,x,First=1){
  if(first==1){
    return(A%*%B%*%x)
  }
  else return(A%*%(B%*%x))
}
