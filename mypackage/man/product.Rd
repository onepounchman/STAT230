% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/product.R
\name{product}
\alias{product}
\title{multiplies two square matrices and a vector}
\usage{
product(A, B, x, First = 1)
}
\arguments{
\item{A}{a Matrix}

\item{B}{a Matrix}

\item{x}{a Vector}

\item{First}{a number deciding the order of multiplication}
}
\value{
The product of two matrices and a vector
}
\description{
multiplies two square matrices and a vector
}
\examples{
A<-matrix(rnorm(36),nrow=6)
B<-matrix(rnorm(36),nrow=6)
x<-runif(6)
product(A,B,x,1)
product(A,B,x,2)
}
