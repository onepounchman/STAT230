#' Perform EM algorithm for ABO blood type example
#'
#' @param n data (nA,nAB,nB,nO) 
#' @param parameters allele frequency probabilities (pA, pB, and pO)
#' @return updated parameters
EM<-function(n,para){
  loop<-100
  eps<-1e-8
  m<-sum(n)
  nA<-n[1]
  nAB<-n[2]
  nB<-n[3]
  nO<-n[4]
  Delta<-rep(1,1,1)
  pA<-para[1]
  pB<-para[2]
  pO<-para[3]
  while(loop>0 & sum( abs(Delta) > eps ) >= 1 ){
    m_AA<-nA*pA^2/(pA^2+2*pA*pO)
    m_AO<-nA*2*pA*pO/(pA^2+2*pA*pO)
    m_BB<-nB*pB^2/(pB^2+2*pB*pO)
    m_BO<-nB*2*pB*pO/(pB^2+2*pB*pO)
    m_AB<-nAB
    m_OO<-nO
    pA<-(2*m_AA+m_AO+m_AB)/(2*m)
    pB<-(2*m_BB+m_BO+m_AB)/(2*m)
    pO<-(2*nO+m_AO+m_BO)/(2*m)
    Delta<-c(pA,pB,pO)-para
    para<-c(pA,pB,pO)
    loop<-loop-1
  }
  return(para)
}
