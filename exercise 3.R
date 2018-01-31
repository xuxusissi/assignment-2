#Xu Xu
#1
#(a)
tmpFn1 <- function(xVec){
  return(xVec^(1:length(xVec)))
}

tmpFn2 <- function(xVec){
  return(xVec^(1:length(xVec))/(1:length(xVec)))
}

#(b)
tmpFn3 <- function(x,n){
  return(1+sum(x^(1:n)/(1:n)))
}

#2
tmpFn <-function(xVec){
  n<-length(xVec)
  return((xVec[1:(n-2)]+xVec[2:(n-1)]+xVec[3:n])/3)
}

#3
tmpFn <- function(xVec){
  ifelse(xVec<0, xVec^2+2*xVec+3, ifelse(xVec<2, xVec+3, xVec^2+4*xVec-7))
}
tmp <- seq(-3, 3, len=1000)
plot(tmp, tmpFn(tmp), type="l")

#4
tmpFn <-function(mat){
  mat[mat%%2==1]<-2*mat[mat%%2==1]
  return(mat)
}

#5
tmpFn <- function(n, k){
  tmp <- diag(k, nr=n)
  tmp[abs(row(tmp)-col(tmp))==1]<-1
  return(tmp)
}

#6
quadrant <- function(alpha){
  return(1+(alpha%%360)%/%90)
}

#7
#(a)
weekday <- function(day, month, year){
  month<-month-2
  if(month<=0) {
    month<-month + 12
    year<-year-1
  }
  c<-year%/%100
  year<-year%%100
  tmp<-floor(2.6*month-0.2)+day+year+year%/% 4+c%/%4-2*c
  return(c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")[1+tmp%%7])
}

#(b)yes

#8
#(a)
testLoop <- function(n){
  xVec<-rep(NA, n-1)
  xVec[1]<-1
  xVec[2]<-2
  for(j in 3:(n-1))
      xVec[j]<-xVec[j-1]+2/xVec[j-1]
  return(xVec)
}

#(b)
testLoop2 <- function(yVec){
  n<-length(yVec)
  sum(exp(seq(along=yVec)))
}

#9
#(a)
quadmap <- function(start, rho, niter){
  xVec<-rep(NA,niter)
  xVec[1]<-start
  for(i in 1:(niter-1)){
    xVec[i+1]<-rho*xVec[i]*(1-xVec[i])
  }
  return(xVec)
}

#(b)
quadmap2 <- function(start, rho){
  x1<-start
  x2<-rho*x1*(1-x1)
  niter<-1
  while(abs(x1-x2)>=0.02){
    x1<-x2
    x2<-rho*x1*(1-x1)
    niter<-niter+1
  }
  return(niter)
}

#10
#(a)
tmpFn <- function(xVec){
  xc<-xVec-mean(xVec)
  denom<-sum(xc^2)
  n<-length(xVec)
  r1<-sum(xc[2:n]*xc[1:(n-1)])/denom
  r2<-sum(xc[3:n]*xc[1:(n-2)])/denom
  return(list(r1=r1, r2=r2))
}

#(b)
tmpFn <- function(x, k){
  xc<-x-mean(x)
  denom<-sum(xc^2)
  n<-length(x)
  tmpF<-function(j){sum(xc[(j+1):n]*xc[1:(n-j)])/denom}
  return(c(1, sapply(1:k, tmpF)))
}
