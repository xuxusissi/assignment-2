#Xu Xu
#1 
#(a)
A<-matrix(c(1,5,-2,1,2,-1,3,6,-3),nr=3)
A%*%A%*%A
#(b)
A[,3]<-A[,2]+A[,3]

#2
B<-matrix(c(10,-10,10),nc=3,nr=15,b=TRUE)
t(B)%*%B

#3
matE<-matrix(c(0,0,0,0,0,0),nc=6,nr=6)
row(matE)
col(matE)
row(matE)-col(matE)
matE[abs(row(matE)-col(matE))==1]<-1
matE

#4
c <- 0:4
C <- outer(c,c,"+")
C

#5
#(a)
outer(0:4,0:4,"+")%%5
#(b)
outer(0:9,0:9,"+")%%10
#(c)
outer(0:8,0:8,"-")%%9

#6
y <- c(7,-1,-3,5,17)
A <- matrix(0,nr=5, nc=5)
A <- abs(col(A)-row(A))+1
x<-solve(A)%*%y
x

#7
set.seed(75)
aMat <- matrix( sample(10, size=60, replace=T), nr=6)
#(a)
apply(aMat, 1, function(x){sum(x>4)})
#(b)
which(apply(aMat,1,function(x){sum(x==7)==2}))
#(c)
aMatColSums <- colSums(aMat)
outer(aMatColSums,aMatColSums,"+")>75
which(outer(aMatColSums,aMatColSums,"+")>75, arr.ind=TRUE)

#8
#(a)
sum((1:20)^4)*sum(1/(4:8))
#(b)
sum((1:20)^4/(3+outer(1:20,1:5,"*")))
#(c)
sum(outer(1:10,1:10,function(i,j){(i>=j)*i^4/(3+i*j)}))