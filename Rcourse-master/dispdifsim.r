library(animation)
oopt = ani.options(interval = 0.2, nmax = 100)
n1=seq(1,10,by=1)
n2=seq(11,101,by=10)
n3=seq(101,1001,by=100)
num=c(n1,n2,n3)
for (i in num) {
  
n=10+i
m=20
delta=diversim(n,m)
x=seq(1/m,1,by=1/m)

  plot(x,delta$d1 ,type="b",lty=1, pch=16, main="Sim", xlab="s", ylab="Delta", xlim=c(0,1),col="red",ylim=c(0,1))
  par(new=TRUE)
  plot(x,delta$d2 ,type="b",lty=1, pch=16, main="Sim", xlab="s", ylab="Delta", xlim=c(0,1),col="blue",ylim=c(0,1))
  par(new=TRUE)
  plot(x,delta$d3 ,type="b",lty=1, pch=16, main="Sim", xlab="s", ylab="Delta", xlim=c(0,1),col="green",ylim=c(0,1))
 
  ani.pause() 
}