library(animation)
oopt = ani.options(interval = 0.2, nmax = 100)
n1=seq(1,10,by=1)
n2=seq(11,101,by=10)
n3=seq(101,1001,by=100)
n4=seq(1001,10001,by=1000)
n5=seq(10001,100001,by=10000)
num=c(n1,n2,n3,n4,n5)
k=length(num)
dlt=data.frame(c(1:k),c(1:k),c(1:k))
dumb=c(1:k)
names(dlt)=c("m1","m2","m3")
x=seq(0,k,by=1)
for (i in 1:k) {
  
  n=10+num[i]
  m=20
  delta=diversim(n,m)
  dlt$m1[i]=delta$minrat[1]
  dlt$m2[i]=delta$minrat[2]
  dlt$m3[i]=delta$minrat[3]
  dumb[i]=1/2
  
  plot(x[1:i],dlt$m1[1:i] ,type="b",lty=1, pch=16, main="Long complicated name", xlab="N", ylab="Minvar percent", xlim=c(0,k+1),col="red",ylim=c(0,1))
  par(new=TRUE)
  plot(x[1:i],dlt$m2[1:i] ,type="b",lty=1, pch=16, main="Long complicated name", xlab="N", ylab="Minvar percent", xlim=c(0,k+1),col="blue",ylim=c(0,1))
  par(new=TRUE)
  plot(x[1:i],dlt$m3[1:i] ,type="b",lty=1, pch=16, main="Long complicated name", xlab="N", ylab="Minvar percent", xlim=c(0,k+1),col="green",ylim=c(0,1))
  par(new=TRUE)
  plot(x[1:i],dumb[1:i] ,type="l",lty=1, lwd=2, main="Long complicated name", xlab="N", ylab="Minvar percent", xlim=c(0,k+1),col="black",ylim=c(0,1))
  ani.pause() 
}
