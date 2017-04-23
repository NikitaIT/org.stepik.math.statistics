smp=rnorm(5,0,1)
smp=sort(smp)
n=length(smp)
Fn=ecdf(smp)
eps=0.0000001
xx=seq(smp[1]-1,smp[1],length.out=20)
labels=c()
for (i in 1:n) {labels=c(labels,sprintf("X_%d",i))}


for( i in 1:(n-1)){
  x=seq(smp[i],smp[i+1],length.out=20)
  xx=c(xx,x)
}
xx=c(xx,seq(smp[n],smp[n]+1,length.out=20))
y=Fn(xx)

plot(xx,y,type="s",ylab=NA,xlab=NA,xlim=c(smp[1]-1-0.1,smp[n]+1+0.1),ylim=c(0,1),xaxt="n")
axis(1,at=smp,labels=labels,las=2)

par(new=TRUE)
plot(xx,pnorm(xx),type="l",ylab="F(x)",xlab="x",xlim=c(smp[1]-1-0.1,smp[n]+1+0.1),ylim=c(0,1),xaxt="n")

for (j in xx) {
  par(new=TRUE)
    plot(c(j,j),c(pnorm(j),Fn(j)),type="l",lty=3,ylab="F(x)",xlab="x",xlim=c(smp[1]-1-0.1,smp[n]+1+0.1),ylim=c(0,1),xaxt="n")
  par(new=TRUE)
    plot(c(j,j),c(pnorm(j),Fn(j-eps)),type="l",lty=3,ylab="F(x)",xlab="x",xlim=c(smp[1]-1-0.1,smp[n]+1+0.1),ylim=c(0,1),xaxt="n")
}
for (k in smp) {
  par(new=TRUE)
  plot(c(k,k),c(pnorm(k),Fn(k)),type="l",lty=1,lwd=10,ylab="F(x)",xlab="x",xlim=c(smp[1]-1-0.1,smp[n]+1+0.1),ylim=c(0,1),col=rgb(0,0,1,1/4),xaxt="n")
  par(new=TRUE)
  plot(c(k,k),c(pnorm(k),Fn(k-eps)),type="l",lty=1,lwd=10,ylab="F(x)",xlab="x",xlim=c(smp[1]-1-0.1,smp[n]+1+0.1),ylim=c(0,1),col=rgb(1,0,0,1/4),xaxt="n")
  }


