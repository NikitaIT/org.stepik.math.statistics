library(animation)
oopt = ani.options(interval = 0.3, nmax = 20)

preal=1/2
prior=c(1,1)



x=seq(1,ani.options("nmax"),by=1)
yreal=rbinom(ani.options("nmax"),1,preal)


ypred=seq(0,0,length.out = ani.options("nmax"))



probPred=data.frame(seq(0,0,length.out = ani.options("nmax")),seq(0,0,length.out = ani.options("nmax")))
colnames(probPred)<-c("Head","Tail")

for (i in 1:ani.options("nmax")) {
  a=prior[1]
  b=prior[2]
  probPred$Head[i]=a/(a+b)
  probPred$Tail[i]=1-a/(a+b)
  if (probPred$Head[i]>1/2) ypred[i]=1
  else if (probPred$Head[i]<1/2) ypred[i]=0
       else ypred[i]=rbinom(1,1,probPred$Head[i])
  
  smp=yreal[i]
  a=a+smp
  b=b+1-smp
  prior=c(a,b)
}

for (i in 1:ani.options("nmax")) {
plot(x[1:i],yreal[1:i] ,type="b",lty=1, pch=16, main="Real and predicted trajectories", xlab="k", ylab="X_k", xlim=c(0,ani.options("nmax")+1),col=rgb(0,0,1,1/4),ylim=c(-0.1,1.1))
par(new=TRUE)
plot(x[1:i],ypred[1:i] ,type="b",lty=5, pch=16, main="Real and predicted trajectories", xlab="k", ylab="X_k",col=rgb(1,0,0,1/4),xlim=c(0,ani.options("nmax")+1),ylim=c(-0.1,1.1))
  ani.pause()
}

