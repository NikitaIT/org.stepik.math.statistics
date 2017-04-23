# �������� ������ ���������� �������� ��-������� ��� ������� ��������.
library(maxLik) # ��� ���������� ����������� ������ ��� ������ ���������� ���������� 
# (��� ��� �����������������)
# �������� �������� --- �������������� �� ��������� ���� �������������
k=10 # ����� 10 ����������, ��� ��� ������� ����� ������� 1000.
d=2  # ����� ���������� ���� �������������.
df=k-d-1 # ����� �������� ������� ��� �.�.
# ������� ������ ��� ������� ������� �� �������
checkbeta=function(sample){
  
  n=length(sample)
  br=seq(0,1,length.out=(k+1)) # ������ ��������� �������� [0,1] ��������������� �������
  obs=hist(sample,breaks=br,plot=F)$counts # ����������� ����� ��������� � ���������
  # �������� ����� (��� ����������� ��������� � ���������) --- ������, �������� �������� �� ��������� theta
  # ��� � ��������� ��:
  prob=function(theta){
    return (pbeta(br[2:(k+1)],theta[1],theta[2])-pbeta(br[1:k],theta[1],theta[2]))
  }
  # ���������� ��-������� �������� ��� ��, ��� � ��� ������� ��������, ������ ������ ��� ������� �� theta.
  chi=function(theta){
    pb=prob(theta)
    ch=obs-n*pb
    ch=ch/sqrt(n*pb)
    ch=ch^2
    return(sum(ch))
  }
  # ������ ������ --- ��������� ������� �� theta
  minest=nlm(chi,c(1,1)) # �������� ��� � ������� ������� nlm
  # ������ ������ --- ���������� ������ theta ��� ��� ����������������� �������������
  logmul=function(theta){
    pr=prob(theta)
    lm=obs*log(pr) # ��������� �������� ������� ������������� ����������������� ������������� � ��������� �� ����������,
    # �� ��������� �� theta
    return (lm)
  }
  # ������ ������� theta, ��������������� logmul
  mulest=maxNR(logmul, start=c(1,1))$estimate
  # �������� ������ ���������� ��������� ���:
  chi_mlemul=chi(mulest)
  
  # ��������� pvalue � ���������� �����
  p.value_min=1-pchisq(minest$minimum,df)
  p.value_mlemul=1-pchisq(chi_mlemul,df)
  
  res=list(minest$minimum,minest$estimate,p.value_min, chi_mlemul, mulest, p.value_mlemul)
  names(res)=c("chi_minimum","min_theta_estimate","p.value_min","chi_mlemul","mle_mul_theta_estimate", "p.value_mlemul")
  return(res)
}


# ���������� �� ��������� ��� ����������� �������
x=rbeta(1000,2,5)
res=checkbeta(x)
print(res)

# ��������� ��� ����� ���� ������ � ���������� ��� �������� ����� ���������.
# pvalue �� ������, ��� ��� ����� ���������� ������� � ������������� ������������ ������� ����� �������,
# ������� 10000 --- 100000 (���������� ����� ���������, ��� ��� ��-������� ��� ������� ��������). 
# �� ����� ���������� ��� �������. �������� ������ � �������� ���������

estmin1=seq(1,1000,by=1)
estmin2=seq(1,1000,by=1)
estmlemul1=seq(1,1000,by=1)
estmlemul2=seq(1,1000,by=1)
chi_min=seq(1,1000,by=1)
chi_mlemul=seq(1,1000,by=1)

for (i in estmin1) {
  x=rbeta(1000,2,5)
  chires=checkbeta(x)
  estmin1[i]=chires$min_theta_estimate[1]
  estmin2[i]=chires$min_theta_estimate[2]
  estmlemul1[i]=chires$mle_mul_theta_estimate[1]
  estmlemul2[i]=chires$mle_mul_theta_estimate[2]
  chi_min[i]=chires$chi_minimum
  chi_mlemul[i]=chires$chi_mlemul
}

xx=seq(0,1,length.out=1000)

# ������ ��������, �� ������� ����������� ������� �� theta, ��� 1000 ���������

plot(xx,estmin1,type="l",col="red",xlim=c(0,1),ylim=c(1,6),xlab="sim",ylab="min_est",
     main="Plot of min_est for 1000 sims")
par(new=TRUE)
plot(xx,rep(2,each=1000),type="l",lwd=4,col="blue",xlim=c(0,1),ylim=c(1,6),xlab="sim",ylab="min_est",
     main="Plot of min_est for 1000 sims")
par(new=TRUE)
plot(xx,estmin2,type="l",col="red",xlim=c(0,1),ylim=c(1,6),xlab="sim",ylab="min_est",
     main="Plot of min_est for 1000 sims")
par(new=TRUE)
plot(xx,rep(5,each=1000),type="l",lwd=4,col="blue",xlim=c(0,1),ylim=c(1,6),xlab="sim",ylab="min_est"
     ,main="Plot of min_est for 1000 sims")


# ������ ��� ��� ����������������� ������������� ��� 1000 ���������

plot(xx,estmlemul1,type="l",col="green",xlim=c(0,1),ylim=c(1,6),xlab="sim",ylab="mle_mul",
     main="Plot of mle_mul for 1000 sims")
par(new=TRUE)
plot(xx,rep(2,each=1000),type="l",lwd=4,col="blue",xlim=c(0,1),ylim=c(1,6),xlab="sim",ylab="mle_mul",
     main="Plot of mle_mul for 1000 sims")
par(new=TRUE)
plot(xx,estmlemul2,type="l",col="green",xlim=c(0,1),ylim=c(1,6),xlab="sim",ylab="mle_mul",
     main="Plot of mle_mul for 1000 sims")
par(new=TRUE)
plot(xx,rep(5,each=1000),type="l",lwd=4,col="blue",xlim=c(0,1),ylim=c(1,6),xlab="sim",ylab="mle_mul"
     ,main="Plot of mle_mul for 1000 sims")

# �������� ����������� ��� chi_min � chi_mlemul � ������� � ��-�������(7)
hist(chi_min,breaks=11,prob=TRUE,ylim=c(0,0.15))
lines(density(chi_min), lty="dotted", col="darkgreen", lwd=2)
curve(dchisq(x,7),col="blue", lwd=2,add=TRUE)

hist(chi_mlemul,breaks=11,prob=TRUE,ylim=c(0,0.15))
lines(density(chi_mlemul), lty="dotted", col="darkgreen", lwd=2)
curve(dchisq(x,7),col="blue", lwd=2,add=TRUE)



# ����: ��� �����, � �������, ������� ����� ����� ��������� ���������� ���������� ���������� ���.


