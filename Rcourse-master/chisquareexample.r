# ������ �������� ��-������� ��� �������� ������� �������� ��������.
# � ����� ��������� ��������� 2� �������� ��� � ������� (����� ��, ��������)
dt=read.table("2m.txt",sep="\n")
n=length(dt$V1)
# ����������� �� �������
rspin=table(dt)

# �������� �������� ����������� � ���, ��� ����� �� ������� ���������� ������������. ������� ��������� ����� ��������:
exp=rep(n/37,each=37)

#�������� ����������� �������
obs=as.vector(rspin)
chitab=data.frame(rspin,exp,(exp-obs),(exp-obs)/sqrt(exp),((exp-obs)/sqrt(exp))^2)
names(chitab)=c("number","obs","exp","exp-obs","pearson_residuals","chisqsummand")

# �������� ���������� ��-�������. ��� ����� ����� ���������� ������� � chitab
chi=sum(chitab$chisqsummand)

# �������� ������� ����������� ������� ����������, �.� ���������� ������� ��������, �� ������� �������� �������� �����������
pvalue=1-pchisq(chi,37-1)


# ���� �� ����� ���� ������� ����������� �������� chisq.test, ����������� � �������������� ������� rspin
test=chisq.test(rspin)
print(test$statistic)
print(test$p.value)



# ������������� pvalue. ���������� ������ ������� ����������, �������� 0.05. ���� �������� pvalue ������ ����� ������
# ����������, �� �������� ����������� �� ������ ���������� 0.05. � ����� ������ pvalue --- ����������� ����, 
# ��� ������� �� ���������� ������������ ������������� (�.�. ������� "�������") ������� ���� �����, ��� ���� ����,
# ��� ��������������� ���� �������. � ��� ��� ����������� 42%. �.�. ���������, ��� 42 �� 100 ������� �� ������������
# ������������� ����� ����� �� ���������� ��� ����.
# P.s: �� ����� ������� � �������� ����� �������� �� ���������.


# ��� ������� �������� ��������� ����������
pval=seq(1:10000)
for (i in 1:10000) {
  x=sample(1:37,1000,replace=TRUE)
  t=table(x)
  ts=chisq.test(t)
  pval[i]=ts$p.value
}

xx=seq(0,1,length.out=10000)
yy=rep(pvalue,each=10000)
plot(xx,pval,type="p",pch=20,col="red",xlim=c(0,1),ylim=c(0,1),xlab="Sim",ylab="pvalue")
par(new=TRUE)
plot(xx,yy,type="l",lty=1,lwd=6,col="blue",xlim=c(0,1),ylim=c(0,1),xlab="Sim",ylab="pvalue")


# ������� �� ��������, ��� ����� R ���������� ��������� ����� "���������". ���� � ���, ��� ����� �����������
# ����� �������, �� ��� ����, ��� �� pvalue ���������� ������������, ��������������� � ���, ��� �������
# �������� �����. (������� ����, �� ������� �� ���������). ��������!





# ���������� ������ ��������� �������� ���� ����� ��� �������� �������. ������� �� �� ������� ����� 1000 �, ����������,
# ������� ����������� ����������� pvalue (�������� �����������).
l=2000000/1000
ind=seq(1000,2000000,by=1000)
pval2=seq(1:l)

x=dt$V1[1:1000]
t=table(x)
ts=chisq.test(t)
pval2[1]=ts$p.value


for (j in 2:l) {
  x=dt$V1[ind[j-1]:ind[j]]
  t=table(x)
  ts=chisq.test(t)
  pval2[j]=ts$p.value
}

hist(pval2,breaks=10,freq=FALSE)
xxx=seq(0,1,length.out=1000)
yyy=rep(1,each=1000)
lines(xxx,yyy,col="red")
# ��� �����, pvalue ������������ ����������.




# ��������� ������ ��������. �������, ��� ����������� ��������� ����������� ����� = p_i. �������� ��������� ��� p_i.
p=floor(obs)/n
# �.�. ����� � �������� ������������ ������������� �������.
exp2=n*p
# ��������� ����� �������� pvalue ��������� ��� ������ ��������
oesq=(obs-exp2)/sqrt(exp2)
chi2=sum(oesq^2)
pval2=1-pchisq(chi2,37-1)
# �������� pvalue=1 (����������=0). � ����� ������� ������ ������� � ������������� ������ (��������� ����� �������)

