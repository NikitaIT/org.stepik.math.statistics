# ������ �������� ��-������� ��� ���������������� �������
# ���������� ������� � ������ ��������� ���������.
x=rbeta(1000,2,4)
n=length(x)
p=2
q=4



# ���������� ������� ����� ����������� � �� ���.
# ��� ����� ����������� ���������� �� �� �������, ��� � ��� ���������� ����������.
# ������, ������� ������, ��� � ������ ���������� ������ ������� (��������) ��� ������� 5�� ��������� �������.
# ����������, ����� ����� �������/����� ����������>5
# �������� ��������� �������. ����� F --- ������� ������������� ���������. �������� ������������������ ���������
# q_i ���, ����� F(q_i+1)-F(q_i)>5/n
k=(log(n)/log(2)+1+2*n^(1/3))/2
k=round(k,digits=0) # ������ ����� ����������
{if (k<6) {k=6} else k=k}





num=floor(n/k)
res=n-num*k
cval=1/num
qt=c(1:(k-1))
qt=(qt*num+res)/n
qt=qbeta(qt,p,q)
# �������� qt --- ����� ����������. �.� ��������� ����� ��� I_i=[qt[i],qt[i+1]]. ����� ������� � ����������.
# I_1=(-\infty,qt[1]), I_k=[qt[k],+\infty)
# �������� ������ ������ ��������� ����������� ��������� �  ��������� I_i ����� num/n (����� I_1, ��� ���� (num+res)/n)


# ������ �������� ����������� ����� ���������� � ���� ����������
# ������� ������� hist � ����� ����������
h=hist(x,breaks=c(0,qt,1),plot=FALSE)
# ��� ���������� ������ h$counts, ��� ��������� ����� ��������� �������, �������� � ������ ����������.
obs=h$counts


# ��������� ����� �� ��� �����
exp=c(1:k)
exp[1]=num+res
for (i in 2:k) {exp[i]=num}


# ������� ��� � ��������� �������
chitable=data.frame(obs,exp,obs-exp,(obs-exp)/sqrt(exp))
names(chitable)=c("obs","exp","diff","pearson_residuals")



# �������� �������� ���������� ��-�������
chi=sum((chitable$pearson_residuals)^2)

# �������� ��������
alpha=0.1
testres=chisqtest(chi,k-1,alpha)
answer=c("")
{if (testres==0) {answer=sprintf("������ �� ������������ �������� ��������")}
  else {answer=sprintf("�������� �������� ����������� �� ������ ���������� %f",alpha)}}

# �������� ������� ����������� ������� ����������
pvalue=1-pchisq(chi,k-1)

# ������� ��� ���������� ����������

res=list(intervals_left=c(-Inf,qt),intervals_right=c(qt,+Inf),observed=chitable$obs, 
           expected=chitable$exp,pearson_residuals=chitable$pearson_residuals,
         pearson_value=chi,pvalue=pvalue,conflev=alpha,
         answer=answer)
print(res)


# �������� ����������� ������� chisq.test
chisqres=chisq.test(chitable$obs,p=chitable$exp/n)
chisqres$statistic
chisqres$p.value
