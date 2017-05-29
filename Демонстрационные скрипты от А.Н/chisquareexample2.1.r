# �������������� ������ ������ �����������
x=rbeta(1000,2,4)
n=length(x)
p=2
q=4

k=(log(n)/log(2)+1+2*n^(1/3))/2
k=round(k,digits=0) # ������ ����� ����������
{if (k<6) {k=6} else k=k}



# �������� ������� hist
h=hist(x,breaks=k,plot=FALSE)
# hist ���������� ����� ������� ����� �����������. �� ����, �������. ��� ���������� h$breaks. � ��� ���������� 
# ��������� �� ���������. � � h$counts ����� ����������� ��������� � ���.
br=h$breaks
obs=h$counts
# ������, ��������� ����� ���������� �������� ���������.
m=length(br)
exp=round(n*(pbeta(br[2:m],p,q)-pbeta(br[1:(m-1)],p,q)))



# ���������� ���������, ��� � ������ ������� �������� ��� ������� 5. ���� ���, �� ����� ��������� 
# ������ ���������� � ��������. ����� �������, ����� �������� ������� �������.
w=which(exp<5)
{while (length(w)>0) {
  br=br[-w]
  print(br)
  m=length(br)
  exp=round(n*(pbeta(br[2:m],p,q)-pbeta(br[1:(m-1)],p,q)))
  w=which(exp<5)
}}





# ������, ����� �������� ������ ��������� �����������, ����� ����������� ��������� �������
h=hist(x,breaks=br,plot=FALSE)
obs=h$counts

# �������� �� �� ��������, ��� � �������� 2.
pearson_residuals=(obs-exp)/sqrt(exp)
chi=sum(pearson_residuals^2)
alpha=0.1
testres=chisqtest(chi,length(exp),alpha)
answer=c("")
{if (testres==0) {answer=sprintf("������ �� ������������ �������� ��������")}
  else {answer=sprintf("�������� �������� ����������� �� ������ ���������� %f",alpha)}}
pvalue=1-pchisq(chi,length(exp)-1)
# ������� ����������
res=list(intervals_left=br[1:(m-1)],intervals_right=br[2:m],observed=obs, 
         expected=exp,pearson_residuals=pearson_residuals,
         pearson_value=chi,pvalue=pvalue,conflev=alpha,
         answer=answer)
print(res)


