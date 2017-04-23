# �������� ��-������� ��� ������� �������� �������� � ���������� ��������������.
# ��� ����� ���������� ��������� � ������������. ������� ������ ��������� ������ �����������.

x=rpois(1000,2)
n=length(x)
# �������� labmda=2.
# �������� �����������:
hist(x,prob=T)
# ��� �����, 10 �����������, � ������� �������� �������� ������ 5, ����� ���������, �� ���������.
# �������� ������� ���������. ��������� ��������: 0,1,2...; ����������� ��.
supp=0:100 # ������ ��������� ��������. ���������� ����������� 100.

intervals=list()
probs=c()

# ��������� ������� ���������� �������� �� supp ����� �� �����, ������� � ������� val.cur,
# �� ��� ���, ���� � ����� n*����������� ������� �������� �� ������ ������ 5.
# �.�. ���������� ������� ������� ������ ��������, ������� � val.cur.
# ����� prob �������� ������� ����������� (������ ���� ��������, ���� ����� ������ ����� ���������� � prob=0)
grgen=function(val.cur,prob) {
y=val.cur
index=which(supp==y) # ��� ���������� ��� ������ ���������� �������� �� supp
invs=c()

  while (n*(prob)<5 & y %in% supp) { # ������� ����, ������������ �������� supp �� ���������� ����
     invs=c(invs,y)
    y=supp[(index+1)]
    index=index+1
    prob=prob+dpois(y,2)
    }
# �� ����� ������ ������
res=list(c(y),invs,prob)
names(res)=c("val.cur","invs","prob")
return(res)
# ...$val.cur --- �������, � �������� �������� ���������� ��������� ������
# ...$invs --- ������, � ������� �������� ������ 5
# ...$prob --- ����������� ������� � ��� ������
}


# �������� � ������ supp � ������ ������ ����� �� �����
j=supp[1]
while (j %in% supp) {
  rs=grgen(j,0)
  intervals=c(intervals,list(rs$invs))
  probs=c(probs,rs$prob)
  j=rs$val.cur
}

# �������� ��������. ��� ����������, ��� ��� �� ��������� ����� �� �����, � ����� ���������� �������� 
# � ��������� �������
print(intervals[(which(n*probs<5))])
# ��� �����, �c� ������. �� ���� �� ���������� �������� ��������� ���������� ������, ��� ���������� 
# ���������� � ����������, ������� �������� �������.

# ������� ������, ��������� ������ ������ ��������� ���� ����� ����������� �����.
# ������ ������ ��������� ���:
m=length(intervals)
print(intervals[1:(m-1)])
print(sprintf(">=%d",intervals[[m]][1]))

# �������� ��� ��� �����������
probs=unlist(lapply(intervals[1:(m-1)], function(x) dpois(x,2)))
probs=c(probs,1-sum(probs))
# ���. ���������� �������, ����������� ���������.
# ����� ����� ����������� ����� ���������, ��������� ��������. �.� hist ��� ������� �����.
headcnt=function(val){return(length(which(x==val)))}# ��������, ��� x --- �������
obs=unlist(lapply(intervals[1:(m-1)],headcnt))

tailcnt=function(val){return(length(which(x>=val)))}# �������� ��� ���������� ����������
obs=c(obs,tailcnt(intervals[[m]][1]))

# ������������ �����.
chitab=data.frame(obs=obs,exp=round(n*probs))
print(chitab)


