# ����� ����� ������� ���������� a. �������� �������� �����������
kolmtest=function(D_value,lev) {
  # ��� ������� �������� ������ ��� ������� n>35
  C=sqrt(-1/2*log(lev/2))/sqrt(n)
  res=c(0)
  if (D_value<C) {res=0}
  else {res=1}
  return (res)
}