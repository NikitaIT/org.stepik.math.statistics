diversim=function(samplen,simnum){
n=samplen
m=simnum
minrat=c(0,0,0)

delta=data.frame(c(1:m),c(1:m),c(1:m),c(1:m))
names(delta)=c("d1","d2","d3","minrat")
for(i in 1:m) 
{
x=rnorm(n,0,1)
mean=sum(x)/n
var=sum(x^2)/n-mean^2
var_ub=n/(n-1)*var
var_ef=(n-1)/(n+1)*var


delta$d1[i]=abs(var-1)
delta$d2[i]=abs(var_ub-1)
delta$d3[i]=abs(var_ef-1)

j=which.min(c(delta$d1[i],delta$d2[i],delta$d3[i]))
minrat[j]=minrat[j]+1
}
minrat=minrat/m
delta$minrat[1:3]=minrat
return (delta)
}