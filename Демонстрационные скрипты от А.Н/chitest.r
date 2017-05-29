chisqtest=function(statval, intnum,lev){
  val=qchisq(1-lev,intnum-1) # пороговая константа
  res=c(0)
  if (statval>=val) {res=1}
  else {res=res}
  return(res)
}