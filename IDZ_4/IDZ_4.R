# @project IDZ_4
# @author Nikita Fiodorov 
# @date 22.04.2017
# @link https://www.dropbox.com/sh/lnrmixhxjlvq72g/AABfBdX0y8r2f_BOAn3ffcI-a?dl=0
# @data 

#подготовка библиотек
list.of.packages <- c("nortest","rmarkdown","revealjs","dplyr","tidyr","xtable","ggvis","ggplot2", "Rcpp","fitdistrplus")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(dplyr)
library(tidyr)
library(xtable)
library(ggvis)
library(ggplot2)
library(fitdistrplus)

#подготовка функций
allProp = function(x){ data.frame(mean = mean(x),var = var(x),asm = sum((x-mean(x))^3)/length(x)/var(x)^(3/2),exc = sum((x-mean(x))^4)/length(x)/var(x)^2-3)}
rgenerate <- function(size,params,FUN=rnorm){
  sapply(size,function(x){do.call(FUN, c(params,x))})
}
#подготовка данных
set.seed(100)
norm_data=as.data.frame(read.csv("51_norm_-23.798434.csv",col.names = c("x")));
a = c(0.25, 0.1, 0.05, 0.01, 0.001)
epsilon = c(10, 0.1, 0.01)

paramN = list(mean = -23, sd = 5)
sizeN = list(x10 = 10,x100=100,x1000=1000,x10000=10000)

epsilon_for_bern = c(0.2, 0.1, 0.01)

##В sizeB бросках значение size с вероятностью prob 
paramB = list(size = 1, prob =  1/10)
sizeB = list(x100=100)

norm_data = c(norm_data,rgenerate(sizeN,paramN))
bern_data = rgenerate(sizeB,paramB,rbinom)
sum(bern_data)
#первичная оценка
sapply(norm_data,summary)
sapply(norm_data,allProp)

#обработка данных

pirCount = function(n, k) {
(k+1)*(n-k+1)
}

pir(4,0)
gg = c()
for(i in 0:100){
  for(j in 0:i){
    gg = c(gg,as.character(pirCount(i,j)))
  }
}
head(gg)
dir()
writeLines(gg,"res.txt",sep = ",")
pirCount(2,1)
pirCount(11,1)
pirCount(2,0)
pirCount(3,1)
pirCount(3,0)
pirCount(3,2)
pirCount(4,2)
f = function(n, k) {
  if(k<0||k>n) {0
    } else if(n==0) { 
    1
  }else {
    1+(f(n-1,k-1)+f(n-1,k))/2;
    }
}
f = function(n, k) {
  row = c(0);
  if(n<1){
    return(0)
  }
  row[2]=row[1]=2-1/(2^(2-1));
  if(n==1){
    return(row)
  }
  for(i in 3:(n+1)){
    temp = row;
    row[i]=row[1]=2-1/(2^(i-1));
    for(j in 2:(i-1)){
      row[j]=(temp[j-1]+temp[j])/2 +1
    }
  }
  row
}
f = function(n, k) {
  row = c(0);
  for(i in 1:(n+1)){
    temp = row;
    row[i]=row[1]=2-1/(2^(i-1));
    if(i>2){
      for(j in 2:(i-1)){
        row[j]=(temp[j-1]+temp[j])/2 +1
      }
    }
  }
  row
}
system.time(a <- f(322,156))
system.time(a <- f1(322,156))

Rprof(tmp <- tempfile())
f(322,156)[157]
example(f)
Rprof()
summaryRprof(tmp)

print("-");
print(l);print(r);
print("-");

f(0,0)
f(1,0)
f(2,0)
f(2,1)
f(3,2)
f(4,2)
f(5,2)
f(322,156)[157]
plot(f(322,156))
table(f(40,156))


5%%2 == 0

f = function(n, k) {
  row = c(0);
  if(n<1){
    return(0)
  }
  row[2]=row[1]=2-1/(2^(2-1));
  if(n==1){
    return(row[1])
  }
  for(i in 3:(n+1)){
    temp = row;
    row[1]=2-1/(2^(i-1));
    #генерация поколения
    for(j in 2:(ceiling(i/2))){
      row[j]=1+(temp[j-1]+temp[j])/2;
    }
    #четная строка длиннее
    if(i%%2 == 0){
      row[i/2+1]=row[i/2];
    }
  }
  #вывод
  if(k+1 > n/2){
    row[n - k+1]
  }else{
    row[k+1]
  }
}
322/2
f(322,156)
322/2
f(5422,161)[162]
f(0,0)
f1
f2
f2 = list(
  f(0,0),
  f(1,0),
  f(2,1),
  f(3,2),
  f(4,2),
  f(5,2),
  f(6,2))
f(3,2)
