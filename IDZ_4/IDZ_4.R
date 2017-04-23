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


