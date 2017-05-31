# @project IDZ_6
# @author Nikita Fiodorov 
# @date 22.04.2017
# @link https://www.dropbox.com/sh/lnrmixhxjlvq72g/AABfBdX0y8r2f_BOAn3ffcI-a?dl=0
# @data 

list.of.packages <- c("maxLik","rmarkdown","revealjs","dplyr","tidyr","xtable","ggvis","ggplot2", "Rcpp","fitdistrplus")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(dplyr)
library(tidyr)
library(xtable)
library(ggvis)
library(ggplot2)
library(fitdistrplus)
library(maxLik)
#выборка данных
dir()
setwd("../IDZ_6/")
#по 3 выборки из 5ти различных семейств распределений
goftest<<-as.data.frame(read.csv("goftest_34.csv"))

# (ОМП для мультиномиального)
# Основная гипотеза --- принадлежность ГС семейству Бета распределений
k=10 # Берем 10 интервалов, так как выборки будут порядка 1000.
d=2  # Число параметров бета распределения.
df=k-d-1 # Число степеней свободы для И.Р.
# Оформим задачу как простую функцию от выборки

allbind <- function(size,params,rFUN=rnorm){
  r = sapply(size,function(x){do.call(FUN, c(params,x))});
  return(t(as.matrix(sapply(r,allProp))))
}

check=function(sample,pFUN=pbeta,k=10,d=2){
  
  df=k-d-1
  
  n=length(sample)
  br=seq(min(sample),max(sample),length.out=(k+1)) # Задаем разбиение носителя [0,1] равноотстоящими точками
  obs=hist(sample,breaks=br,plot=F)$counts # Наблюдаемое число попаданий в интервалы
  # Ожидаемо число (или вероятности попаданий в интервалы) --- теперь, является функцией от параметра theta
  # Так и реализуем их:
  prob=function(theta){
    return (pFUN(br[2:(k+1)],theta[1],theta[2])-pFUN(br[1:k],theta[1],theta[2]))
  }
  # Статистика хи-квадрат выглядит так же, как и для простой гипотезы, только теперь это функция от theta.
  chi=function(theta){
    pb=prob(theta)
    ch=obs-n*pb
    ch=ch/sqrt(n*pb)
    ch=ch^2
    return(sum(ch))
  }
  nlm(chi,c(1,1))
  # Первый способ --- вычислить минимум по theta
  minest=nlm(chi,c(1,1)) # делается это с помощью функции nlm
  # Второй способ --- подставить вместо theta ОМП для мультиномиального распределения
  logmul=function(theta){
    lm=obs*log(prob(theta)) # Вычисляем логарифм функции правдоподобия мультиномиального распределения с точностью до постоянной,
    # не зависящей от theta
    return (lm)
  }
  
  # Теперь находим theta, максимизирующий logmul
  mulest=maxNR(logmul, start=c(1,1))$estimate
  # Значение второй статистики считается так:
  chi_mlemul=chi(mulest)
  
  # Вычисляем pvalue и записываем ответ
  p.value_min=1-pchisq(minest$minimum,df)
  p.value_mlemul=1-pchisq(chi_mlemul,df)
  
  res=list(minest$minimum,minest$estimate[1],minest$estimate[2],p.value_min, chi_mlemul, mulest[1],mulest[2], p.value_mlemul)
  names(res)=c("chi_minimum","min_theta_estimate_L","min_theta_estimate_R","p.value_min","chi_mlemul","mle_mul_theta_estimate_L","mle_mul_theta_estimate_R", "p.value_mlemul")
  return(as.data.frame(t(res)))
}


# Постмотрим на результат для конкретного примера
x=rbeta(1000,2,5)
res=check(x)
print(res)


rbind(check(sample = goftest$X1_cauchy,pFUN = pcauchy),
      check(sample = goftest$X2_cauchy,pFUN = pcauchy),
      check(sample = goftest$X3_cauchy,pFUN = pcauchy),
      check(sample = goftest$X1_norm,pFUN = pnorm),
      check(sample = goftest$X2_norm,pFUN = pnorm),
      check(sample = goftest$X3_norm,pFUN = pnorm),
      check(sample = goftest$X1_chisq,pFUN = pgamma),
      check(sample = goftest$X2_chisq,pFUN = pgamma),
      check(sample = goftest$X3_chisq,pFUN = pgamma)
      )
print(res1)
a = check(sample = goftest$X1_pois,pFUN = pnorm)
check(sample = goftest$X1_chisq,pFUN = pchisq)
check(sample = goftest$X1_nbinom_m.10,pFUN = pnbinom)
