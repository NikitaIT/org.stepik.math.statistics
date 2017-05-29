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
  #а. тест на нормальность с известной дисперсией 
  norm_test <- function(X,possible_mean,sd,a_levels) {
    test = c();
    #предполагаемое среднее
    test$possible_mean = possible_mean
    #значение распределения
    test$statistic = sqrt(length(X))*(mean(X)-possible_mean)/sd;
    #пороговое значение
    test$сritical_value<-qnorm(1-a_levels/2)
    #принимаем или нет
    test$a_levels = a_levels
    test$success = !(abs(test$statistic)>test$сritical_value)
    #реально достигнутый уровень значимости
    test$p_value = min(2-2*pnorm(test$statistic),2*pnorm(test$statistic))
    test
  }
  #b. тест на нормальность с не известной дисперсией 
  t_test <- function(X,possible_mean,a_levels) {
    test = list();
    #предполагаемое среднее
    test$possible_mean = possible_mean
    #значение распределения
    test$statistic = sqrt(length(X))*(mean(X)-possible_mean)/sqrt(var(X));
    #пороговое значение
    test$сritical_value<-qt(1-a_levels/2,length(X)-1)
    #принимаем или нет
    test$a_levels = a_levels
    test$success = !(abs(test$statistic)>test$сritical_value)
    #реально достигнутый уровень значимости
    test$p_value = min(2-2*pt(test$statistic,length(X)-1),2*pt(test$statistic,length(X)-1))
    test
  }
  
#подготовка данных
  set.seed(100)
  norm_data=as.data.frame(read.csv("51_norm_-23.798434.csv",col.names = c("x")));
  a = c(0.25, 0.1, 0.05, 0.01, 0.001)
  epsilon = c(10, 0.1, 0.01, 0)
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
  sapply(norm_data,mean)

#обработка данных
  (t_tests_results = lapply(epsilon, 
                          function(eps) {
                            sapply(norm_data[-1],t_test,
                                   possible_mean = paramN$mean+eps,
                                   a_levels = (1-a))
                          }))

  (norm_test_results = lapply(epsilon, 
                            function(eps) {
                              sapply(norm_data[-1],norm_test,
                                     possible_mean = paramN$mean+eps,
                                     a_levels = (1-a),
                                     sd = paramN$sd)
                            }))
  
#проверка
  #стандартный тест Стьюдента 
  (data_ttest = sapply(norm_data[-1],t.test,mu = paramN$mean))
  slice(as.data.frame(norm_data_ttest), 3)


