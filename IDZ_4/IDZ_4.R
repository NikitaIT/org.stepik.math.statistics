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
    tmp = data.frame(critical_value = qnorm(1-a_levels/2))
    #принимаем или нет
    tmp = cbind(tmp,a_levels)
    tmp = cbind(tmp,success = !(abs(test$statistic)>tmp$critical_value))
    test$levels = tmp
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
    tmp = data.frame(critical_value = qt(1-a_levels/2,length(X)-1))
    #принимаем или нет
    tmp = cbind(tmp,a_levels)
    tmp = cbind(tmp,success = !(abs(test$statistic)>tmp$critical_value))
    test$levels = tmp
    #реально достигнутый уровень значимости
    test$p_value = min(2-2*pt(test$statistic,length(X)-1),2*pt(test$statistic,length(X)-1))
    test
  }
  #4. тест бернули
  bern_test <- function(X,possible_prob,a_levels) {
    test = list();
    #предполагаемое среднее
    test$possible_prob = possible_prob
    #значение распределения
    test$statistic = length(X)*(mean(X)-possible_prob)/sqrt(length(X)*possible_prob*(1-possible_prob));
    #пороговое значение
    tmp = data.frame(critical_value = qnorm(1-a_levels/2))
    #принимаем или нет
    tmp = cbind(tmp,a_levels)
    tmp = cbind(tmp,success = !(abs(test$statistic)>tmp$critical_value))
    test$levels = tmp
    #реально достигнутый уровень значимости
    test$p_value = min(2-2*pnorm(test$statistic),2*pnorm(test$statistic))
    test
  }
  
#подготовка данных
  set.seed(100)
  norm_data=as.data.frame(read.csv("51_norm_-23.798434.csv",col.names = c("x")));
  paramFile = list(mean = -23.798434,sd = 1)#из назв.файла и sd = 1(всегда)
  a = c(0.25, 0.1, 0.05, 0.01, 0.001)
  epsilon = c(10, 0.1, 0.01, 0)
  paramN = list(mean = -23, sd = 5)#выбираются случ. знач.
  sizeN = list(x10 = 10,x100=100,x1000=1000,x10000=10000)
  
  epsilon_for_bern = c(0.2, 0.1, 0.01,0)
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
  eps=0
  #1. для сгенерированных распределений
  (t_tests_results = lapply(epsilon, 
                          function(eps) {
                            t(sapply(norm_data[-1],t_test,
                                   possible_mean = paramN$mean+eps,
                                   a_levels = (1-a)))
                          }))
  (norm_test_results = (lapply(epsilon, 
                            function(eps) {
                              t(sapply(norm_data[-1],norm_test,
                                     possible_mean = paramN$mean+eps,
                                     a_levels = (1-a),
                                     sd = paramN$sd))
                            })))
  tmp = rbind(norm_test_results[[1]],norm_test_results[[2]],norm_test_results[[3]],norm_test_results[[4]])
  tmp[,3]
  tmp = rbind(t_tests_results[[1]],t_tests_results[[2]],t_tests_results[[3]])
  tmp[,3]
  #2. для распределения из файла
  (t_test_from_file_results = sapply(norm_data[1],t_test,
         possible_mean = paramFile$mean,
         a_levels = (1-a)))
  t_test_from_file_results[3]
  (norm_test_from_file_results = sapply(norm_data[1],norm_test,
         possible_mean = paramFile$mean,
         a_levels = (1-a),
         sd = paramFile$sd))
  norm_test_from_file_results[3]
  #3. для распределения бернули
  (bern_test_results = (lapply(epsilon[-1], 
                               function(eps) {
                                 t(bern_test(bern_data,
                                             possible_prob = paramB$prob+eps,
                                             a_levels = (1-a)))
                               })))
  tmp = rbind(bern_test_results[[1]],bern_test_results[[2]],bern_test_results[[3]])
  tmp[,3]
  #3. для распределения бернули с другими уровнями
  (bern_test_results = (lapply(epsilon_for_bern, 
                               function(eps) {
                                 t(bern_test(bern_data,
                                             possible_prob = paramB$prob+eps,
                                             a_levels = (1-a)))
                               })))
  tmp = rbind(bern_test_results[[1]],bern_test_results[[2]],bern_test_results[[3]])
  tmp[,3]
#проверка
  #стандартный тест Стьюдента 
  (data_ttest = sapply(norm_data[-1],t.test,mu = paramN$mean))
  (data_ttest = sapply(norm_data[1],t.test,mu = paramFile$mean))
  slice(as.data.frame(norm_data_ttest), 3)
  
  
  bern_data[1]=1
  bern_data[2]=1
  bern_data[3]=1
  sum(bern_data)
  tmp =t(bern_test(bern_data,
              possible_prob = paramB$prob,
              a_levels = (1-a)))
  tmp[3]
  