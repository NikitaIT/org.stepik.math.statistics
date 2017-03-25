# @project IDZ_3
# @author Nikita Fiodorov 
# @date 12.02.2017
# @link https://www.dropbox.com/sh/lnrmixhxjlvq72g/AABfBdX0y8r2f_BOAn3ffcI-a?dl=0
# @data 

list.of.packages <- c("rmarkdown","revealjs","dplyr","tidyr","xtable","ggvis","ggplot2", "Rcpp")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(dplyr)
library(tidyr)
library(xtable)
library(ggvis)
library(ggplot2)

#init
n = 1000
normal_mean = -2
normal_sd = 0.25
a = c(0.25, 0.1, 0.05, 0.01)
p_values = 1 - a
dataset = data.frame(row.names = a)
##paste0("p = ", paste0(p_values, sep = "", collapse = ", p = "))
normal<-rnorm(n,normal_mean,sqrt(normal_sd))
M = mean(normal)

#check

# ggplot
ggplot(as.data.frame(list(no = normal)), aes(x = no,y = ..density..)) + 
  labs(x = "Значения",
       y = "Плотность",
       title = "Выборочное распределение") +
  geom_histogram(bins = 30,colour = "darkgreen", fill = "white") + 
  geom_density(alpha = 0.2,fill = "lightgreen",colour = "darkgreen")+
  # задаем отметки на оси x
  scale_x_continuous(breaks = as.vector(summary(normal)), 
                     labels = c(names(summary(normal))))+
  # задаем вертикальные линии
  geom_vline(xintercept = as.vector(summary(normal)), colour = "red")

# квантили для нормального и t-стьюдента
dataset$qnorm = sapply(1-a/2,qnorm)
dataset$qt = sapply(1-a/2,qt,n-1)# qt(p,df,...)
# отклонение границ u(a)*sigma/sqrt(n)
### По заданию
# a.  Считая  дисперсию(s) известной,  постройте доверительный интервал для мат ож(a).
dataset$intervalLeftA = M-dataset$qnorm*sqrt(normal_sd/n)
dataset$intervalRightA = M+dataset$qnorm*sqrt(normal_sd/n)
#b.	Считая  дисперсию(s) неизвестной,  постройте доверительный интервал для мат ож(a).
dataset$intervalLeftB = M-dataset$qt*sqrt(var(normal)/n);
dataset$intervalRightB = M+dataset$qt*sqrt(var(normal)/n);
# c.  Постройте доверительный интервал для дисперсии(s).
dataset$intervalLeftC = (n*normal_sd)/qchisq(1-a/2,n-1)
dataset$intervalRightC = (n*normal_sd)/qchisq(a/2,n-1)
#d.	Считая  дисперсию s известной,  постройте асимптотический 
#доверительный интервал для a на базе ОМП. Сравните с результатом пункта a).
dataset$intervalLeftD = M-sqrt(normal_sd/n)*dataset$qnorm;
dataset$intervalRightD = M + sqrt(normal_sd/n)*dataset$qnorm;
###e.	Считая  мат. ожид. а известным,  постройте асимптотический  доверительный интервал для s на базе ОМП. Сравните с результатом пункта c).
dataset$intervalLeftE = var(normal)-sqrt(2/n)*dataset$qnorm*var(normal);
dataset$intervalRightE = var(normal) + sqrt(2/n)*dataset$qnorm*var(normal);
s