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
set.seed(123)
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

ss=10
dataset$a=as.factor(a)
ggplot(data.frame(a=a,i = dataset$intervalRightA),aes(x = i,y = a,colour = 1), size = ss*1)+
  geom_point(data = data.frame(a=a,i = dataset$intervalRightB), colour = 2,size=ss*0.9)+
  geom_point(data = data.frame(a=a,i = dataset$intervalRightC), colour = 3,size=ss*0.8)+
  geom_point(data = data.frame(a=a,i = dataset$intervalRightD), colour = 4,size=ss*0.7)+
  geom_point(data = data.frame(a=a,i = dataset$intervalRightE), colour = 5,size=ss*0.6)+
  geom_point(data = data.frame(a=a,i = dataset$intervalLeftA), colour = 6,size=ss*0.5)+
  geom_point(data = data.frame(a=a,i = dataset$intervalLeftB), colour = 7,size=ss*0.4)+
  geom_point(data = data.frame(a=a,i = dataset$intervalLeftC), colour = 8,size=ss*0.3)+
  geom_point(data = data.frame(a=a,i = dataset$intervalLeftD), colour = 9,size=ss*0.2)+
  geom_point(data = data.frame(a=a,i = dataset$intervalLeftE), colour = 10,size=ss*0.1)+
  geom_vline(xintercept = as.vector(var(normal)), colour = "red")+
  geom_vline(xintercept = as.vector(M), colour = "green")+
  labs(x = "Уровень значимости. Зеленая линия - мат ож, Красная - дисперсия",
       y = "Величина распределения ",
       title = "Птички на ветке.")
dataset$a=NULL


#Неизвестное распределение
type<<-as.data.frame(read.csv("type1_1.csv"));

#построим асимптотический доверительный интервал для a на базе ОМП.
library(fitdistrplus);
#Коши или нормальное, т.к. тяжелые хвосты
typeVcauchy = mledist(data=type$x, distr="cauchy", optim.method="default",
                      lower=-Inf, upper=Inf,start = formals(type$x))

var_typeVcauchy_OMP = (mean(typeVcauchy$hessian))^(-1)
sd_typeVcauchy_OMP = sqrt(var_typeVcauchy_OMP)

typeVnorm = mledist(data=type$x, distr="norm", optim.method="default",
                    lower=-Inf, upper=Inf,start = formals(type$x))

var_typeVnorm_OMP = (mean(typeVnorm$hessian))^(-1)
sd_typeVnorm_OMP = sqrt(var_typeVnorm_OMP)

all_p_Viuw = data.frame(var_typeVnorm_OMP=var_typeVnorm_OMP,sd_typeVnorm_OMP=sd_typeVnorm_OMP,var_typeVcauchy_OMP=var_typeVcauchy_OMP,sd_typeVcauchy_OMP=sd_typeVcauchy_OMP)
all_p_Viuw
min_sd_OMP = min(all_p_Viuw$sd_typeVcauchy_OMP,all_p_Viuw$sd_typeVnorm_OMP)

sapply(a,qnorm)*min_sd_OMP
