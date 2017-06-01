# @project IDZ_8
# @author Nikita Fiodorov 
# @date 31.05.2017
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
setwd("../IDZ_8_malov/")

experimentResults = data.frame()
experimentResults = as.data.frame(factor(as.vector(read.table("X.txt",sep = " ",dec = "."),mode = "double"),labels=c("O","I","II","III","IV"),ordered = T));
experimentResults = cbind(experimentResults,as.vector(read.table("Y.txt",sep = " ",dec = "."),mode = "double"));
colnames(experimentResults) = c("factor","value")


ggplot(experimentResults, aes(x = factor, y = value,color=factor)) + 
  geom_boxplot()
#мы видим что медиана 1 фактора и 4 выше всех,
#что 4 фактор представлен 1 наблюдением
#самый большой разброс у 2 фактора
#наиб различие между 4 и 2 гр.

#гипотиза о равенстве мат ож
#в случае их равенства, общее среднее явл оценкой для a

#- Выборки случайно и взаимно независимы
#- Ген.сов имеет нормальный закон распределения
#- Ген. сов имеют равные дисперсии

#модель x_i_j = (мат ож i)+(влияние j факт на i парам)+(ошибка)
fit <- aov(value ~ factor, data=experimentResults)
summary(fit)
#factor Mean Sq - оценка дисп при нулевой гип.
#Residuals Mean Sq - точечная оценка дисп независимо от выполн гип
#мы можем сказать, что фактор не является значимым предиктором для значения
#попарное сравнение с поправкой тьюке по умолчанию 95% д.и.
#diff - различия в абс числах 
#lwr,upr - д.и
#p adj - статистическая значимость между группами
(tykyFitTest = TukeyHSD(fit,conf.level = 0.98))
plot(tykyFitTest)
(tykyFitTestFactor = data.frame(p = tykyFitTest$factor[,4],factor = rownames(tykyFitTest$factor)))
ggplot(tykyFitTestFactor,aes(x = factor, y = p))+
  geom_point()


ggplot(experimentResults, aes(x = factor, y = value)) + 
  geom_boxplot()
#,col=factor
ggplot(experimentResults, aes(x = as.numeric(row.names(experimentResults)), y = value,col=factor)) + 
  geom_point(aes(color = factor),size = 2)+
  geom_smooth()
ggplot(experimentResults, aes(x = as.numeric(row.names(experimentResults)), y = value)) + 
  geom_point(aes(color = factor),size = 2)+
  geom_smooth(method = "lm")+
  facet_grid(.~factor)

pd = position_dodge(0.1)
ggplot(experimentResults, aes(x = factor, y = value, color = factor, group = factor)) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.2, lwd = 0.8, position = pd)+  
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 5, position = pd, pch=15) +
  theme_bw()

kruskal.test(value ~ factor, data = experimentResults)
# Значение р оказывается совсем не близким к нулю. 
#Поэтому мы не можем отвергнуть нулевую гипотезу о одинаковом распределении.



contr.matrix <- matrix(c(-1, 0, 1, -1, 1, 0), ncol = 2)
rownames(contr.matrix) <- c("A", "B", "C")
contr.matrix

with(experimentResults, stripchart(value ~ factor, xlab = "value", ylab = "Условия"))

experimentResults$factor = factor(experimentResults$factor,ordered = T)
contrasts(experimentResults$factor)
experimentResults$factor = factor(experimentResults$factor,ordered = F)
contrasts(experimentResults$factor)
(contrasts(experimentResults$factor) <- contr.sum(n = 5))
(contrasts(experimentResults$factor) <- contr.helmert(n = 5))
# Изменим базовый уровень на 4:
experimentResults$factor <- relevel(experimentResults$factor, ref = "IV")
levels(experimentResults$factor)
fit_lm <- lm(value ~ factor, data = experimentResults)
summary(fit_lm)
par(mfrow=c(2, 2))
plot(fit_lm)
#Как видим, программа автоматически выбрала в качестве базового уровня группу наблюдений для 0
#Estimate - среднее в группе(коэф b в лин модели)
#В следующей строке приведена информация, отражающая, насколько препарат B эффективнее по сравнению с препаратом А: видим, что среднее количество выживших насекомых в группе B было несколько выше, чем в группе A (на 0.83), но это повышение не было статистически значимым (Pr(>|t|) = 0.604).

xtable(fm2) 
xtable(anova(fm2))


coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics
ggplot(experimentResults, aes(factor,value))+
  geom_point(size = 5)+
  geom_smooth(method = "lm")
