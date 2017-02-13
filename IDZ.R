# @project IDZ_1
# @author Nikita Fiodorov 
# @date 12.02.2017
# @link https://www.dropbox.com/sh/lnrmixhxjlvq72g/AABfBdX0y8r2f_BOAn3ffcI-a?dl=0
# @data annual-diameter-of-skirt-at-hem-.csv

install.packages("ggplot2");
install.packages("stargazer");
library(ggplot2)
library(stargazer);

mustHave <- function(){
  #   выборочный эксцесс
  exc<<-function(x){
    sum((x-mean(x))^4)/length(x)/var(x)^2-3;
  }
  #   выборочную асимметрию
  asm <<- function(x){
    sum((x-mean(x))^3)/length(x)/var(x)^(3/2)
  }
  #   все характеристики
  allProp <<- function(x){ data.frame(mean = mean(x),var = var(x),asm = asm(x),exc = exc(x))}
  typeof(AnnualDiameter$AnnualDiameter)
  empiricalFun<<-function(x,t){z<-x[x<t]; length(z)/length(x)}
  empiricalPlot<<-function(x){
    xu<-unique(sort(x));
    yu<-0; 
    for(i in 1:length(xu)) yu[i]<-empiricalFun(x,xu[i]);
    yu[length(xu)+1]<-1
    z<-stepfun(xu,yu);
    plot.ecdf(z,col.01line = "red",col="green",main = "plotE");
  }
}
mustHave();

#init
init <- function(){
  AnnualDiameter<<-as.data.frame(read.csv("IDZ_1/annual-diameter-of-skirt-at-hem-.csv",col.names = c("AnnualDiameter")));

  #a.	сгенерировать выборку длины 1000 из данного распределения (стр. 19)
  n <- 1000;
  rG <<- rgamma(n = n, shape = 10, rate = 5.3);
  rN <<- rnorm(n = n, mean = 2.3, sd = 0.3);
  rNB <<- rnbinom(n = n, size = 32,prob = 1/5);
  #   значения функции распределения в точке x
  funP <<- list(G = {function(x){ pgamma(q = x, shape = 10, rate = 5.3);}},
               N = {function(x){ pnorm(q = x, mean = 2.3, sd = 0.3);}},
               NB = {function(x){ pnbinom(q = x, size = 32,prob = 1/5);}}
  );
}
init();

# Выборка с темпиратурой
empiricalPlot(AnnualDiameter$AnnualDiameter);
hist(AnnualDiameter$AnnualDiameter, 
     breaks = 20,
     freq = F, 
     col = "lightblue",
     xlab = "Темпиратура",
     ylab = "Плотность вероятности",
     main = "Гистограмма, с кривой плотности Темпиратуры"
);
AnnualDiameterProp <- allProp(AnnualDiameter$AnnualDiameter);

write.csv(AnnualDiameterProp,file = "AnnualDiameterProp.csv")
# конец выборки с темпиратурой

#b.	построить по данной выборке эмпирическую функцию распределения;
bildEmpiricalPlots <- function(){
    empiricalPlot(rG);
    empiricalPlot(rN);
    empiricalPlot(rNB);
}
bildEmpiricalPlots();


#c.	построить гистограмму частот;

#   Смотреть после пункта d.

#d.	сравнить гистограмму частот и реальную плотность данного распределения 
#  (вычисление значения плотности в точке 19 стр.)
#
ranges <- list(G = (((range(rG)[1]*100):(range(rG)[2]*100))/100),
               N = (((range(rN)[1]*100):(range(rN)[2]*100))/100),
               NB = range(rNB)[1]:range(rNB)[2]
              );
densitys <- list(G = dgamma(x = ranges$G,
                            shape = 10, rate = 5.3),
                 N = dnorm(x = ranges$N, 
                           mean = 2.3, sd = 0.3),
                 NB = dnbinom(x = ranges$NB, 
                              size = 32,prob = 1/5))

#   построение c&d Зеленый - ген.совок. Красный - выборка
hist3 <- function(){
  {
    hist(rG, 
         breaks = 20,
         freq = F, 
         col = "lightblue",
         xlab = "Темпиратура",
         ylab = "Плотность вероятности",
         main = "rG Гистограмма, с кривой плотности"
    );
    lines(density(rG), col = "red", lwd = 2);
    lines(x = ranges$G, y = densitys$G, col = "green", lwd = 2);
  }
  {
    hist(rN, 
         breaks = 20, 
         freq = F, 
         col = "lightblue",
         xlab = "Темпиратура",
         ylab = "Плотность вероятности",
         main = "rN Гистограмма, с кривой плотности"
    );
    lines(density(rN), col = "red", lwd = 2);
    lines(x = ranges$N, y = densitys$N, col = "green", lwd = 2);
  }
  {
    hist(rNB, 
         breaks = 20, 
         freq = F, 
         col = "lightblue",
         xlab = "Темпиратура",
         ylab = "Плотность вероятности",
         main = "NB Гистограмма, с кривой плотности"
    );
    lines(density(rNB), col = "red", lwd = 2);
    lines(x = ranges$NB, y = densitys$NB, col = "green", lwd = 2);
  }
}
hist3();
#e.	вычислить следующие выборочные характеристики(стр.20-22):  

#   выборочное среднее
means <- c(G = mean(rG), N = mean(rN), NB = mean(rNB));
#   выборочную дисперсию
vars <- c(G = var(rG), N = var(rN), NB = var(rNB));
#   выборочную асимметрию
asms <- c(G = asm(rG), N = asm(rN), NB = asm(rNB));
#   выборочный эксцесс
excs <- c(G = exc(rG), N = exc(rN), NB = exc(rNB));

#f.	сравнить результаты пункта 'e' с реальными характеристиками распределения 

meansReal <- c(G = mean(funP$G(ranges$G)), N = mean(funP$N(ranges$N)), NB = mean(funP$NB(ranges$N)));
varsReal <- c(G = var(funP$G(ranges$G)), N = var(funP$N(ranges$N)), NB = var(funP$NB(ranges$N)));
asmsReal <- c(G = asm(funP$G(ranges$G)), N = asm(funP$N(ranges$N)), NB = asm(funP$NB(ranges$N)));
excsReal <- c(G = exc(funP$G(ranges$G)), N = exc(funP$N(ranges$N)), NB = exc(funP$NB(ranges$N)));


t <- data.frame(meansReal,means,
           varsReal,vars,
           asmsReal,asms,
           excsReal,excs)
write.csv(t,file = "tProp.csv")
