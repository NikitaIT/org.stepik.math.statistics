# @project IDZ_1
# @author Nikita Fiodorov 
# @date 12.02.2017
# @link https://www.dropbox.com/sh/lnrmixhxjlvq72g/AABfBdX0y8r2f_BOAn3ffcI-a?dl=0
# @data annual-diameter-of-skirt-at-hem-.csv

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

#init
  AnnualDiameter<<-as.data.frame(read.csv("annual-diameter-of-skirt-at-hem-.csv",col.names = c("AnnualDiameter")));
  set.seed(100);
  #a.	сгенерировать выборку длины 1000 из данного распределения (стр. 19)
  n = 1000;
  propG = list(shape = 10, rate = 5.3);
  propN = list(mean = 2.3, sd = 0.3);
  propNB = list(size = 32,prob = 1/5);
  rG = rgamma(n = n, shape = 10, rate = 5.3);
  rN = rnorm(n = n, mean = 2.3, sd = 0.3);
  rNB = rnbinom(n = n, size = 32,prob = 1/5);
  distributions = list(rG=rG,rN=rN,rNB=rNB);

# Выборка с темпиратурой
plot.ecdf(AnnualDiameter$AnnualDiameter,col.01line = "red",col="green",main = "Empirical Plot AnnualDiameter");

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
lapply(distributions,FUN = plot.ecdf,col.01line = "red",col="green")

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
         xlab = "Диаметр юбки",
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
         xlab = "Диаметр юбки",
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
         xlab = "Диаметр юбки",
         ylab = "Плотность вероятности",
         main = "NB Гистограмма, с кривой плотности"
    );
    lines(density(rNB), col = "red", lwd = 2);
    lines(x = ranges$NB, y = densitys$NB, col = "green", lwd = 2);
  }
}
hist3();
#e.	вычислить выборочные характеристики(стр.20-22):  
distributionsProp = t(sapply(distributions,allProp))


expectedProp = rbind(rG =  c(mean = propG$shape/propG$rate, var= propG$shape/propG$rate^2,asm = 2/sqrt(propG$shape),exc = 6/propG$shape),
                     rN =  c(mean = propN$mean, var= propN$sd^2,asm = 0,exc = 0),
                     rNB =  c(mean = propNB$size*(1-propNB$prob)/propNB$prob, var= propNB$size*(1-propNB$prob)/propNB$prob^2,asm = (2-propNB$prob)/sqrt(propNB$size*(1-propNB$prob)),exc = 6/propNB$size - (propNB$prob)/(propNB$size*(1-propNB$prob))))
expectedProp
#f.	сравнить результаты пункта 'e' с реальными характеристиками распределения 

deltaProp = rbind(rG = allProp(rG)  - c(mean = propG$shape/propG$rate, var= propG$shape/propG$rate^2,asm = 2/sqrt(propG$shape),exc = 6/propG$shape),
                  rN = allProp(rN) - c(mean = propN$mean, var= propN$sd^2,asm = 0,exc = 0),
                  rNB = allProp(rNB)  - c(mean = propNB$size*(1-propNB$prob)/propNB$prob, var= propNB$size*(1-propNB$prob)/propNB$prob^2,asm = (2-propNB$prob)/sqrt(propNB$size*(1-propNB$prob)),exc = 6/propNB$size - (propNB$prob)/(propNB$size*(1-propNB$prob))))
deltaProp
write.csv(distributionsProp,file = "tProp.csv")

