
# @project IDZ_2
# @author Nikita Fiodorov 
# @date 12.02.2017
# @link https://www.dropbox.com/sh/lnrmixhxjlvq72g/AABfBdX0y8r2f_BOAn3ffcI-a?dl=0
# @data annual-diameter-of-skirt-at-hem-.csv


#	Для выборок объема 10, 100,1000, 10000 из стандартного нормального закона

allbind <- function(){
rN<- c()
rN$x10 <- rnorm(n = 10, mean = 0,sd = 1);
rN$x100 <- rnorm(n = 100, mean = 0,sd = 1);
rN$x1000 <- rnorm(n = 1000, mean = 0,sd = 1);
rN$x10000 <- rnorm(n = 10000, mean = 0,sd = 1);

#вычислить  следующие оценки дисперсии:  выборочную дисперсию, 
#несмещенную выборочную дисперсию,  эффективную выборочную дисперсию. 
vars<- function(x){sum((x-mean(x))^2)/(length(x))}
varef<-function(x){(length(x)+1)*var(x)/(length(x)) }
allProp <- function(x){ data.frame(vars = vars(x),var = var(x),varef=varef(x))}
return(rbind(x10 = allProp(rN$x10),
             x100 = allProp(rN$x100),
             x1000 = allProp(rN$x1000),
             x10000 = allProp(rN$x10000)));
}
varabs<- function(x){abs(allbind()-1)}
all<- function(){(varabs()+varabs()+varabs()+varabs()+varabs()+
                    varabs()+varabs()+varabs()+varabs()+varabs())/10}
all()
#абсолютное значение отклонения оценки от истинного значения (=1).

all <- abs(allbind()-1)
#при больших n , асимптотически эффективной оценкой дисперсии   
#является и выборочная дисперсия
#при больших n , состоятельной оценкой дисперсии  
#является и исправленная(не смещенная) выборочная дисперсия

#выборка данных
dir()
setwd("../IDZ_2/")
unif<<-as.data.frame(read.csv("unif_2.csv"));
cauchy<<-as.data.frame(read.csv("cauchy_1.csv"));
type<<-as.data.frame(read.csv("type1_1.csv"));
#Необходимо построить оценку параметров, использую метод максимального правдоподобия.
install.packages("fitdistrplus");
library(fitdistrplus);
#fitdist(data, distr, method="mle", start,...)
mledist(data=unif$x, distr="unif", optim.method="default",
        lower=-Inf, upper=Inf,start = formals(unif$x))
mledist(data=cauchy$x, distr="cauchy", optim.method="default",
        lower=-Inf, upper=Inf,start = formals(cauchy$x))


