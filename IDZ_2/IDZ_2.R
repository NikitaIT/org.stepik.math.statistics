
# @project IDZ_2
# @author Nikita Fiodorov 
# @date 12.02.2017
# @link https://www.dropbox.com/sh/lnrmixhxjlvq72g/AABfBdX0y8r2f_BOAn3ffcI-a?dl=0
# @data annual-diameter-of-skirt-at-hem-.csv

install.packages("fitdistrplus");
#	Для выборок объема 10, 100,1000, 10000 из стандартного нормального закона
#	f(x)=ln((1/pi)*( scale/ ( (x-location)^2+scale^2) );scale=5;location=3
#вычислить  следующие оценки дисперсии:  выборочную дисперсию, 
#несмещенную выборочную дисперсию,  эффективную выборочную дисперсию. 
vars<- function(x){sum((x-mean(x))^2)/(length(x))}
varef<-function(x){(length(x)+1)*var(x)/(length(x)) }
allProp <- function(x){ c(vars = vars(x),var = var(x),varef=varef(x))}

paramN = list(mean = 0, sd = 1)
sizeN = list(x10 = 10,x100=100,x1000=1000,x10000=10000)

allbind <- function(size,params,FUN=rnorm){
r = sapply(size,function(x){do.call(rnorm, c(params,x))});
return(t(as.matrix(sapply(r,allProp))))
}
varabs<- function(...){abs(allbind(sizeN,paramN)-1)}
all<- function(){(varabs()+varabs()+varabs()+varabs()+varabs()+
                    varabs()+varabs()+varabs()+varabs()+varabs())/10}
all()
#абсолютное значение отклонения оценки от истинного значения (=1).

all <- abs(allbind(sizeN,paramN)-1)
#при больших n , асимптотически эффективной оценкой дисперсии   
#является и выборочная дисперсия
#при больших n , состоятельной оценкой дисперсии  
#является и исправленная(не смещенная) выборочная дисперсия

#выборка данных
dir()
setwd("../IDZ_2/")
#равномерное распределение
unif<<-as.data.frame(read.csv("unif_2.csv"));
#Коши распределение
cauchy<<-as.data.frame(read.csv("cauchy_1.csv"));
#Неизвестное распределение
type<<-as.data.frame(read.csv("type1_1.csv"));

#Необходимо построить оценку параметров, использую метод максимального правдоподобия.

#maxLik sample START
install.packages("maxLik");
library(maxLik);
LL<-function(t){sum(dcauchy(cauchy$x,t[1],t[2],log=TRUE))}
LL(c(1,1))
ml<-maxNR(LL,start=c(0,1))
ml$estimate
#maxLik sample END

library(fitdistrplus);

checkHistSample = function(x,FUN,params,name){
hist(x,breaks=30,col="blue",freq=FALSE,main = name)
expected_sample = do.call(FUN, c(list(x =sort(x)),params))
lines(sort(x),expected_sample,col="red",lwd=2)
}

#fitdist(data, distr, method="mle", start,...)
unifVunif = mledist(data=unif$x, distr="unif", optim.method="default",
        lower=-Inf, upper=Inf,start = formals(unif$x))
cauchyVcauchy = mledist(data=cauchy$x, distr="cauchy", optim.method="default",
                       lower=-Inf, upper=Inf,start = formals(cauchy$x))
var_OMP = (-mean(cauchyVcauchy$hessian))^(-1)



par(mfrow=c(2,2))
hist(type$x,breaks = 2*length(type$x)^(1/3), freq = F, col = "lightblue");
hist(rnorm(n = 10^6, mean = 0, sd = 1),freq = F, col = "lightblue");
hist(rcauchy(n = 10^2,0,0.1),breaks = 50,freq = F, col = "lightblue");
hist(rt(n = 10^2,df=1),breaks = 50,freq = F, col = "lightblue");

#Коши или нормальное, т.к. тяжелые хвосты
typeVcauchy = mledist(data=type$x, distr="cauchy", optim.method="default",
        lower=-Inf, upper=Inf,start = formals(type$x))

checkHistSample(type$x,dcauchy,as.list(typeVcauchy$estimate),"typeVcauchy")

typeVnorm = mledist(data=type$x, distr="norm", optim.method="default",
        lower=-Inf, upper=Inf,start = formals(type$x))
checkHistSample(type$x,dnorm,as.list(typeVnorm$estimate),"typeVnorm")
