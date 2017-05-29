# Приведем пример реализации критерия хи-квадрат для сложной гипотезы.
library(maxLik) # Эта библиотека понадобится только для второй статистики отклонения 
# (ОМП для мультиномиального)
# Основная гипотеза --- принадлежность ГС семейству Бета распределений
k=10 # Берем 10 интервалов, так как выборки будут порядка 1000.
d=2  # Число параметров бета распределения.
df=k-d-1 # Число степеней свободы для И.Р.
# Оформим задачу как простую функцию от выборки
checkbeta=function(sample){
  
  n=length(sample)
  br=seq(0,1,length.out=(k+1)) # Задаем разбиение носителя [0,1] равноотстоящими точками
  obs=hist(sample,breaks=br,plot=F)$counts # Наблюдаемое число попаданий в интервалы
  # Ожидаемо число (или вероятности попаданий в интервалы) --- теперь, является функцией от параметра theta
  # Так и реализуем их:
  prob=function(theta){
    return (pbeta(br[2:(k+1)],theta[1],theta[2])-pbeta(br[1:k],theta[1],theta[2]))
  }
  # Статистика хи-квадрат выглядит так же, как и для простой гипотезы, только теперь это функция от theta.
  chi=function(theta){
    pb=prob(theta)
    ch=obs-n*pb
    ch=ch/sqrt(n*pb)
    ch=ch^2
    return(sum(ch))
  }
  # Первый способ --- вычислить минимум по theta
  minest=nlm(chi,c(1,1)) # делается это с помощью функции nlm
  # Второй способ --- подставить вместо theta ОМП для мультиномиального распределения
  logmul=function(theta){
    pr=prob(theta)
    lm=obs*log(pr) # Вычисляем логарифм функции правдоподобия мультиномиального распределения с точностью до постоянной,
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
  
  res=list(minest$minimum,minest$estimate,p.value_min, chi_mlemul, mulest, p.value_mlemul)
  names(res)=c("chi_minimum","min_theta_estimate","p.value_min","chi_mlemul","mle_mul_theta_estimate", "p.value_mlemul")
  return(res)
}


# Постмотрим на результат для конкретного примера
x=rbeta(1000,2,5)
res=checkbeta(x)
print(res)

# Посмотрим как ведут себя оценки и статистики для большого числа симуляций.
# pvalue не рисуем, так как чтобы подвердить правило о равномерности понадобиться большой объем выборок,
# порядка 10000 --- 100000 (сходимость здесь медленней, чем для хи-квадрат для простой гипотезы). 
# Не будем издеваться над железом. Нарисуем оценки и значения статистик

estmin1=seq(1,1000,by=1)
estmin2=seq(1,1000,by=1)
estmlemul1=seq(1,1000,by=1)
estmlemul2=seq(1,1000,by=1)
chi_min=seq(1,1000,by=1)
chi_mlemul=seq(1,1000,by=1)

for (i in estmin1) {
  x=rbeta(1000,2,5)
  chires=checkbeta(x)
  estmin1[i]=chires$min_theta_estimate[1]
  estmin2[i]=chires$min_theta_estimate[2]
  estmlemul1[i]=chires$mle_mul_theta_estimate[1]
  estmlemul2[i]=chires$mle_mul_theta_estimate[2]
  chi_min[i]=chires$chi_minimum
  chi_mlemul[i]=chires$chi_mlemul
}

xx=seq(0,1,length.out=1000)

# Рисуем значения, на которых достигается минимум по theta, для 1000 симуляций

plot(xx,estmin1,type="l",col="red",xlim=c(0,1),ylim=c(1,6),xlab="sim",ylab="min_est",
     main="Plot of min_est for 1000 sims")
par(new=TRUE)
plot(xx,rep(2,each=1000),type="l",lwd=4,col="blue",xlim=c(0,1),ylim=c(1,6),xlab="sim",ylab="min_est",
     main="Plot of min_est for 1000 sims")
par(new=TRUE)
plot(xx,estmin2,type="l",col="red",xlim=c(0,1),ylim=c(1,6),xlab="sim",ylab="min_est",
     main="Plot of min_est for 1000 sims")
par(new=TRUE)
plot(xx,rep(5,each=1000),type="l",lwd=4,col="blue",xlim=c(0,1),ylim=c(1,6),xlab="sim",ylab="min_est"
     ,main="Plot of min_est for 1000 sims")


# Рисуем ОМП для мультиномиального распределения для 1000 симуляций

plot(xx,estmlemul1,type="l",col="green",xlim=c(0,1),ylim=c(1,6),xlab="sim",ylab="mle_mul",
     main="Plot of mle_mul for 1000 sims")
par(new=TRUE)
plot(xx,rep(2,each=1000),type="l",lwd=4,col="blue",xlim=c(0,1),ylim=c(1,6),xlab="sim",ylab="mle_mul",
     main="Plot of mle_mul for 1000 sims")
par(new=TRUE)
plot(xx,estmlemul2,type="l",col="green",xlim=c(0,1),ylim=c(1,6),xlab="sim",ylab="mle_mul",
     main="Plot of mle_mul for 1000 sims")
par(new=TRUE)
plot(xx,rep(5,each=1000),type="l",lwd=4,col="blue",xlim=c(0,1),ylim=c(1,6),xlab="sim",ylab="mle_mul"
     ,main="Plot of mle_mul for 1000 sims")

# Построим гистограммы для chi_min и chi_mlemul и сравним с хи-квадрат(7)
hist(chi_min,breaks=11,prob=TRUE,ylim=c(0,0.15))
lines(density(chi_min), lty="dotted", col="darkgreen", lwd=2)
curve(dchisq(x,7),col="blue", lwd=2,add=TRUE)

hist(chi_mlemul,breaks=11,prob=TRUE,ylim=c(0,0.15))
lines(density(chi_mlemul), lty="dotted", col="darkgreen", lwd=2)
curve(dchisq(x,7),col="blue", lwd=2,add=TRUE)



# Итог: как видно, в среднем, разницы между двумя способами вычисления статистики отклонения нет.


