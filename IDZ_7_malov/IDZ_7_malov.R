# @project IDZ_7
# @author Nikita Fiodorov 
# @date 12.02.2017
# @link https://www.dropbox.com/sh/lnrmixhxjlvq72g/AABfBdX0y8r2f_BOAn3ffcI-a?dl=0
# @data annual-diameter-of-skirt-at-hem-.csv

list.of.packages <- c("maxLik","rmarkdown","revealjs","dplyr","tidyr","xtable","ggvis","ggplot2", "Rcpp")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(dplyr)
library(tidyr)
library(xtable)
library(ggvis)
library(ggplot2)
library(maxLik)
dir()
setwd("../IDZ_7_malov/")
#init
  set.seed(100);
  #   выборочный эксцесс
  exc = function(x){
    sum((x-mean(x))^4)/length(x)/var(x)^2-3;
  }
  #   выборочную асимметрию
  asm = function(x){
    sum((x-mean(x))^3)/length(x)/var(x)^(3/2)
  }
  #   все характеристики
  allProp = function(x){ data.frame(mean = mean(x),var = var(x),median  = median(x),asm = asm(x),exc = exc(x))}
  
#a.
  #чтение
  a_1 = 0.02; a = 0.00; b = 5.79; lamda_0 = 4.00; lamda_1 = 6.00
  PuasDistr = as.vector(read.table("puas.txt",sep = " ",dec = "."),mode = "double");
  distributions = list(PuasDistr=PuasDistr);
  #вариационный ряд
  sort(PuasDistr)
  table (PuasDistr)
  #эмпирическую функцию распределения
  lapply(distributions,FUN = plot.ecdf,col.01line = "red",col="green")
  #гистограмму частот;
  hist(PuasDistr, 
       breaks = 20,
       freq = F, 
       col = "lightblue",
       xlab = "Диаметр юбки",
       ylab = "Плотность вероятности",
       main = "PuasDistr Гистограмма"
  );
#b.
  #все характеристики
  distributionsProp = t(sapply(distributions,allProp))
  #Вероятность попадания в интервал (percentiles for x)
  pPuasDistr <- ecdf(PuasDistr) # эмпир функц
  pPuasDistr # pPuasDistr<-function(x,t){z<-x[x<t]; length(z)/length(x)}
  pPuasDistr(PuasDistr)  # процентили
  pPuas = function(a,b){pPuasDistr(b) - pPuasDistr(a)}
  pPuas(a,b)
#c.
  LL<-function(t){sum(dpois(distributions$PuasDistr,t[1],log=TRUE))}
  ml<-maxNR(LL,start=c(1)) #максимум функции правдоподобия
  val<-ml$estimate; print (val) #оценка максимального правдоподобия
#d.
  PuasDistr__a_1<-qnorm (1-a_1/2)
  T<-array(dim=2)
  mean = as.numeric(distributionsProp[1])
  T[1]<-mean-PuasDistr__a_1*sqrt(mean/length(PuasDistr))
  T[2]<-mean+PuasDistr__a_1*sqrt(mean/length(PuasDistr))
  T #доверительный интервал 
#e.
  x = PuasDistr
  n<-length(x); r<-3
  a1<-c(-Inf, 5, 6); b1<-c(4, 5, Inf)
  border <-c(4, 5) #общий массив границ интервалов
  h<-hist(x,breaks=c(min(x),border,max(x)),plot=FALSE) 
  nu<-h$counts; print (nu) #частоты элементов
  p1<-array(dim = r)
  p1[1]<- ppois(border[1], lamda_0)
  p1[r] <- 1-ppois(border[r-1], lamda_0)
  p1[2:(r-1)]<-ppois(border[2:(r-1)],lamda_0)-ppois(border[1:(r-2)],lamda_0)
  print (p1)
  res <- array (dim = r)
  res [1:r] <- (nu[1:r] - n*p1[1:r])/sqrt(n*p1[1:r])
  print (res)
  res2 <- array (dim = r)
  res2 [1:r]<- (res[1:r])^2  
  print (res2)
  Xi2<-sum(res2)
  xal<-qchisq(1-a_1, r-1)
  Xi2>xal
  #находим наибольший уровень значимости, при котором нет оснований отвергнуть гипотезу:
  al2<-1-pchisq(Xi2,r-1); al2 	
#f.
  csq<-function (t){  #для X2
    p<-pnorm(b1,0,t) - pnorm (a1,0,t);
    f<-sum((nu-n*p)^2/(n*p));
    print (f)
  }
  
  X2<-nlm(csq,p=mean(x))
  xal1<-qchisq (1-a_1, r-2)
  X2$minimum<=xal1
  alpha2<-1-pchisq(X2$minimum,r-2)
  print (alpha2)
#g.
  c<-0
  alpha1<-0.02
  lambda0 = lamda_0
  alpha0<-1-ppois(c,lambda0*n)-dpois(c, 8*n)
  while (alpha0 > alpha1) 
  {
    c<-c+1;
    alpha0<-1-ppois(c,lambda0*n)-dpois(c, lambda0*n)
  }
  c
  p<-(alpha1-alpha0)/dpois(c,lambda0*n)
  p
  alpha0
  lche<- sum(x)
  lche>=c 
  ##################################################
  c<-0; lambda1 <-5
  alpha0<-ppois(c,lambda1*n)
  while(alpha0<alpha1){
    c<-c+1;
    alpha0<-ppois(c,lambda1*n)
  }
  c<-c-1
  c
  alpha0<-ppois(c,lambda1*n)
  p<-(alpha1-alpha0)/(dpois(c,lambda1*n))
  alpha0
  p
  lche<- sum(x)
  lche<=c   	
#геометрическое распределение
  #c.
  library(maxLik)
  LL<-function(t){sum(dgeom(x,t[1],log=TRUE))}
  ml<-maxNR(LL,start=c(1)) #максимум функции правдоподобия
  val<-ml$estimate; print (val) #оценка макс.правдоподобия
  #довер инт
  alpha<-0.02
  T<-array(dim=2)
  xal<-qnorm (1-alpha/2)
  T[1]<-mean-xal*sqrt((mean*(mean+1))/length(x)) #левая граница Д.И.
  T[2]<-mean+xal*sqrt((mean*(mean+1))/length(x)) #правая граница Д.И. 
  T
  ###################################################
  r<-3  						#количество интервалов
  a<-8				
  b<-array(dim=r-1)					#вектор границ
  b[1]<-4; b[2]<-5;			
  h<-hist(x,breaks=c(min(x),b,max(x)),plot=FALSE) #построение гистограммы
  p<-array(dim=3)					#вектор теоретических вероятностей
  p[1]<-pgeom(b[1],1/(a+1))			
  p[2]<-pgeom(b[2],1/(a+1))-pgeom(b[1],1/(a+1))
  p[3]<-1-pgeom(b[2],1/(a+1))	
  print (p)
  print(nu)						#получение вектора частот
  v10<-(nu-n*p)/sqrt((n*p))	
  print (v10)
  v1<-(nu-n*p)^2/(n*p)				#вектор слагаемых величины X2
  print (v1)
  X2<-sum(v1)					#вычисление величины X2
  print (X2)
  xa<-qchisq(1-alpha,2)				#вычисление квантиля  
  X2>xa						
  alpha2<-1-pchisq(X2,2)			#находим наибольший уровень значимости, при
  alpha2						#котором нет оснований отвергнуть гипотезу
  ###############################################################
  P<-function(a){  			
    p[1]<-pgeom(b[1],1/(a+1))
    i<-2
    while(i<r){
      p[i]<-pgeom(b[i],1/(a+1))-pgeom(b[i-1],1/(a+1));
      i<-i+1;
    }
    p[r]<-1-pgeom(b[r-1],1/(a+1))
    p;}
  X2<-function(a){g<-n*P(a);f<-(nu-g)^2/g;sum(f)} 
  XM<-nlm(X2,p=mean) #проводим  минимизацию, 
  xb<-qchisq(1-0.02,r-2)    #вычисляем квантиль	 		
  XM$minimum<xb				#  гипотезу принимаем на заданном уровне знач.
  alpha2<-1-pchisq(XM$minimum,r-2)	#наибольший уровень значимости, на котором 
  alpha2		

  
  
  
  
  
  
  
  #x<-scan ("input2.txt"); print (x)
  x<-c(-2.132, -2.055, -3.007, -0.132, -1.923, -2.490, 0.216, 1.419, -1.333, 1.463, 1.057, 0.204, 0.677, 2.485, -3.322, -0.357, -0.981, -4.702, -1.580, -1.529, 0.120, 0.155, 1.790, 1.115, 1.048, 0.331, 0.724, 0.882, 0.190, -2.078, 0.115, -5.353, -2.241, -2.579, 2.513, -0.641, 1.444, -2.345, -2.724, -2.699, -2.737, -2.666, -0.169, -0.243, -3.408, -2.715, -0.545, -4.892, 0.089, -5.338)
  print (x)
  sortVec<-sort(x); print (sortVec) #вариационный ряд
  y<-sort(x)
  table (x)
  #################################################################
  f<-function(x,t){z<-x[x<t]; length(z)/length(x)}
  xu<-unique (sort(x))
  yu<-0; for(i in 1:length(xu)) yu[i]<-f(x,xu[i]); yu[length(xu)+1]<-1
  z<-stepfun(xu,yu)
  plot(z,verticals=FALSE)
  ######################################
  hist (x, right = TRUE, freq = TRUE, col = "yellow")
  ######################################
  mean<-sum(x)/length(x); print (mean)
  var<-sum(x^2)/length(x)-mean^2; print (var)
  med<-sort(x)[trunc(length(x)*1/2+1)]; print (med)  #медиана - выборочная квантиль порядка 1/2
  #можно и так: med<-median (x); print (med) 
  asm<-sum((x-mean)^3)/length(x)/var^(3/2); print (asm)
  exc<-sum((x-mean)^4)/length(x)/var^2-3; print (exc)
  a<--2.20; b<--0.20
  p<-f(x,b)-f(x,a); print (p)
  ####################################
  al<-0.1
  n<-length(x)
  xal<-qnorm (1-al/2)
  T<-array(dim=2)
  T[1]<-1/mean-xal*(1/mean)/(sqrt(n))
  T[2]<-1/mean+xal*(1/mean)/(sqrt(n))
  T
  ################################### дальше др.
  a0<-5
  sig0<-2
  n<-50
  aa<--1
  sig<-2
  
  
  Fy<-ecdf(x)
  Fnorm<-pnorm(y,a0,sig0)
  Diff<-array(dim=50)
  for(i in 1:n){Diff[i]<-abs(Fy(y[i])-Fnorm[i])}
  D<-max(Diff)
  Dn<-D*sqrt(n)
  Dn
  ####
  alpha2<-0.1
  r<-5                                                                           #количество интервалов
  a<-5
  b<-array(dim=r-1)                                                         #вектор границ
  b[1]<--2.715; b[2]<--1.923; b[3]<--0.132; b[4]<-0.724;
  h<-hist(x,breaks=c(min(x),b,max(x)),plot=FALSE)                #построение гистограммы
  p[1]<-pnorm(b[1],a,sig0)
  i<-2
  while(i<=r-1){
    p[i]<-pnorm(b[i],a,sig0)-pnorm(b[i-1],a,sig0);
    i<-i+1;
  }
  p[r]<-1-pnorm(b[r-1],a,sig0)				#конец заполнения вектора
  yhu<-h$counts					#получение вектора частот
  v1<-(yhu-n*p)^2/(n*p)				#вектор слагаемых величины X2
  X2<-sum(v1)					#вычисление величины X2
  xa<-qchisq(1-alpha2,r-1)				#вычисление квантиля  
  X2<xa
  alpha3<-1-pchisq(X2,r-1)			#находим наибольший уровень значимости, при котором нет оснований отвергнуть гипотезу
  alpha3
  ######
  P<-function(aa,sig){
    p[1]<-pnorm(b[1],aa,sig)
    i<-2
    while(i<=r-1){
      p[i]<-pnorm(b[i],aa,sig)-pnorm(b[i-1],aa,sig);
      i<-i+1;
    }
    p[r]<-1-pnorm(b[r-1],aa,sig);p;}
  h<-hist(x,c(min(x),b,max(x)),plot=FALSE)			 #новая гистограмма
  yhu<-h$counts
  X2<-function(a,b){g<-n*P(a,b);f<-(yhu-g)^2/g;sum(f)}	 #и величина X2 зависит от параметра
  XM<-nlm(X2,mean(x),sqrt(var(x))) 	#проводим нелинейную минимизацию, отыскивая тем самым 
  yb<-qchisq(1-alpha2,r-3)    		#вычисляем квантиль 
  XM$minimum<yb				#  гипотезу принимаем на заданном уровне знач.
  alpha3<-1-pchisq(XM$minimum,r-3)	#наибольший уровень значимости, на котором 
  alpha3					#нет оснований отвергнуть гипотезу
  ###
  A<-0		
  A<-qnorm(alpha2,n*a0,sqrt(n)*sig0)
  sum(y)<A
  A
  ### Change
  a1<--1
  A<-0      
  A<-qnorm(1-alpha2,n*a1,sqrt(n)*sig0)
  A
  sum(y)<A
  #####
  T<-array(dim=2)
  T[1]<-med-qnorm(1-alpha2/2)*sum(abs(y-med))/n
  T[2]<-med+qnorm(1-alpha2/2)*sum(abs(y-med))/n
  T
  
  TT<-array(dim=2)
  TT[1]<-((sqrt(2)-qnorm(0.995))*sum(abs(y-med))/n)^2
  TT[2]<-((sqrt(2)+qnorm(0.995))*sum(abs(y-med))/n)^2
  TT
  ##
  Flapl<-function(x,a,sig){
    if (x>a) p<-1-exp(-sqrt(2)*(x-a)/sig)/2
    else p<-exp(sqrt(2)*(x-a)/sig)/2
    p;}
  Fy<-ecdf(x);
  Flaplvec<-array(dim=50);
  for( i in 1:n){Flaplvec[i]<-Flapl(y[i],5.5,0.5)} ###WTF
  Diff<-array(dim=50);
  for(i in 1:n){Diff[i]<-abs(Fy(y[i])-Flaplvec[i])}
  D<-max(Diff);
  Dn<-D*sqrt(n);
  Dn
  ###
  h<-hist(y,breaks=c(min(y),b,max(y)),plot=FALSE) 	#построение гистограммы
  p<-array(dim=5)			#вектор теоретических вероятностей
  p[1]<-Flapl(b[1],a,sig0)
  i<-2
  while(i<=r-1){
    p[i]<-Flapl(b[i],a,sig0)-Flapl(b[i-1],a,sig0);
    i<-i+1;
  }
  p[r]<-1-Flapl(b[r-1],a,sig0)
  yhu<-h$counts					#получение вектора частот
  v1<-(yhu-n*p)^2/(n*p)			#вектор слагаемых величины X2
  X2<-sum(v1)					#вычисление величины X2
  xa<-qchisq(1-alpha2,r-1)			#вычисление квантиля  
  X2<xa						#гипотеза опроверглась
  alpha3<-1-pchisq(X2,r-1		)#находим наибольший уровень значимости, при
  alpha3					#котором нет оснований отвергнуть гипотезу
  ####			#уровень значимости (очень мал)
  P<-function(aa,sig){
    p[1]<-Flapl(b[1],aa,sig)
    i<-2
    while(i<=r-1){
      p[i]<-Flapl(b[i],aa,sig)-Flapl(b[i-1],aa,sig);
      i<-i+1;
    }
    p[r]<-1-Flapl(b[r-1],aa,sig);p;}
  h<-hist(x,c(min(y),b,max(x)),plot=FALSE) 			#новая гистограмма
  yhu<-h$counts
  X2<-function(a,b){g<-n*P(a,b);f<-(yhu-g)^2/g;sum(f)} 	#и величина X2 зависит от параметра
  XM<-nlm(X2,mean(y),sqrt(var(y))) 		#проводим нелинейную минимизацию, отыскивая тем самым 
  alpha3<-1-pchisq(XM$minimum,r-3)		#наибольший уровень значимости, на котором 
  alpha3
  
  ligm<-(sqrt(2)/50*sum(abs(x+0.641)))
  print (ligm)
  ligm2<-4/(1/50*sum(x^2)- mean)
  print (ligm2)
  