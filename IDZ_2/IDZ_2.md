# IDZ 2
Nikita Fiodorov  
21.04.17  



<a name="Idz_2"></a>

[- -> Код {file}](https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_2/IDZ_2.R)

[- -> Данные Коши распределения {file}](https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_2/cauchy_1.csv)

[- -> Данные равномерного распределения {file}](https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_2/unif_2.csv)

[- -> Данные искомого распределения{file}](https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_2/type1_1.csv)

[- -> Задание {file}](https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_1/%D0%97%D0%B0%D0%B4%D0%B0%D0%BD%D0%B8%D0%B5%20%D0%BE%D1%82%209.2.2017.docx)

### Данные

Даны следующие распределения и параметры:

Тип Распределения			      | Параметры
----------------------------|--------------
Коши						            | a in R, b > 0 
Равномерное				        	| a ≤ b
Искомое					 	          | ??


```
##              unif            cauchy            type
## 1 Min.   :-3.55   Min.   :-508660   Min.   :-5.17  
## 2 1st Qu.:-1.70   1st Qu.:     -1   1st Qu.:-0.65  
## 3 Median : 0.24   Median :      1   Median :-0.01  
## 4 Mean   : 0.24   Mean   :   -101   Mean   :-0.05  
## 5 3rd Qu.: 2.14   3rd Qu.:      3   3rd Qu.: 0.60  
## 6 Max.   : 4.10   Max.   :   1838   Max.   : 4.58
```

### Выполнение работы

#### Выборочная дисперсия, несмещенная выборочная дисперсия, эффективная выборочная дисперсия.

Пусть $X_{1},\ldots ,X_{n},\ldots$  — выборка из распределения вероятности. Тогда

- выборочная дисперсия — это случайная величина $S_{n}^{2}={\frac  {1}{n}}\sum \limits _{{i=1}}^{n}\left(X_{i}-{\bar  {X}}\right)^{2}={\frac  {1}{n}}\sum \limits _{{i=1}}^{n}X_{i}^{2}-\left({\frac  {1}{n}}\sum \limits _{{i=1}}^{n}X_{i}\right)^{2},$

- несмещённая (исправленная) дисперсия — это случайная величина $S^{2}={\frac  {1}{n-1}}\sum \limits _{{i=1}}^{n}\left(X_{i}-{\bar  {X}}\right)^{2}$ или $S^{2}={\frac  {n}{n-1}}S_{n}^{2}.$

- эффективная выборочная дисперсия $S^{x^{2}}={\frac  {n-1}{n+1}}S_{n}^{2}.$ Эта оценка будет не несмещенной;
Все три выборочные дисперсии являются состоятельными оценками теоретической дисперсии. Если ${\mathrm  {D}}[X_{i}]=\sigma ^{2}<\infty$ , то $S_{n}^{2}\to ^{{\!\!\!\!\!\!{\mathbb  {P}}}}\;\sigma ^{2}$
, $S^{2}\to ^{{\!\!\!\!\!\!{\mathbb  {P}}}}\;\sigma ^{2}$ и $S^{x^{2}}\to ^{{\!\!\!\!\!\!{\mathbb  {P}}}}\;\sigma ^{2}$.



####	Для выборок объема 10, 100, 1000, 10^{4} из стандартного нормального закона(0, 1)

$$f(x,\mu,\sigma)={\tfrac {1}{\sigma {\sqrt {2\pi }}}}\;e^{-{\frac {(x-\mu )^{2}}{2\sigma ^{2}}}},$$
$$F(x,0,1) = \frac 1 {\sqrt {2 \pi}} \int _{-\infty} ^x e^{ -\frac {t^2}{2}} dt.$$
Статистики случайной выборки выборок:


```
##         vars   var varef
## x10    0.819 0.910 1.001
## x100   0.809 0.817 0.826
## x1000  1.009 1.010 1.011
## x10000 0.997 0.998 0.998
```

#### Абсолютное значение отклонения(=1)



В среднем для 10 средних по выборке:


```
##           vars     var   varef
## x10    0.42094 0.37661 0.34830
## x100   0.07320 0.07596 0.07872
## x1000  0.02870 0.02893 0.02927
## x10000 0.00743 0.00745 0.00747
```

<p align="right"><b>Выводы:</b></p>
<p align="right"><i>При больших n, асимптотически эффективной оценкой дисперсии 
является выборочная дисперсия. </i></p></p>
<p align="right"><i>При больших n, состоятельной оценкой дисперсии 
является исправленная(не смещенная) выборочная дисперсия.</i></p></p>

----

#### Оценка параметров, использую метод максимального правдоподобия.



#### Построение оценки для Коши

$f_{X}(x)={\frac  {1}{\pi \gamma \left[1+\left({\frac  {x-x_{0}}{\gamma }}\right)^{2}\right]}}={1 \over \pi }\left[{\gamma  \over (x-x_{0})^{2}+\gamma ^{2}}\right]$, где ${\displaystyle x_{0}\in \mathbb {R} }$ — параметр сдвига; ${\displaystyle \gamma >0}$ — параметр масштаба.

$$LL(x;x_{0},\gamma)=n*log(\gamma)+\sum n*log(\gamma^2 + (x_{i}−x_{0})^ 2 )$$

Запишем так:

$$\log{\left (\frac{y}{\pi} \frac{1}{y^{2} + \left(x - z\right)^{2}} \right )}$$

Частные производные:

$$\frac{\partial}{\partial y} f{\left (x,y,z \right )} = \frac{1}{y} \left(y^{2} + \left(x - z\right)^{2}\right) \left(- \frac{2 y^{2}}{\left(y^{2} + \left(x - z\right)^{2}\right)^{2}} + \frac{1}{y^{2} + \left(x - z\right)^{2}}\right) = 0$$
$$\frac{\partial}{\partial z} f{\left (x,y,z \right )} = \frac{2 x - 2 z}{y^{2} + \left(x - z\right)^{2}} =0$$

Естественно, нужно быть овощем, чтобы решать такую систему уравнений. По этому все вычисления предоставим mledist из библиотеки fitdistrplus. 

#### Построение оценки для равномерного распределения

После прошлой попытки построить оценку, ленивые Апачи покидают Американские земли.

#### Сравнение с Коши


```
## location    scale 
##    0.862    1.713
```

Для оценка дисперсии по матрице Гессе:


```
## [1] -0.00233
```

<p align="right"><i>Гессиан положительно определён => найдена точка локального минимума функции.</i></p></p>

#### Сравнение с Равномерным


```
##   min   max 
## -3.55  4.10
```

Для оценка дисперсии по матрице Гессе:


```
## [1] NA
```

#### Предположение о неизвестном распределении.

![](README_figs/README-unnamed-chunk-10-1.png)<!-- -->

<p align="right"><b>Предположение:</b></p>
<p align="right"><i>Коши или нормальное, т.к. тяжелые хвосты и холм</i></p></p>

#### Сравнение с Коши


```
## $estimate
## location    scale 
##  0.00531  0.55711 
## 
## $convergence
## [1] 0
## 
## $loglik
## [1] -10602
## 
## $hessian
##          location scale
## location     7873   271
## scale         271 14681
## 
## $optim.function
## [1] "optim"
## 
## $fix.arg
## NULL
## 
## $optim.method
## [1] "Nelder-Mead"
## 
## $fix.arg.fun
## NULL
## 
## $weights
## NULL
## 
## $counts
## function gradient 
##       39       NA 
## 
## $optim.message
## NULL
```
Посмотрим гистограмы:
![](README_figs/README-unnamed-chunk-12-1.png)<!-- -->
<p align="right"><i>Близко</i></p></p>


#### Сравнение с нормальным


```
## $estimate
##    mean      sd 
## -0.0546  0.9930 
## 
## $convergence
## [1] 0
## 
## $loglik
## [1] -9883
## 
## $hessian
##      mean    sd
## mean 7099     0
## sd      0 14198
## 
## $optim.function
## [1] "optim"
## 
## $fix.arg
## NULL
## 
## $optim.method
## [1] "Nelder-Mead"
## 
## $fix.arg.fun
## NULL
## 
## $weights
## NULL
## 
## $counts
## function gradient 
##       43       NA 
## 
## $optim.message
## NULL
```

Посмотрим гистограмы:
![](README_figs/README-unnamed-chunk-14-1.png)<!-- -->

<p align="right"><i>Не так близко, как Коши</i></p></p>
