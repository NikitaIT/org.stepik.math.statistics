<p align="right">Студент: Федоров Н.С.</p>

<p align="right">Группа: 5362</p>

<p align="right">Преподаватель: Медведев А.Н.</p>

# <p align="center">[ Репозиторий Статистика ](https://github.com/NikitaIT/org.stepik.math.statistics/)</p>

<p align="right"><i>Из этой первой лекции по теории вероятностей я запомнил толь-</i></p>
<p align="right"><i>ко полузнакомый термин «математическое ожидание». Незнакомец</i></p>
<p align="right"><i>употреблял этот термин неоднократно, и каждый раз я представ-</i></p>
<p align="right"><i>лял себе большое помещение, вроде зала ожидания, с кафельным</i></p>
<p align="right"><i>полом, где сидят люди с портфелями и бюварами и, подбрасывая</i></p>
<p align="right"><i>время от времени к потолку монетки и бутерброды, сосредоточен-</i></p>
<p align="right"><i>но чего-то ожидают. До сих пор я часто вижу это во сне. Но тут</i></p>
<p align="right"><i>незнакомец оглушил меня звонким термином «предельная теорема</i></p>
<p align="right"><i>Муавра — Лапласа» и сказал, что всё это к делу не относится.</i></p>
<p align="right">Аркадий и Борис Стругацкие, Стажёры</p>

---

[ Отчеты по ИДЗ ](https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_1/IDZ_1.md)
    
[-> 1. Нормальное, Гамма и N-Биомиальное распределения {отчет}](#Idz_1)
    
[-> 2. Оценки, Метод максимального правдоподобия{отчет}](#Idz_2)
    
[-> 3. {отчет}](#Idz_3)
    
[-> 4. {отчет}](#Idz_4)
    
[-> 5. {отчет}](#Idz_5)

---

### <a name="Idz_2"></a>	ИДЗ №2

[- -> Код {file}](https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_2/IDZ_2.R)

[- -> Данные Коши распределения {file}](https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_2/cauchy_1.csv)

[- -> Данные равномерного распределения {file}](https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_2/unif_2.csv)

[- -> Данные искомого распределения{file}](https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_2/type1_1.csv)

[- -> Задание {file}](https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_1/%D0%97%D0%B0%D0%B4%D0%B0%D0%BD%D0%B8%D0%B5%20%D0%BE%D1%82%209.2.2017.docx)

#### Данные

Даны следующие распределения и параметры:

Тип Распределения			| Параметры
----------------------------|--------------
Коши						| a in R, b > 0 
Равномерное					| a ≤ b
Искомое					 	| ??

#### Выполнение работы

##### Выборочная дисперсия, несмещенная выборочная дисперсия, эффективная выборочная дисперсия.

```R
vars  <- function(x){sum((x-mean(x))^2)/(length(x))}
var   <- var
varef <- function(x){(length(x)+1)*var(x)/(length(x))}
```
Одна из 10 выборок средних по выборке:

       |    vars    |     var    |    varef
-------|------------|------------|------------
x10    | 0.5970598  | 0.6633998  | 0.7297398
x100   | 0.8201128  | 0.8283968  | 0.8366807
x1000  | 1.0053878  | 1.0063942  | 1.0074006
x10000 | 0.9976700  | 0.9977697  | 0.9978695

##### Абсолютное значение отклонения(=1)

```R
varabs<- function(x){abs(allbind()-1)}
```
В среднем для 10 средних по выборке:

       |    vars    |     var    |    varef
-------|------------|------------|------------
x10    | 0.34654800 | 0.33488905 | 0.32837795
x100   | 0.12398427 | 0.12725684 | 0.13052941
x1000  | 0.03461316 | 0.03384700 | 0.03308085
x10000 | 0.01382609 | 0.01378747 | 0.01376773

<p align="right"><b>Выводы:</b></p>
<p align="right"><i>При больших n, асимптотически эффективной оценкой дисперсии 
является выборочная дисперсия. </i></p></p>
<p align="right"><i>При больших n, состоятельной оценкой дисперсии 
является исправленная(не смещенная) выборочная дисперсия.</i></p></p>

----

##### Оценка параметров, использую метод максимального правдоподобия.

```R
install.packages("fitdistrplus");
library(fitdistrplus);
```
```R
mledist(data=cauchy$x, distr="cauchy", optim.method="default",
        lower=-Inf, upper=Inf,start = formals(cauchy$x))
```

$convergence | 0 		 | успешно найдены оценки параметров 0-10 Y-N и коды-ошибки
-------------|-----------|---------------------------------------------------------------
$loglik 	 | -15262.95 | значение логарифма функции правдоподобия при найденной оценке.

$estimate — оценка неизвестных параметров.

 location |  scale 
----------|----------
0.8619625 | 1.7134990 

$hessian  — значения гессиана.

         |  location   |   scale
---------|-------------|----------
location | 846.786988  | 6.686417
scale    |  6.686417   |856.165432

<p align="right"><i>Гессиан положительно определён => найдена точка локального минимума функции.</i></p></p>

```R
 mledist(data=unif$x, distr="unif", optim.method="default",lower=-Inf, upper=Inf,start = formals(unif$x))
```

$estimate — оценка неизвестных параметров.

   min    |   max 
----------|----------
-3.546441 | 4.104417 

##### Предположение о неизвестном распределении.

```R
par(mfrow=c(2,2))
hist(type$x,breaks = 2*length(type$x)^(1/3), freq = F, col = "lightblue");
hist(rnorm(n = 10^6, mean = 0, sd = 1),freq = F, col = "lightblue");
hist(rcauchy(n = 10^2,0,0.1),breaks = 50,freq = F, col = "lightblue");
hist(rt(n = 10^2,df=1),breaks = 50,freq = F, col = "lightblue");
```

<p align="right"><b>Предположение:</b></p>
<p align="right"><i>Коши или нормальное, т.к. тяжелые хвосты и холм</i></p></p>
<img src="https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_2/4hist.png"></img>




---


### <a name="Idz_1"></a>	ИДЗ №1

[- -> Код {file}](https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_1/IDZ.R)

[- -> Данные {file}](https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_1/AnnualDiameterProp.csv)

[- -> Задание {file}](https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_1/%D0%97%D0%B0%D0%B4%D0%B0%D0%BD%D0%B8%D0%B5%20%D0%BE%D1%82%209.2.2017.docx)

#### Данные

Даны следующие распределения и параметры:

Тип Распределения			| Параметры
----------------------------|--------------
Гамма						| p=10, b=5.3
Нормальное					| a=2.3, σ=0.3
Отрицательное Биномиальное 	| m=32, p=1/5


Распределение в файле: [file](https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_1/AnnualDiameterProp.csv)

#### Выполнение работы

Необходимо для каждого распределения:

* a.	сгенерировать выборку длины 1000 из данного распределения (см. стр. 19 методички)

```R
  #a.	сгенерировать выборку длины 1000 из данного распределения (стр. 19)
  n <- 1000;
  rG <<- rgamma(n = n, shape = 10, rate = 5.3);
  ...
```

* b.	построить по данной выборке эмпирическую функцию распределения;

```R
  #b. построить по данной выборке эмпирическую функцию распределения; 
  bildEmpiricalPlots <- function(){ empiricalPlot(rG); ... }
```
```R
  #  значения функции распределения в точке x
  funP <<- list(G = {function(x){ pgamma(q = x, shape = 10, rate = 5.3);}}, ... }
  );
```

<h2 align="center">Эмпирические</h2>
<table>
<tr>
<td>
<h2>RplotG</h2>
<img src="https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_1/RplotG.png"></img>
<td>
<h2>RplotN</h2>
<img src="https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_1/RplotN.png"></img>
<td>
<h2>RplotNB</h2>
<img src="https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_1/RplotNB.png"></img>
</tr>
</table>

<p align="right"><b>Выводы:</b></p>
<p align="right">Ну что тут скажешь, ничего особенного</p>

----

* c.	построить гистограмму частот;
* d.	сравнить гистограмму частот и реальную плотность данного распределения (вычисление значения плотности в точке в пакете R описано на той же 19 стр.)

```R
  #c&d.	сравнить гистограмму частот и реальную плотность данного распределения
  # точки для наложения
  ranges <- list(G = (((range(rG)[1]*100):(range(rG)[2]*100))/100), ... )
  # плотности
  densitys <- list(G = dgamma(x = ranges$G,shape = 10, rate = 5.3), ... )
  #   построение Зеленый - ген.совок. Красный - выборка
hist3 <- function(){
  {
    hist(rG, breaks = 20, freq = F, ... );
    lines(density(rG), col = "red", lwd = 2);
    lines(x = ranges$G, y = densitys$G, col = "green", lwd = 2);
  }
 ```

<p color="green" align="right">Реальное распределение - ЗЕЛЕНЫМ</p>
<p color="red" align="right">Выборочное распределение - КРАСНЫМ</p>

<h2 align="center">Гистограммы</h2>
<table>
<tr>
<td>
<h2>RhistG</h2>
<img src="https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_1/RhistG.png"></img>
<td>
<h2>RhistN</h2>
<img src="https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_1/RhistN.png"></img>
<td>
<div style="width: 33%">
<h2>RhistNB</h2>
<img src="https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_1/RhistNB.png"></img>
</tr>
</table>

<p align="right"><b>Выводы:</b></p>
<p align="right"><i>По гистограммам видно, что распр. выборки из 1000 наблюбений близко к ген.сов.</i></p>
<p align="right"><i>Так же очевидно, что работает ЗБЧ и ЦПТ</i></p>
<p align="right"><i>И гамма и NB иногда могут быть приближены нормальным распределением</i></p>
----


* e.	вычислить следующие выборочные характеристики:  выборочное среднее, выборочную дисперсию, выборочную асимметрию, выборочный эксцесс; (см. стр. 20-22 методички)

```R
# e. все характеристики
  allProp <<- function(x){ data.frame(mean = mean(x),var = var(x),asm = asm(x),exc = exc(x))} 
```

* f.	сравнить результаты пункта e  с реальными характеристиками распределения 

Таблица сопоставления:

    |    mean   |     var     |     asm    |    exc
----|-----------|-------------|------------|----------
rG  |  1.919927 |  0.3470468  | 0.53692972 |0.38642037
G   |  1.886792 |  0.3559986  | 0.53323230 |0.38232323
    |           |             |            |          
rN  |  2.284932 |  0.0936350  | 0.05035014 |0.01973037
N   |  2.300000 |  0.0900000  | 0.05003010 |0.01912628
    |           |             |            |          
rNB | 129.320000| 574.4940941 | 0.24431109 |0.31274550
NB  | 128.424362| 574.8357284 | 0.24243744 |0.32824786

Таблица смежности:

    |    mean   |     var     |     asm    |    exc
----|-----------|-------------|------------|----------
dG  | 0.03313449|-0.008951797 |0.0036974174| 0.0040971
dN  |-0.01506806| 0.003635003 |0.0003200364| 0.0006040
dNB | 0.89563800|-0.341634306 |0.0018736453|-0.0155023

<p align="right"><b>Выводы:</b></p>
<p align="right"><i>Значения выборки совпали с значегиями для ГС, различия не значимы</i></p></p>
----
Распределение из файла:

```R
# читаем темпиратуры
AnnualDiameter<<-as.data.frame(read.csv("IDZ_1/annual-diameter-of-skirt-at-hem-.csv",col.names = c("AnnualDiameter")));
   ```
Повторяем пункты a-d

<table>
<tr><td colspan="5"></td></tr>
<tr><td></td><td>mean</td><td>var</td><td>asm</td>,<td>exc</td></tr>
<tr><td></td><td>731.086956</td><td>51786.0811</td><td>-0.7350737</td><td>0.7727566</td></tr>
<tr><td colspan="5"></td></tr></table>

<table>
<tr>
<td>
<h2 align="center">Гистограмма</h2>
<img src="https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_1/hist.png"></img>
<td>
<h2 align="center">Эмпирические</h2>
<img src="https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_1/empiricalPlot.png"></img>
</td></tr></table>

<p align="right"><b>Выводы:</b></p>
<p align="right"><i>Данных недостаточно, чтобы делать выводы</i></p>
----
```R
# сохраняем результаты
write.csv(AnnualDiameterProp,file = "AnnualDiameterProp.csv")
   ```











