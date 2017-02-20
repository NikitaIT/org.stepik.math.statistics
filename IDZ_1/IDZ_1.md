# <p align="center">[ Репозиторий Статистика ](https://github.com/NikitaIT/org.stepik.math.statistics/)</p>

<p align="right">Студент: Федоров Н.С.</p>

<p align="right">Группа: 5362</p>

<p align="right">Преподаватель: Медведев А.Н.</p>

---

[ Отчет по ИДЗ ](https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_1/IDZ_1.md)
    
[-> 1](#Idz_1)
    
[-> 2](#Idz_2)
    
[-> 3](#Idz_3)
    
[-> 4](#Idz_4)
    
[-> 5](#Idz_5)

---

#### <a name="Idz_1"></a>	ИДЗ №1

##### Задание

Даны следующие распределения и параметры:

Тип Распределения			| Параметры
----------------------------|--------------
Гамма						| p=10, b=5.3
Нормальное					| a=2.3, σ=0.3
Отрицательное Биномиальное 	| m=32, p=1/5


Распределение в файле: file

Необходимо для каждого распределения:

* a.	сгенерировать выборку длины 1000 из данного распределения (см. стр. 19 методички)
* b.	построить по данной выборке эмпирическую функцию распределения;
* c.	построить гистограмму частот;
* d.	сравнить гистограмму частот и реальную плотность данного распределения (вычисление значения плотности в точке в пакете R описано на той же 19 стр.)
* e.	вычислить следующие выборочные характеристики:  выборочное среднее, выборочную дисперсию, выборочную асимметрию, выборочный эксцесс; 
(см. стр. 20-22 методички)
* f.	сравнить результаты пункта e  с реальными характеристиками распределения 



```R
  #a.	сгенерировать выборку длины 1000 из данного распределения (стр. 19)
  n <- 1000;
  rG <<- rgamma(n = n, shape = 10, rate = 5.3);
  ...
```
```R
  #b. построить по данной выборке эмпирическую функцию распределения; 
  bildEmpiricalPlots <- function(){ empiricalPlot(rG); ... }
```
```R
  #  значения функции распределения в точке x
  funP <<- list(G = {function(x){ pgamma(q = x, shape = 10, rate = 5.3);}}, ... }
  );
```
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

```R
# e. все характеристики
  allProp <<- function(x){ data.frame(mean = mean(x),var = var(x),asm = asm(x),exc = exc(x))} 
```

```R
# читаем темпиратуры
AnnualDiameter<<-as.data.frame(read.csv("IDZ_1/annual-diameter-of-skirt-at-hem-.csv",col.names = c("AnnualDiameter")));
   ```
Повторяем пункты a-d

```R
# сохраняем результаты
write.csv(AnnualDiameterProp,file = "AnnualDiameterProp.csv")
   ```


<div style="text-align: center;">
<h1>Отчет 1</h1>
<table style="margin:auto;text-align:center;border:1px solid black;">
<tr><td colspan="9" style="border-bottom: 20px solid black"></td></tr>
<tr><td>"",<td>"meansReal",<td>"means",<td>"varsReal",<td>"vars",<td>"asmsReal",<td>"asms",<td>"excsReal",<td>"excs"</td></tr>
<tr><td>"G",<td>0.640332256909482,<td>1.88373867932996,<td>0.142814997054233,<td>0.35542869877411,<td>-0.596028551852354,<td>0.478069480771901,<td>-1.29477217813216,<td>0.133389753239832</td></tr>
<tr><td>"N",<td>0.48069643020812,<td>2.31551091492194,<td>0.165445118275218,<td>0.0918393367292663,<td>0.0701878180934444,<td>-0.0100929571610628,<td>-1.72443444383002,<td>-0.048480234942891</td></tr>
<tr><td>"NB",<td>2.75245521612138e-20,<td>127.153,<td>2.2098810347116e-39,<td>599.190781781782,<td>2.08520531547468,<td>0.291460077013184,<td>2.5346496517362,<td>0.0138651671837597</td></tr>
<tr><td colspan="9" style="border-bottom: 2px solid black"></td></tr></table>
<div style="width: 33%">
<h2>RhistG</h2>
<img src="https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_1/RhistG.png"></img>
</div>
<div style="width: 33%">
<h2>RhistN</h2>
<img src="https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_1/RhistN.png"></img>
</div>
<div style="width: 33%">
<h2>RhistNB</h2>
<img src="https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_1/RhistNB.png"></img>
</div>
<div style="width: 33%">
<h2>RplotG</h2>
<img src="https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_1/RplotG.png"></img>
</div>
<div style="width: 33%">
<h2>RplotN</h2>
<img src="https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_1/RplotN.png"></img>
</div>
<div style="width: 33%">
<h2>RplotNB</h2>
<img src="https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_1/RplotNB.png"></img>
</div>

<h1>Отчет 2</h1>
<table style="margin:auto;text-align:center;border:1px solid black;">
<tr><td colspan="5" style="border-bottom: 2px solid black"></td></tr>
<tr><td>""</td><td>"mean"</td><td>"var"</td><td>"asm"</td>,<td>"exc"</td></tr>
<tr><td>"1"</td><td>731.086956521739</td><td>51786.0811594203</td><td>-0.735073717935867</td><td>0.772756645400363</td></tr>
<tr><td colspan="5" style="border-bottom: 2px solid black"></td></tr></table>

<h2>hist</h2>
<img src="https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_1/hist.png"></img>
<h2>empiricalPlot</h2>
<img src="https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_1/empiricalPlot.png"></img>
</div>