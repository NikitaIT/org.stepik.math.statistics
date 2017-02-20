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
    
[-> 2. {отчет}](#Idz_2)
    
[-> 3. {отчет}](#Idz_3)
    
[-> 4. {отчет}](#Idz_4)
    
[-> 5. {отчет}](#Idz_5)

---

### <a name="Idz_1"></a>	ИДЗ №1

[- -> Код {file}](https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ.R)

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

* e.	вычислить следующие выборочные характеристики:  выборочное среднее, выборочную дисперсию, выборочную асимметрию, выборочный эксцесс; (см. стр. 20-22 методички)

```R
# e. все характеристики
  allProp <<- function(x){ data.frame(mean = mean(x),var = var(x),asm = asm(x),exc = exc(x))} 
```

* f.	сравнить результаты пункта e  с реальными характеристиками распределения 

<table>
<tr><td colspan="9" style="border-bottom: 20px solid black"></td></tr>
<tr><td><td>meansReal<td>means<td>varsReal<td>vars<td>asmsReal<td>asms<td>excsReal<td>excs</td></tr>
<tr><td>G<td>0.64033225<td>1.88373867<td>0.1428149<td>0.35542869<td>-0.5960285<td>0.4780694<td>-1.29477217<td>0.13338975</td></tr>
<tr><td>N,<td>0.48069643<td>2.31551091<td>0.16544511<td>0.09183933<td>0.0701878<td>-0.010092957<td>-1.72443444<td>-0.04848023</td></tr>
<tr><td>NB<td>2.75245521612138e-20<td>127.153<td>2.2098810347116e-39,<td>599.19078<td>2.085205315<td>0.29146007<td>2.534649651<td>0.0138651671</td></tr>
<tr><td colspan="9"></td></tr></table>

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

<h2 align="center">Гистограмма</h2>
<img src="https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_1/hist.png"></img>
<h2 align="center">Эмпирические</h2>
<img src="https://github.com/NikitaIT/org.stepik.math.statistics/blob/master/IDZ_1/empiricalPlot.png"></img>
</div>

```R
# сохраняем результаты
write.csv(AnnualDiameterProp,file = "AnnualDiameterProp.csv")
   ```











