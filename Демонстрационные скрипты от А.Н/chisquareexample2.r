# Пример Критерия хи-квадрат для нетабулированной выборки
# Генерируем выборку и задаем параметры кандидата.
x=rbeta(1000,2,4)
n=length(x)
p=2
q=4



# Необходимо выбрать число промежутков и их вид.
# Для числа промежутков используем то же правило, что и при построении гистограмм.
# Однако, следует учесть, что в каждый промежуток должно попасть (ожидаемо) как минимум 5ть элементов выборки.
# Желательно, чтобы объем выборки/число интервалов>5
# Поступим следующим образом. Пусть F --- функция распределения кандидата. Построим последовательность квантилей
# q_i так, чтобы F(q_i+1)-F(q_i)>5/n
k=(log(n)/log(2)+1+2*n^(1/3))/2
k=round(k,digits=0) # задаем число интервалов
{if (k<6) {k=6} else k=k}





num=floor(n/k)
res=n-num*k
cval=1/num
qt=c(1:(k-1))
qt=(qt*num+res)/n
qt=qbeta(qt,p,q)
# Получили qt --- концы интервалов. Т.е интервалы имеют вид I_i=[qt[i],qt[i+1]]. Кроме первого и последнего.
# I_1=(-\infty,qt[1]), I_k=[qt[k],+\infty)
# Согласно нашему выбору ожидаемые вероятности попадания в  интервалы I_i равны num/n (кроме I_1, для него (num+res)/n)


# Теперь вычислим наблюлаемое число наблюдений в этих интервалах
# Вызовем функцию hist с таким разбиением
h=hist(x,breaks=c(0,qt,1),plot=FALSE)
# Нас интересует список h$counts, где посчитано число элементов выборки, попавших в каждый промежуток.
obs=h$counts


# Ожидаемое число мы уже знаем
exp=c(1:k)
exp[1]=num+res
for (i in 2:k) {exp[i]=num}


# Выведем это в наглядную таблицу
chitable=data.frame(obs,exp,obs-exp,(obs-exp)/sqrt(exp))
names(chitable)=c("obs","exp","diff","pearson_residuals")



# Вычислим значение статистики хи-квадрат
chi=sum((chitable$pearson_residuals)^2)

# Применим критерий
alpha=0.1
testres=chisqtest(chi,k-1,alpha)
answer=c("")
{if (testres==0) {answer=sprintf("Данные не противоречат основной гипотезе")}
  else {answer=sprintf("Основная гипотеза отвергается на уровне значимости %f",alpha)}}

# Вычислим реально достигнутый уровень значимости
pvalue=1-pchisq(chi,k-1)

# Выведем все полученный результаты

res=list(intervals_left=c(-Inf,qt),intervals_right=c(qt,+Inf),observed=chitable$obs, 
           expected=chitable$exp,pearson_residuals=chitable$pearson_residuals,
         pearson_value=chi,pvalue=pvalue,conflev=alpha,
         answer=answer)
print(res)


# Применим стандартную функцию chisq.test
chisqres=chisq.test(chitable$obs,p=chitable$exp/n)
chisqres$statistic
chisqres$p.value
