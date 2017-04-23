# Альтернативный способ выбора промежутков
x=rbeta(1000,2,4)
n=length(x)
p=2
q=4

k=(log(n)/log(2)+1+2*n^(1/3))/2
k=round(k,digits=0) # задаем число интервалов
{if (k<6) {k=6} else k=k}



# Вызываем функцию hist
h=hist(x,breaks=k,plot=FALSE)
# hist предлагает взять большее число промежутков. Ну чтож, возьмем. Нас интересует h$breaks. В нем содержится 
# разбиение на интервалы. А в h$counts число наблюдаемых попаданий в них.
br=h$breaks
obs=h$counts
# Однако, ожидаемое числа наблюдений придется вычислять.
m=length(br)
exp=round(n*(pbeta(br[2:m],p,q)-pbeta(br[1:(m-1)],p,q)))



# Необходимо проверить, что в каждый ожидамо попадает как минимум 5. Если нет, то нужно объденить 
# плохой промежуток с соседним. Можно вручную, можно написать простую функцию.
w=which(exp<5)
{while (length(w)>0) {
  br=br[-w]
  print(br)
  m=length(br)
  exp=round(n*(pbeta(br[2:m],p,q)-pbeta(br[1:(m-1)],p,q)))
  w=which(exp<5)
}}





# Теперь, когда выкинули плохие интервалы пересчитаем, число наблюдаемых элементов выборки
h=hist(x,breaks=br,plot=FALSE)
obs=h$counts

# Вычислим те же значения, что и впримере 2.
pearson_residuals=(obs-exp)/sqrt(exp)
chi=sum(pearson_residuals^2)
alpha=0.1
testres=chisqtest(chi,length(exp),alpha)
answer=c("")
{if (testres==0) {answer=sprintf("Данные не противоречат основной гипотезе")}
  else {answer=sprintf("Основная гипотеза отвергается на уровне значимости %f",alpha)}}
pvalue=1-pchisq(chi,length(exp)-1)
# Выведем результаты
res=list(intervals_left=br[1:(m-1)],intervals_right=br[2:m],observed=obs, 
         expected=exp,pearson_residuals=pearson_residuals,
         pearson_value=chi,pvalue=pvalue,conflev=alpha,
         answer=answer)
print(res)


