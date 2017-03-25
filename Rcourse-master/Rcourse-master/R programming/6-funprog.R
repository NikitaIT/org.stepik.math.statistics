# Random walk with absorption
simulate_walk <- function(rad=6, n_max = 100, p = 1e-2) {
  current_position <- c(0,0)
  for (i in 1:n_max) {
    is_absorbed <- rbinom(1, 1, p)
    if (is_absorbed) return(1)
    current_position <- current_position + c(rnorm(1),rnorm(1))
    if (sqrt(current_position[1]^2+current_position[2]^2) > rad) return(2)
  }
  return(3)
}

# Simulate results
result <- replicate(10000, simulate_walk(), simplify = T)
result2 = sum(result==2)/length(result)
head(result)
result <- data.frame(
  status = sapply(result, function(x) x$status),
  positionX = sapply(result, function(x) x$position1),
  positionY = sapply(result, function(x) x$position2),
  steps = sapply(result, function(x) x$steps)
)

# Inspect results
tapply(result$positionX, result$status, length)
tapply(result$steps, result$status, mean)


install.packages("dplyr")
library(dplyr)
df = data.frame(first_name=" (им€)",
      last_name= "(фамили€)",
      email_address ="(электронна€ почта)",
      postal_address ="(почтовый адрес)",
      date_added=1)
select(df, -3:4)
df %>% select(c(1:2, 5))
select(df, contains("name"), date_added)
select(df, first_name, last_name, date_added)
select(df, -contains("_add"))
select(df, matches("_.{4,5}$"))
transform


install.packages("rmarkdown")


test_data = x<-as.data.frame(list(V1 = c(NA, -0.5, -0.7, -8), V2 = c(-0.3, NA, -2, -1.2), V3 = c(1, 2, 3, NA)))

get_negative_values <- function(test_data){
  a = sapply(test_data,function(x){x[!is.na(x)][x[!is.na(x)]<0]})
  a[ sapply(a,function(x){length(x)})>0]
}


test_data <- as.data.frame(list(V1 = c(NA, NA, NA, NA, 13, 12, 9, 10, 8, 9, 11, 11, 10, 12, 9), V2 = c(NA, 12, 8, NA, 11, 11, 9, 8, 8, 10, 10, 11, 10, 10, 10), V3 = c(NA, 5, NA, 13, 12, 11, 11, 14, 8, 12, 8, 8, 10, 10, 8), V4 = c(10, 10, 10, 10, 13, 10, 11, 7, 12, 10, 7, 10, 13, 10, 9)))
na_rm  <- function(x){
  apply(x,2,function(x){x[is.na(x)]=mean(x,na.rm = T); x})
}

na_rm(test_data)


















