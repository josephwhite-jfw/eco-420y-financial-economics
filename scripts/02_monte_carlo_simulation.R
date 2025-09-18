set.seed(12345)
i = 1 
return = rep(-1,10000)

while (i<=10000) {
  die = sample(1:6,1)
  if (die==6) return[i] = 4
  i = i+1
}
  
print(mean(return))
print(var(return))

set.seed(12345)
i = 1 
return = rep(-1,100000)

while (i<=100000) {
  die = sample(1:6,1)
  if (die==6) return[i] = 4
  i = i+1
}

print(mean(return))
print(var(return))

set.seed(12345)
i=1
return = rep(-1,100000)
while (i<=100000) {
  die1 = sample(1:6,1)
  die2 = sample(1:6,1)
  if (die1+die2==12) return[i] = 29
  i = i+1
}

print(mean(return))
print(var(return))

set.seed(12345)
i=1
muA = rep(log(1)-log(2),100000); muB = rep(log(1)-log(2),100000)
while (i<=100000) {
  die = sample(1:6,1)
  if (die==6) muA[i] = log(6)-log(2)
  dice1 = sample(1:6,1)
  dice2 = sample(1:6,1)
  if (dice1+dice2==12) muB[i] = log(31)-log(2)
  i = i+1
}

print(mean(muA))
print(mean(muB))

library(tseries)
library(quantmod)
symbols = c("SP500")
getSymbols(symbols, src = "FRED")
class(SP500)
head(SP500)

# XTS object
set.seed(12345)
dates = as.Date("2020-01-01") + 0:9
values = rnorm(10)
xts_data = xts(values, order.by = dates)
head(xts_data)
class(xts_data)

sp = coredata(SP500)
n = nrow(sp)
j = 1
ra.sp=NULL
while (j+252<=n) {
  ra.sp = c(ra.sp, (sp[j+252]-sp[j])/sp[j])
  j = j + 1
}

quantile(ra.sp, prob=c(0.05,0.5,0.95),na.rm=T)
hist(ra.sp,na.rm=T)
print(ra.sp)


# Optional Stuff
# A new strategy: buy-and-hold-until-return-being-at-least-10percent
# Distribution of duration 
sp = coredata(SP500)
n = nrow(sp)
j = 1
r.du=NULL
while (j+252<=n) {
  return = (sp[(j+1):length(sp)] - sp[j])/sp[j]
  r.du = c(r.du,which(return>0.1)[1])
  j = j + 1
}
r.du
quantile(r.du, prob=c(0.05,0.5,0.95),na.rm=T)

j = 1177
return = (sp[(j+1):length(sp)] - sp[j])/sp[j]
return

j = 51
return = (sp[(j+1):length(sp)] - sp[j])/sp[j]
return


# check dates of missing values
index(SP500[is.na(SP500)])

# lag price and daily return
SP500$lag = lag(SP500, k=1)
SP500$dailyreturn = (SP500$SP500-SP500$lag)/SP500$lag
head(SP500)
