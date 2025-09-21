#======================== ECO_420Y_Homework_1 ==================================
#============================ Question 1 =======================================
## Pay 2 dollars to roll three dice. The casino pays 5 if sum=10, else pays 1.

set.seed(123)

i = 1
return = rep(-1,100000)

while (i <= 100000) {
  die1 = sample(1:6,1)
  die2 = sample(1:6,1)
  die3 = sample(1:6,1)
  if ((die1+die2+die3)==10) return[i] = 3
  i = i+1
}

print(mean(return))
print(var(return))

#============================ Question 2 =======================================
## Simulate 95% CI for 5-year buy-hold-sell returns on SP500

library(tseries)
library(quantmod)

symbols = c("SP500")
getSymbols(symbols, src="FRED")

sp = coredata(SP500)
n = nrow(sp)
j = 1
ra_sp = NULL

while (j + 252*5 <= n) {
  ra_sp = c(ra_sp, (sp[j+252*5] - sp[j]) / sp[j])
  j = j + 1
}

quantile(ra_sp, probs =c(0.025,0.975), na.rm=TRUE)
