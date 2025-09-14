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