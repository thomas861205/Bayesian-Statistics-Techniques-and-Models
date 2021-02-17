m = 1e5

theta = rbeta(m, 5, 3)
mu_odd = mean(1 / (1 - theta) - 1)

odd = theta / (1 - theta)
ind = odd > 1
mean(ind)


y = rnorm(m, 0, 1)
quantile(x=y, prob=.3)
qnorm(.3, 0, 1) # true