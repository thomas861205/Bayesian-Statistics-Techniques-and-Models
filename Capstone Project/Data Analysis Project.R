dat <- read.table("C:/Users/Thomas/Downloads/Linear_models-master/Linear_models-master/hw6/acc.txt", header=T)
head(dat)
# ACC: acceleration of different vehicles.
# WHP: weight-to-horsepower ratio.
# SP: the speed at which they were traveling. 
# G: the grade, G=0 implies the road was horizontal.

fit1 <- lm(ACC ~ WHP + SP + G, data=dat)
# Call:
# lm(formula = ACC ~ WHP + SP + G, data = dat)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -3.3124 -0.9003  0.2486  0.9489  2.3477 

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  7.19949    0.60087  11.982 9.57e-16 ***
# WHP         -0.01838    0.00269  -6.833 1.62e-08 ***
# SP          -0.09347    0.01307  -7.149 5.45e-09 ***
# G           -0.15548    0.09040  -1.720   0.0922 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 1.47 on 46 degrees of freedom
# Multiple R-squared:  0.624, Adjusted R-squared:  0.5995 
# F-statistic: 25.45 on 3 and 46 DF,  p-value: 7.451e-10
plot(predict(fit1), resid(fit1))

set.seed(72)
library("rjags")
library("coda")

mod1_string = " model {
    for (i in 1:length(ACC)) {
        ACC[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*WHP[i] + b[2]*SP[i] + b[3]*G[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

data1_jags = as.list(dat)
params1 = c("b0", "b", "sig")
mod1 = jags.model(textConnection(mod1_string), data=data1_jags, n.chains=3)
update(mod1, 1000) # burn-in
mod1_sim = coda.samples(model=mod1,variable.names=params1, n.iter=10000)
mod1_csim = do.call(rbind, mod1_sim) # combine multiple chains

plot(mod1_sim)
gelman.diag(mod1_sim)
autocorr.diag(mod1_sim)
autocorr.plot(mod1_csim)
effectiveSize(mod1_sim)
raftery.diag(mod1_csim)
colMeans(mod1_csim)
#        b[1]        b[2]        b[3]          b0         sig 
# -0.01830005 -0.09379089 -0.15487931  7.20113606  5.92745221 
dic1 = dic.samples(mod1, n.iter=1e3)
# Mean deviance:  276.3 
# penalty 5.182 
# Penalized deviance: 281.5 

prplot <- function(g,i)
{
# Partial residuals plot for predictor i
  xl<-attributes(g$terms)$term.labels[i]
  yl<-paste("beta*",xl,"+res",sep="")
  x<-model.matrix(g)[,i+1]
  plot(x,g$coeff[i+1]*x+g$res,xlab=xl,ylab=yl)
  abline(0,g$coeff[i+1])
  invisible()
}
prplot(fit1, 1)
prplot(fit1, 2)
prplot(fit1, 3)
plot(fit1$fit, fit1$residuals)
sum(fit1$residuals^2)

fit2 <- lm(ACC ~ I(1/WHP) + SP + G, data=dat)
# Call:
# lm(formula = ACC ~ I(1/WHP) + SP + G, data = dat)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -1.7984 -0.5691 -0.2770  0.5166  3.1755 

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   3.081084   0.402048   7.663 9.31e-10 ***
# I(1/WHP)    104.966921   9.858324  10.648 5.33e-14 ***
# SP           -0.094498   0.009879  -9.566 1.64e-12 ***
# G            -0.186027   0.069105  -2.692  0.00987 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 1.121 on 46 degrees of freedom
# Multiple R-squared:  0.7813,    Adjusted R-squared:  0.7671 
# F-statistic: 54.78 on 3 and 46 DF,  p-value: 3.198e-15

dat$WHP2 = dat$WHP^2
fit3 <- lm(ACC ~  ., data=dat)
# Call:
# lm(formula = ACC ~ . + I(WHP^2), data = dat)

# Residuals:
#      Min       1Q   Median       3Q      Max 
# -1.70446 -0.64576 -0.05457  0.54006  2.28414 

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.011e+01  5.055e-01  19.999  < 2e-16 ***
# WHP         -9.569e-02  9.175e-03 -10.429 1.37e-13 ***
# SP          -1.004e-01  8.188e-03 -12.259 6.08e-16 ***
# G           -2.236e-01  5.689e-02  -3.929  0.00029 ***
# I(WHP^2)     2.710e-04  3.162e-05   8.570 5.18e-11 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.9161 on 45 degrees of freedom
# Multiple R-squared:  0.8572,    Adjusted R-squared:  0.8445 
# F-statistic: 67.51 on 4 and 45 DF,  p-value: < 2.2e-16

mod3_string = " model {
    for (i in 1:length(ACC)) {
        ACC[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*WHP[i] + b[2]*SP[i] + b[3]*G[i] + b[4]*WHP2[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:4) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

data3_jags = as.list(dat)
params1 = c("b0", "b", "sig")
mod3 = jags.model(textConnection(mod3_string), data=data3_jags, n.chains=3)
update(mod3, 1000) # burn-in
mod3_sim = coda.samples(model=mod3,variable.names=params1, n.iter=100000)
mod3_csim = do.call(rbind, mod3_sim) # combine multiple chains

plot(mod3_sim)
gelman.diag(mod3_sim)
autocorr.diag(mod3_sim)
autocorr.plot(mod3_csim)
effectiveSize(mod3_sim)
raftery.diag(mod3_csim)
colMeans(mod3_csim)
#          b[1]          b[2]          b[3]          b[4]            b0           sig 
# -0.0969762749 -0.1006402149 -0.2248987218  0.0002753742 10.1647473566  5.8796627188 
dic3 = dic.samples(mod3, n.iter=1e3)
# Mean deviance:  274.2 
# penalty 5.943 
# Penalized deviance: 280.2

prplot(fit3, 1)



mod4_string = " model {
    for (i in 1:length(ACC)) {
        ACC[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*WHP2[i] + b[2]*SP[i] + b[3]*G[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

data4_jags = as.list(dat)
params1 = c("b0", "b", "sig")
mod4 = jags.model(textConnection(mod4_string), data=data4_jags, n.chains=3)
update(mod4, 1000) # burn-in
mod4_sim = coda.samples(model=mod4,variable.names=params1, n.iter=10000)
mod4_csim = do.call(rbind, mod4_sim) # combine multiple chains
dic4 = dic.samples(mod4, n.iter=1e3) # 282.8


mod5_string = " model {
    for (i in 1:length(ACC)) {
        ACC[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*WHP[i] + b[2]*SP[i] + b[3]*WHP2[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

data5_jags = as.list(dat)
params1 = c("b0", "b", "sig")
mod5 = jags.model(textConnection(mod5_string), data=data5_jags, n.chains=3)
update(mod5, 1000) # burn-in
mod5_sim = coda.samples(model=mod5,variable.names=params1, n.iter=10000)
mod5_csim = do.call(rbind, mod5_sim) # combine multiple chains
dic5 = dic.samples(mod5, n.iter=1e3) # 277.9
colMeans(mod5_csim)


mod6_string = " model {
    for (i in 1:length(ACC)) {
        ACC[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*WHP[i] + b[2]*SP[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:2) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

data6_jags = as.list(dat)
params1 = c("b0", "b", "sig")
mod6 = jags.model(textConnection(mod6_string), data=data6_jags, n.chains=3)
update(mod6, 1000) # burn-in
mod6_sim = coda.samples(model=mod6,variable.names=params1, n.iter=10000)
mod6_csim = do.call(rbind, mod6_sim) # combine multiple chains
dic6 = dic.samples(mod6, n.iter=1e3) # 279.2


mod7_string = " model {
    for (i in 1:length(ACC)) {
        ACC[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*WHP2[i] + b[2]*SP[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:2) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

data7_jags = as.list(dat)
params1 = c("b0", "b", "sig")
mod7 = jags.model(textConnection(mod7_string), data=data7_jags, n.chains=3)
update(mod7, 1000) # burn-in
mod7_sim = coda.samples(model=mod7,variable.names=params1, n.iter=10000)
mod7_csim = do.call(rbind, mod7_sim) # combine multiple chains
dic7 = dic.samples(mod7, n.iter=1e3) # 280.1


mod8_string = " model {
    for (i in 1:length(ACC)) {
        ACC[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*WHP[i] + b[2]*WHP2[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:2) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

data8_jags = as.list(dat)
params1 = c("b0", "b", "sig")
mod8 = jags.model(textConnection(mod8_string), data=data8_jags, n.chains=3)
update(mod8, 1000) # burn-in
mod8_sim = coda.samples(model=mod8,variable.names=params1, n.iter=10000)
mod8_csim = do.call(rbind, mod8_sim) # combine multiple chains
dic8 = dic.samples(mod8, n.iter=1e3) # 281.9