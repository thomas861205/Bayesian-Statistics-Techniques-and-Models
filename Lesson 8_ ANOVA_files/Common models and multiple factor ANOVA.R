library("car")  # load the 'car' package
data("Anscombe")  # load the data set
set.seed(72)

library("rjags")

mod1_string = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
        ## Initial guess of variance based on overall
        ## variance of education variable. Uses low prior
        ## effective sample size. Technically, this is not
        ## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

data1_jags = as.list(Anscombe)
params1 = c("b0", "b", "sig")
inits1 = function() {
    inits = list("b"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}
mod1 = jags.model(textConnection(mod1_string), data=data1_jags, inits=inits1, n.chains=3)
update(mod1, 1000) # burn-in
mod1_sim = coda.samples(model=mod1,
                        variable.names=params1,
                        n.iter=5000)
mod1_csim = do.call(rbind, mod1_sim) # combine multiple chains

autocorr.diag(mod1_sim)
colMeans(mod1_csim)
   #       b[1]          b[2]          b[3]            b0           sig 
   # 0.07965776    0.81209123   -0.10514694 -282.14177191   27.35325459 






mod2_string = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]
    }
    
    for (i in 1:3) {
        b[i] ~ ddexp(0.0, 1.0)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
        ## Initial guess of variance based on overall
        ## variance of education variable. Uses low prior
        ## effective sample size. Technically, this is not
        ## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "


Xc = scale(Anscombe, center=TRUE, scale=TRUE)
str(Xc)
data2_jags = as.list(data.frame(Xc))

params2 = c("b", "sig")
inits2 = function() {
    inits = list("b"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}
mod2 = jags.model(textConnection(mod2_string), data=data2_jags, inits=inits2, n.chains=3)
update(mod2, 1000) # burn-in
mod2_sim = coda.samples(model=mod2,
                        variable.names=params2,
                        n.iter=5000)
mod2_csim = do.call(rbind, mod2_sim) # combine multiple chains

autocorr.diag(mod2_sim)
colMeans(mod2_csim)
 #        b[1]         b[2]         b[3]          sig 
 # 0.427165888  0.209816664 -0.001784868  5.593082924 
 plot(mod2_sim)








# clear all
# two-way

data("warpbreaks")
X = model.matrix( ~ wool + tension, data=warpbreaks)
head(X)

mod2_string = " model {
    for( i in 1:length(y)) {
        y[i] ~ dnorm(mu[i], prec)
        mu[i] = int + alpha*isWoolB[i] + beta[1]*isTensionM[i] + beta[2]*isTensionH[i]
    }
    
    int ~ dnorm(0.0, 1.0/1.0e6)
    alpha ~ dnorm(0.0, 1.0/1.0e6)
    for (j in 1:2) {
        beta[j] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(3/2.0, 3*1.0/2.0)
    sig = sqrt(1.0 / prec)
} "

data2_jags = list(y=log(warpbreaks$breaks), isWoolB=X[,"woolB"], isTensionM=X[,"tensionM"], isTensionH=X[,"tensionH"])

params2 = c("int", "alpha", "beta", "sig")

mod2 = jags.model(textConnection(mod2_string), data=data2_jags, n.chains=3)
update(mod2, 1e3)

mod2_sim = coda.samples(model=mod2,
                        variable.names=params2,
                        n.iter=5e3)

## convergene diagnostics
plot(mod2_sim)
# gelman.diag(mod2_sim)
# autocorr.diag(mod2_sim)
# effectiveSize(mod2_sim)
(dic2 = dic.samples(mod2, n.iter=1e3))


# full cell
mod3_string = " model {
    for( i in 1:length(y)) {
        y[i] ~ dnorm(mu[woolGrp[i], tensGrp[i]], prec)
    }
    
    for (j in 1:max(woolGrp)) {
        for (k in 1:max(tensGrp)) {
            mu[j,k] ~ dnorm(0.0, 1.0/1.0e6)
        }
    }
    
    prec ~ dgamma(3/2.0, 3*1.0/2.0)
    sig = sqrt(1.0 / prec)
} "

str(warpbreaks)

data3_jags = list(y=log(warpbreaks$breaks), woolGrp=as.numeric(warpbreaks$wool), tensGrp=as.numeric(warpbreaks$tension))

params3 = c("mu", "sig")

mod3 = jags.model(textConnection(mod3_string), data=data3_jags, n.chains=3)
update(mod3, 1e3)

mod3_sim = coda.samples(model=mod3,
                        variable.names=params3,
                        n.iter=5e3)
mod3_csim = as.mcmc(do.call(rbind, mod3_sim))

plot(mod3_sim, ask=TRUE)

## convergence diagnostics
# gelman.diag(mod3_sim)
# autocorr.diag(mod3_sim)
# effectiveSize(mod3_sim)
# raftery.diag(mod3_sim)
(dic3 = dic.samples(mod3, n.iter=1e3))


# full cell: seperate variance
mod4_string = " model {
    for( i in 1:length(y)) {
        y[i] ~ dnorm(mu[woolGrp[i], tensGrp[i]], prec[woolGrp[i], tensGrp[i]])
    }
    
    for (j in 1:max(woolGrp)) {
        for (k in 1:max(tensGrp)) {
            mu[j,k] ~ dnorm(0.0, 1.0/1.0e6)
            prec[j,k] ~ dgamma(1.0/2.0, 1.0/2.0)
        }
    }
    sig = sqrt(1.0 / prec)
} "

data4_jags = list(y=log(warpbreaks$breaks), woolGrp=as.numeric(warpbreaks$wool), tensGrp=as.numeric(warpbreaks$tension))

params4 = c("mu", "sig")

mod4 = jags.model(textConnection(mod4_string), data=data4_jags, n.chains=3)
update(mod4, 1e3)
mod4_sim = coda.samples(model=mod4,
                        variable.names=params4,
                        n.iter=5e3)
mod4_csim = as.mcmc(do.call(rbind, mod4_sim))

plot(mod4_sim, ask=TRUE)
## convergence diagnostics
# gelman.diag(mod4_sim)
# autocorr.diag(mod4_sim)
# effectiveSize(mod4_sim)
# raftery.diag(mod4_sim)
(dic4 = dic.samples(mod4, n.iter=1e3))