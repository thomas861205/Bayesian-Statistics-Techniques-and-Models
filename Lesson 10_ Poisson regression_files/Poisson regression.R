library("COUNT")

data("badhealth")
?badhealth
head(badhealth)

hist(badhealth$numvisit, breaks=20)
plot(jitter(log(numvisit)) ~ jitter(age), data=badhealth, subset=badh==0, xlab="age", ylab="log(visits)")
points(jitter(log(numvisit)) ~ jitter(age), data=badhealth, subset=badh==1, col="red")

library("rjags")

mod_string = " model {
    for (i in 1:length(numvisit)) {
        numvisit[i] ~ dpois(lam[i])
        log(lam[i]) = int + b_badh*badh[i] + b_age*age[i] + b_intx*age[i]*badh[i]
    }
    
    int ~ dnorm(0.0, 1.0/1e6)
    b_badh ~ dnorm(0.0, 1.0/1e4)
    b_age ~ dnorm(0.0, 1.0/1e4)
    b_intx ~ dnorm(0.0, 1.0/1e4)
} "

set.seed(102)

data_jags = as.list(badhealth)

params = c("int", "b_badh", "b_age", "b_intx")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                        variable.names=params,
                        n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

## convergence diagnostics
plot(mod_sim)

# gelman.diag(mod_sim)
# autocorr.diag(mod_sim)
# autocorr.plot(mod_sim)
# effectiveSize(mod_sim)

## compute DIC
dic = dic.samples(mod, n.iter=1e3) # 5633


### Additive Model

mod1_string = " model {
    for (i in 1:length(numvisit)) {
        numvisit[i] ~ dpois(lam[i])
        log(lam[i]) = int + b_badh*badh[i] + b_age*age[i]
    }
    
    int ~ dnorm(0.0, 1.0/1e6)
    b_badh ~ dnorm(0.0, 1.0/1e4)
    b_age ~ dnorm(0.0, 1.0/1e4)
} "

data_jags = as.list(badhealth)

params1 = c("int", "b_badh", "b_age")

mod1 = jags.model(textConnection(mod1_string), data=data_jags, n.chains=3)
update(mod1, 1e3)

mod1_sim = coda.samples(model=mod1,
                        variable.names=params1,
                        n.iter=5e3)
mod1_csim = as.mcmc(do.call(rbind, mod1_sim))

## convergence diagnostics
# plot(mod1_sim)

# gelman.diag(mod1_sim)
# autocorr.diag(mod1_sim)
# autocorr.plot(mod1_sim)
# effectiveSize(mod1_sim)

## compute DIC
dic1 = dic.samples(mod1, n.iter=1e3) # 5638






library("rjags")

mod_string = " model {
    for (i in 1:length(calls)) {
        calls[i] ~ dpois( days_active[i] * lam[i] )
        log(lam[i]) = b0 + b[1]*age[i] + b[2]*isgroup2[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1e2)
    b[1] ~ dnorm(0.0, 1.0/1e2)
    b[2] ~ dnorm(0.0, 1.0/1e2)
} "

set.seed(102)

data_jags = as.list(dat)

params = c("b0", "b")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                        variable.names=params,
                        n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

## convergence diagnostics
plot(mod_sim)

# gelman.diag(mod_sim)
# autocorr.diag(mod_sim)
# autocorr.plot(mod_sim)
# effectiveSize(mod_sim)

## compute DIC
dic = dic.samples(mod, n.iter=1e3) # 5633