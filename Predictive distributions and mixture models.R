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
# plot(mod_sim)

# gelman.diag(mod_sim)
# autocorr.diag(mod_sim)
# autocorr.plot(mod_sim)
# effectiveSize(mod_sim)

## compute DIC
# dic = dic.samples(mod, n.iter=1e3)

x1 = c(29, 1)
loglam1 = mod_csim[,"b0"] + mod_csim[,c(1,2)] %*% x1
lam1 = exp(loglam1)
(n_sim = length(lam1))
y1 = rpois(n=n_sim, lambda=lam1*30)
plot(table(factor(y1, levels=0:18))/n_sim, pch=2, ylab="posterior prob.", xlab="calls")

# at least 3 times = P(n >= 3)
mean(y1 >= 3) # 0.22