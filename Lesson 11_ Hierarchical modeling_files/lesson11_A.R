# hierarchical
library("rjags")

mod_string = " model {
    for (i in 1:length(y)) {
      y[i] ~ dnorm(theta[grp[i]], prec)
    }

    for (j in 1:max(grp)) {
      theta[j] ~ dnorm(mu, prec2)
    }

    mu ~ dnorm(0, 1.0/1.0e6)
    prec2 ~ dgamma(1.0/2.0, 1.0*3.0/2.0)
    sig22 = 1.0 / prec2
    prec ~ dgamma(2.0/2.0, 2.0*1.0/2.0)
    sig2 = 1.0 / prec
} "

set.seed(113)

data_jags = as.list(dat)

params = c("theta")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))
(means_theta = colMeans(mod_csim)) # posterior mean
#  theta[1]   theta[2]   theta[3]   theta[4]   theta[5] 
# 0.9045009 -1.4862418 -1.0997279  0.2254067 -0.3688453 


means_anova = tapply(dat$y, INDEX=dat$grp, FUN=mean)
## dat is the data read from pctgrowth.csv
#         1          2          3          4          5 
# 0.9900000 -1.6166667 -1.1625000  0.2533333 -0.3714286 
plot(means_anova)
## where means_theta are the posterior point estimates for the industry means.
points(means_theta, col="red")