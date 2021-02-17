mod_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dbin(phi[i], n[i])
        logit(phi[i]) = b0 + b[1]*Age[i] + b[2]*OMElow[i] + b[3]*Loud[i] + b[4]*Noiseincoherent[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/5.0^2)
    for (j in 1:4) {
        b[j] ~ dnorm(0.0, 1.0/4.0^2)
    }
    
} "

data_jags = as.list(as.data.frame(X))
data_jags$y = dat$Correct # this will not work if there are missing values in dat (because they would be ignored by model.matrix). Always make sure that the data are accurately pre-processed for JAGS.
data_jags$n = dat$Trials
str(data_jags) # make sure that all variables have the same number of observations (712).

params = c("b0", "b")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                        variable.names=params,
                        n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

raftery.diag(mod_sim)

# Prediction
(pm_coef = colMeans(mod_csim))
pm_Xb = pm_coef["b0"] + X %*% pm_coef[1:4]
phat = 1.0 / (1.0 + exp(-pm_Xb))
head(phat)

(tab0.7 = table(phat > 0.7, (dat$Correct / dat$Trials) > 0.7))
sum(diag(tab0.7)) / sum(tab0.7)

x = matrix(c(60, 0, 50, 0), nrow=1, ncol=4)
pred = 1.0 / (1.0 + exp(-(pm_coef["b0"] + x %*% pm_coef[1:4])))
head(pred)

