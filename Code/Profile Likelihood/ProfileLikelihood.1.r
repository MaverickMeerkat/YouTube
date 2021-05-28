# lm example

x = seq(1, 10, 0.5)
y = 2.3*x + rnorm(19)
plot(x, y)
fit = lm(y ~ x)
summary(fit)
confint(fit)

loglik = function(b) {
  y.hat = fit$coefficients[1] + b*x
  loglik = 0
  for (i in 1:length(x)) {
    loglik = loglik + dnorm(y[i], y.hat[i], log=T)
  }
  return(loglik)
}

logLik(fit)
(v = loglik(fit$coefficients[2])-qchisq(0.975, 1)/2)

beta = seq(1.8, 2.8, 0.01)
ll = rep(0, length(beta))
for (i in 1:length(beta)){
  ll[i] = loglik(beta[i])
}
plot(beta, ll, ylim=c(-60, -10))
max(ll)

loglik(approx(ll, beta, xout=v)$y)
aprx = approx(ll, beta, xout=v)
ll.low = aprx$x; beta.low = aprx$y
points(beta.low, ll.low, col=2)
beta.MLE = fit$coefficients[2]
beta.hi = 2*beta.MLE - beta.low  
points(beta.hi, loglik(beta.hi), col=3)

pchisq(2*(loglik(beta.MLE)-loglik(beta.low)), 1)
pchisq(2*(loglik(beta.MLE)-loglik(beta.hi)), 1)

confint(fit) # works with normality (lm), not with reverse GLR
c(beta.low, beta.hi)


# glm example

x = seq(1, 10, 0.5)
mu = 2.3*x + rnorm(19)
y1 = rpois(19, mu)
y2 = rpois(19, mu)
y3 = rpois(19, mu)
y = c(y1, y2, y3)
x = c(x, x, x)
plot(x, y)

fit = glm(y ~ x, family=poisson(link="identity"))
fit$coefficients

confint(fit)

loglik = function(b) {
  func = function(a) {
    mu.hat = a + b*x
    ll = 0
    for (i in 1:length(x)) {
      ll = ll + dpois(y[i], mu.hat[i], log=T)
    }
    return(ll)
  }
  fit1 = optimize(func, c(-5, 5), maximum=T)
  return(fit1$objective)
}

logLik(fit)
loglik(fit$coefficients[2])

beta = seq(1.8, 2.9, 0.001)
ll = rep(0, length(beta))
for (i in 1:length(beta)){
  ll[i] = loglik(beta[i])
}
plot(beta, ll)
max(ll)


(ind = which(ll > logLik(fit)-qchisq(0.975, 1)/2)[1])
(beta.lo = beta[ind])
(beta.MLE = fit$coefficients[2])
(beta.hi = 2*beta.MLE - beta.lo)
confint(fit)

pchisq(2*(loglik(beta.MLE)-loglik(beta.lo)), 1)
pchisq(2*(loglik(beta.MLE)-loglik(beta.hi)), 1)

confint(fit)