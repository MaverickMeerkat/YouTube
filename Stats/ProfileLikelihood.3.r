# Simpler Examples of Profile Likelihood

## 1 - Create CI for MLE of Normal Distribution

# Draw from standard normal
n = 100
x = rnorm(n)
mu.MLE = mean(x)
sigma.MLE = sqrt(var(x)*(n-1)/n)

# log Likelihood function
logL = function(theta) {
  mu = theta[1]
  sigma = theta[2]
  sum(dnorm(x, mu, sigma, log=T))
}

# Contour plot of the log likelihood as function of the mu-sigma plane
mus = seq(-2, 2, 0.01)
sigmas = seq(0.5, 2, 0.01)
xy = expand.grid(mus, sigmas)
z = apply(xy, 1, logL)
dim(z) <- c(401, 151)
contour(mus, sigmas, z, nlevels=300)

# 3d Surface plot
library(plotly)
zz <- t(z)
fig <- plot_ly(x=~mus, y=~sigmas, z =~zz)
fig <- fig %>% add_surface()
fig <- fig %>% layout(
  title = "Log Likelihood on the Mu-Sigma plane",
  scene = list(
    xaxis = list(title = "Mu"),
    yaxis = list(title = "Sigma"),
    zaxis = list(title = "Log Likelihood")
  ))
fig

# Profile Likelihood function for Mu
ProfLik.Mu = function(mu) {
  sigma.mle = sqrt(sum((x-mu)^2)/n)
  logL(c(mu, sigma.mle))
}
l.Mu = lapply(mus, ProfLik.Mu)
plot(mus, l.Mu, type="l")

# Compute (profile likelihood) confidence interval for Mu
wch = which(l.Mu > logL(c(mu.MLE, sigma.MLE))-qchisq(0.95, 1)/2)
(ind.lo = wch[1])
(mu.lo = mus[ind.lo])
(ind.hi = wch[length(wch)])
(mu.hi = mus[ind.hi])
c(mu.lo, mu.hi)
# compared with regular MLE condifdence interval
mean(x) - qt(0.975, n-1) * sqrt(var(x)/n)
mean(x) + qt(0.975, n-1) * sqrt(var(x)/n)
# fit = lm(x ~ 1)
# confint(fit)

# Profile Likelihood function for Sigma
ProfLik.Sig = function(sig) {
  mu = mean(x)
  logL(c(mu, sig))
}
l.Sig = lapply(sigmas, ProfLik.Sig)
plot(sigmas, l.Sig, type="l")

# Compute (profile likelihood) confidence interval for Sigma
wch = which(l.Sig > logL(c(mu.MLE, sigma.MLE))-qchisq(0.95, 1)/2)
(ind.lo = wch[1])
(sigmas.lo = sigmas[ind.lo])
(ind.hi = wch[length(wch)])
(sigmas.hi = sigmas[ind.hi])
c(sigmas.lo, sigmas.hi)
# compared with regular MLE condifdence interval
c(sqrt((n-1)*var(x)/qchisq(0.975, n-1)), sqrt((n-1)*var(x)/qchisq(0.025, n-1)))


## 2 - Create CI for Normal Regression

# Create regression data and fit to it
x = seq(3, 8, 0.1)
n = length(x)
u = rnorm(n)
a = 3 # intercept
b = 1.5 # slope
y = a + b*x + u
plot(x,y)
fit = lm(y~x)

ProfLik.a = function(a){
  fit = lm(y ~ 0 + x, offset=rep(a, n))
  logLik(fit)
}
ProfLik.a(fit$coefficients[1]) # should be equal to logLik(fit)

as = seq(2,4.5, 0.01)
l.a = lapply(as, ProfLik.a)
plot(as, l.a, type="l")

ProfLik.b = function(b) {
  fit = lm(y ~ 1, offset=b*x)
  logLik(fit)
}
ProfLik.b(fit$coefficients[2]) # should be equal to logLik(fit)

bs = seq(0.5, 2.5, 0.01)
l.b = lapply(bs, ProfLik.b)
plot(bs, l.b, type="l")

# Compute (profile likelihood) confidence interval for a (intercept)
wch = which(l.a > logLik(fit)-qchisq(0.95, 1)/2)
(ind.lo = wch[1])
(a.lo = as[ind.lo])
(ind.hi = wch[length(wch)])
(a.hi = as[ind.hi])
c(a.lo, a.hi)

# Compute (profile likelihood) confidence interval for b (slope)
wch = which(l.b > logLik(fit)-qchisq(0.95, 1)/2)
(ind.lo = wch[1])
(b.lo = bs[ind.lo])
(ind.hi = wch[length(wch)])
(b.hi = bs[ind.hi])
c(b.lo, b.hi)

confint(fit) # gives "Wald" intervals for coefficients

