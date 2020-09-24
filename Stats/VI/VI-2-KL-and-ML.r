# Variational Inference 
# We want to minimize the KL divergence between two distributions
# By using Gradient Descent/Ascent

# Example: P(x) - Real ~ N(0,1)
x = rnorm(100) # observed values
# Q(x) - approximation ~ N(mu, sigma^2)
# start with randomized values
mu.0 = 5
sigma.sq.0 = 3
# KL(P||Q) = E[log P(x)] - E[log Q(x)]  (E w.r.t. P) we want to minimize this
# 1st term doesn't depend on parameters so we want to maximize 2nd term
# Gradient (w.r.t. mu and sigma^2) comes into the expectations
# Approximate E with sample average
# ( KL(Q||P) wouldn't work, as we don't have any way to approximate P(x) )
logQ = function(x, mu, sigma.sq) {
  -.5*log(2*pi)-.5*log(sigma.sq)-.5*(x-mu)^2/sigma.sq
}
dlogQ.dMu = function(x, mu, sigma.sq) {
  (x-mu)/sigma.sq
}
dlogQ.dS = function(x, mu, sigma.sq) {
  .5*((x-mu)/sigma.sq)^2-1/(2*sigma.sq)
}

# Gradient ascent
ls = 0.5 # learning step
mu.t = mu.0
sigma.sq.t = sigma.sq.0
tol = 1e-4
step.mu = step.sigma = 1
while((abs(step.mu) > tol) || (abs(step.sigma) > tol)) {
  step.mu = ls*mean(dlogQ.dMu(x, mu.t, sigma.sq.t))
  mu.t = mu.t + step.mu
  step.sigma = ls*mean(dlogQ.dS(x, mu.t, sigma.sq.t))
  sigma.sq.t = sigma.sq.t + step.sigma
}

curve(dnorm(x,mu.t,sqrt(sigma.sq.t)), from=-3, to=3)
curve(dnorm(x),from=-3, to=3, add=T, col=2)

# somewhat unsurprising, but the results are equal to the Maximum-Likelihood estimators
n = length(x)
c(mean(x), mu.t)
c(var(x)*(n-1)/n, sigma.sq.t)

# D. Refaeli