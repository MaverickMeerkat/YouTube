# Laplace's Approximation
library(LearnBayes)

# Example 1 - good fit
f.x = function(x) {
  (4-x^2)*exp(-x^2)
}

log.f.x = function(x) {
  log(f.x(x))
}

fit = laplace(log.f.x, 1)
(mode = fit$mode)
(const = f.x(mode))
(var = fit$var[1])
(nc = exp(fit$int))
const*sqrt(2*pi*var)
# real integral ~= 6.20358 (Wolfram-Alpha)

# Example 2 - not so good fit
curve(dgamma(x, 2, 1), from=-3, to=10, ylim=c(0, 0.4), ylab="pdf")
log.f.x = function(x) {
  dgamma(x, 2, 1, log=T)
}
fit = laplace(log.f.x, 2)
(mode = fit$mode)
(const = f.x(mode))
(var = fit$var[1])
(nc = exp(fit$int))
curve(dnorm(x, mode, sqrt(var)), from=-3, to=10, add=T, col=2)

# Example 3 - horrible fit
f.x = function(x) {
  10*exp(-10*x^2)*sin(x^2)
}
log.f.x = function(x) {
  log(f.x(x))
}
fit = laplace(log.f.x, 0.5)
exp(fit$int)
# real integral ~= 0.2785 (Wolfram-Alpha)