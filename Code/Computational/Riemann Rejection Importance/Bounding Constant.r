# 1D
f = function(x) {
  return (dnorm(x))
}

g = function(x) {
  return (1/10)
}

func = function(x) {
  return (log(f(x)) - log(g(x)))
}

bounding = function (func, interval) {
  fit = optimize(func, interval, maximum=TRUE)
  return (exp(fit$objective))
}

c = bounding(func, interval = c(-10, 10)); c
exp(-log(sqrt(2*pi))-log(0.1)) # analytically


# 2D
library(mvtnorm)

f = function(x) {
  return (dmvnorm(x))
}

g = function(x) {
  return (1/100)
}

func = function(x) {
  return (log(f(x)) - log(g(x)))
}

bounding = function (func, initial) {
  fit = optim(initial, func, control = list(fnscale=-1)) # control needed for max instead of min
  return (exp(fit$value))
}

bounding(func, c(5,3))


# David Refaeli