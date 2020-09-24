# KL (continous) function, p and q should have a log flag as 2nd parameter
KL = function(p, q, low=-Inf, up=Inf){
  f = function(x) p(x)*(p(x, T)-q(x, T))
  integrate(f, lower=low, upper=up)
}

# KL between two normals
curve(dnorm(x), from=-9, to=11)
curve(dnorm(x, 2), from=-9, to=11, add=T, col=2)
p = function(x, lg=F) dnorm(x, log=lg)
q = function(x, lg=F) dnorm(x, 2, log=lg)
KL(p, q)
KL(q, p)

# KL between Chi-square 1-df & Exp(1)
curve(dchisq(x, 1), from=0, to=10)
curve(dexp(x), from=0, to=10, add=T, col=2)
p = function(x, lg=F) dchisq(x, 1, log=lg)
q = function(x, lg=F) dexp(x, log=lg)
KL(p, q, 0)
KL(q, p, 0)

# KL between normal & t
curve(dnorm(x), from=-10, to=10)
curve(dt(x,1), from=-10, to=10, add=T, col=2)
p = function(x, lg=F) dnorm(x, log=lg)
q = function(x, lg=F) dt(x, 1, log=lg)
KL(p, q)
KL(q, p) # divergent! i.e. KL = infinity

# normal vs. t in tails
curve(dnorm(x), from=10, to=12, ylim=c(0, 0.0035))
curve(dt(x,1), from=10, to=12, add=T, col=2)
dt(10,1)/dnorm(10) # 10^19 more likely !!!

# KL (discrete) function, p and q should have a log flag as 2nd parameter
KL_dsc = function(p, q){
  f = function(x) p(x)*(p(x, T)-q(x, T))
  l = 0:2^20 # big enough, but not too much to blow my memory
  sum(f(l))
}
p = function(x, lg=F) dpois(x, 1, log=lg)
q = function(x, lg=F) dpois(x, 2, log=lg)
KL_dsc(p, q)
1*log(1/2)+2-1
KL_dsc(q, p)
2*log(2/1)+1-2
# KL(p, q) = lambd_P*log(lambd_P/lambd_Q) + lambd_Q - lambd_P 


# D. Refaeli