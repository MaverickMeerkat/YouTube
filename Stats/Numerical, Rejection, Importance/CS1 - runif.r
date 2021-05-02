##############################################################################
# Computational Statistics - 1 - Random Sampling, Uniform, Inverse Transform #
##############################################################################

# Random numbers
runif(1) 
rexp(1)
rnorm(1)
rpois(1, lambda=5)

## Middle Square Algorithm
library(sets)
# seed
x = 1234
already_seen = set()
counter = 0
n = nchar(as.character(x))
while(!(x %in% already_seen)){
  counter = counter + 1
  already_seen = set_union(already_seen, set(x))
  print(c(counter, x))
  new = as.character(x*x)
  n2 = nchar(new)
  add = 2*n-n2
  for (i in 1:add){
    new = paste0('0', new)
  }
  x = as.integer(substr(new, as.integer(n/2)+1, 2*n-as.integer(n/2)))
}

# What R uses? Mersenne-Twister
RNGkind() # Uniform - Normal - Sampling
?RNG
.Random.seed # vector of 2 numbers + 624 random numbers used by the MT algorithm
.Random.seed[1:10]
set.seed(3)
.Random.seed[1:10]
runif(14)
runif(624-.Random.seed[2]+1)
.Random.seed[1:10]

# reproduce
set.seed(3)
runif(1)
set.seed(3)
runif(1)

# Use current time vs. Restore RData?
runif(1)

# Inverse Transform 
n = 1000000
U = runif(n)

# Exponential, lambda = 5
lambda = 5
X = -log(1-U)/lambda
dexp5 = function(x) {dexp(x, rate=5)}

hist(X, freq=F)
lines(density(X), col='red')
curve(dexp5, from=0, to=2.5, add=T, col='blue')

# Bernoulli, p=0.3
p = 0.3
X = 1*(U < p)
dd = c(0, 1)
pp = c(1-sum(X)/n, sum(X)/n)
plot(dd, pp, type='h', col=2, ylim=c(0,1))
points(dd,pp,col=2);abline(h=0,col=3)

# © D. Refaeli

