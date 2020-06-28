# 3D - Numerical, Rejection Sampl., Importance Sampl.
library(mvtnorm)

f = function(mat) {
  return (exp(-mat[,1]^2-mat[,2]^2))
}

# Numerical
start.time <- Sys.time()

dx = dy = 0.2
x = seq(-5, 5, dx)
y = seq(-5, 5, dy)
msh = expand.grid(X=x, Y=y)
f.xy = f(msh)
sum(f.xy)*dx*dy # pi

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


# Rejection 
# Uniform(-5,5)
start.time <- Sys.time()

n = 100000
g.x = runif(n, -5, 5)
g.y = runif(n, -5, 5)
g.xy = matrix(c(g.x, g.y), nrow=length(g.x))
c = 10^2 # how much (the highest point of) f.x is bigger than g.x
t = f(g.xy)
u = runif(n)
indices = (u < t)
ratio = sum(indices)/length(indices) # ratio is very small...
ratio * c

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


# Normal
start.time <- Sys.time()

n = 50000
g.xy = rmvnorm(n, c(0,0))
c = (2*pi)
t = f(g.xy)/(c*dmvnorm(g.xy))
u = runif(n)
indices = (u < t)
ratio = sum(indices)/length(indices)
ratio * c

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


# Importance
start.time <- Sys.time()

g.xy = rmvnorm(n, c(0,0))
w = f(g.xy)/dmvnorm(g.xy)
mean(w)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


