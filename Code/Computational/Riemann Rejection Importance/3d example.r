# 3D - Numerical, Rejection Sampl., Importance Sampl.
library(mvtnorm)

f = function(mat) {
  return (exp(-mat[,1]^2-mat[,2]^2-mat[,3]^2))
}


# Numerical
start.time <- Sys.time()

dx = dy = dz = 0.2
x = seq(-5, 5, dx)
y = seq(-5, 5, dy)
z = seq(-5, 5, dz)
msh = expand.grid(X=x, Y=y, Z=z)
f.xyz = f(msh)
sum(f.xyz)*dx*dy*dz # pi^(3/2)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

####

# Rejection 
# Uniform(-5,5)
start.time <- Sys.time()

n = 100000
g.x = runif(n, -5, 5)
g.y = runif(n, -5, 5)
g.z = runif(n, -5, 5)
g.xyz = matrix(c(g.x, g.y, g.z), nrow=length(g.x))
c = 10^3 # how much (the highest point of) f.x is bigger than g.x
t = f(g.xyz)
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
g.xyz = rmvnorm(n, c(0,0,0))
c = (2*pi)^(3/2)
t = f(g.xyz)/(c*dmvnorm(g.xyz))
u = runif(n)
indices = (u < t)
ratio = sum(indices)/length(indices)
ratio * c

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


# Importance
start.time <- Sys.time()

g.xyz = rmvnorm(n, c(0,0,0))
w = f(g.xyz)/dmvnorm(g.xyz)
mean(w)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
