# Examples taken from:
# https://web.stanford.edu/group/candes/knockoffs/software/knockoffs/

library(knockoff)
result = ?knockoff.filter(X, y, knockoffs=create.fixed, statistic=stat.glmnet_lambdasmax)
create.fixed
create_equicorrelated
create_sdp

decompose <- function(X, randomize) {
  n = nrow(X); p = ncol(X)
  stopifnot(n >= 2*p)
  
  result = svd(X)
  Q = qr.Q(qr(cbind(result$u, matrix(0,n,p))))
  u_perp = Q[,(p+1):(2*p)]
  if (randomize) {
    Q = qr.Q(qr(rnorm_matrix(p,p)))
    u_perp = u_perp %*% Q
  }
  result$u_perp = u_perp
  result
}

decompose(x, F)
svd(x)

?qr.Q()
x = matrix(1:6, ncol=2)
A = cbind(x, matrix(0,3,1))
qr.Q(qr(A))
t(tt)%*%tt
Q = tt[,3:4]
t(Q)%*%Q

tt
qr.Q(qr(x))
qr.R(qr(cbind(x, matrix(0,4,2))))

y = A[,1]
v = norm(y,type="2")*c(1,0,0)-y
F1 = diag(3)-2*drop(1/(t(v)%*%v))*(v%*%t(v))

y = (F1%*%A)[2:3,2]
v = norm(y,type="2")*c(1,0)-y
Ft = diag(2)-2*drop(1/(t(v)%*%v))*(v%*%t(v))
F2 = matrix(c(1,0,0,0,Ft[,1],0,Ft[,2]), ncol=3)

Ft%*%y
F2%*%F1%*%A


F1[,2]%*%F1[,3]
