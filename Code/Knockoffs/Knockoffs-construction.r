set.seed(1234)

# Problem parameters
n = 1000          # number of observations
p = 300           # number of variables
k = 30            # number of variables with nonzero coefficients
amplitude = 4.5   # signal amplitude (for noise level = 1)

# Generate the variables from a multivariate normal distribution
mu = rep(0,p)
rho = 0.25
Sigma = toeplitz(rho^(0:(p-1)))  # AR correlation
X = matrix(rnorm(n*p),n) %*% chol(Sigma)
# ^ this is just a way to create X from N(0, Sigma)
# Remember X ~ N(0, I), X*(Sigma^0.5) ~ N(0, Sigma)

library(knockoff)
X.KO.eq = create.fixed(X, method='equi')
X.KO.eq$X[1:5,1]
X[1:5,1]/norm(X[,1], type="2")
X.KO.eq$Xk[1:5,1]

# https://github.com/msesia/knockoff-filter/blob/master/R/knockoff/R/create_fixed.R
create_equicorrelated <- function(X, randomize) {
  # Compute SVD and U_perp.
  X.svd = decompose(X, randomize)
  
  # Set s = min(2 * smallest eigenvalue of X'X, 1), so that all the correlations
  # have the same value 1-s.
  if (any(X.svd$d <= 1e-5 * max(X.svd$d)))
    stop(paste('Data matrix is rank deficient.',
               'Equicorrelated knockoffs will have no power.'))
  lambda_min = min(X.svd$d)^2
  s = min(2*lambda_min, 1)
  
  # Construct the knockoff according to Equation 1.4.
  s_diff = pmax(0, 2*s - (s/X.svd$d)^2) # can be negative due to numerical error
  X_ko = (X.svd$u %*diag% (X.svd$d - s / X.svd$d) +
            X.svd$u_perp %*diag% sqrt(s_diff)) %*% t(X.svd$v)
}

decompose <- function(X, randomize) {
  n = nrow(X); p = ncol(X)
  stopifnot(n >= 2*p)
  
  result = canonical_svd(X)
  Q = qr.Q(qr(cbind(result$u, matrix(0,n,p))))
  u_perp = Q[,(p+1):(2*p)]
  if (randomize) {
    Q = qr.Q(qr(rnorm_matrix(p,p)))
    u_perp = u_perp %*% Q
  }
  result$u_perp = u_perp
  result
}

X.KO.sdp = create.fixed(X, method='sdp')


create_sdp <- function(X, randomize) {
  # Compute SVD and U_perp.
  X.svd = decompose(X, randomize)
  
  # Check for rank deficiency.
  tol = 1e-5
  d = X.svd$d
  d_inv = 1 / d
  d_zeros = d <= tol*max(d)
  if (any(d_zeros)) {
    warning(paste('Data matrix is rank deficient.',
                  'Model is not identifiable, but proceeding with SDP knockoffs'),immediate.=T)
    d_inv[d_zeros] = 0
  }
  
  # Compute the Gram matrix and its (pseudo)inverse.
  G = (X.svd$v %*diag% d^2) %*% t(X.svd$v)
  G_inv = (X.svd$v %*diag% d_inv^2) %*% t(X.svd$v)
  
  # Optimize the parameter s of Equation 1.3 using SDP.
  s = create.solve_sdp(G)
  s[s <= tol] = 0
  
  # Construct the knockoff according to Equation 1.4:
  C.svd = canonical_svd(2*diag(s) - (s %diag*% G_inv %*diag% s))
  X_ko = X - (X %*% G_inv %*diag% s) + 
    (X.svd$u_perp %*diag% sqrt(pmax(0, C.svd$d))) %*% t(C.svd$v)
}

# D. Refaeli