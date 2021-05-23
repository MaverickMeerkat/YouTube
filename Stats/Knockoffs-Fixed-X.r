# Examples taken from:
# https://web.stanford.edu/group/candes/knockoffs/software/knockoffs/

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

# Generate the response from a linear model
nonzero = sample(p, k)
beta = amplitude * (1:p %in% nonzero) / sqrt(n)
y.sample = function(X) X %*% beta + rnorm(n)
y = y.sample(X)

# Fixed-X Knockoffs
library(knockoff)
result = knockoff.filter(X, y, knockoffs=create.fixed, statistic=stat.glmnet_lambdasmax)
result
length(result$selected)

# Power and FDR
TP = length(intersect(result$selected, nonzero)); TP
Power = TP/k; Power
FDR = (length(result$selected)-TP)/length(result$selected); FDR

# Compare to BH
model = lm(y~0+X)
summary(model)
pvalues = unname(summary(model)$coefficients[,4])
BHq = p.adjust(pvalues, method="BY") # vs. BY
result.BH = which(BHq<0.1)

TP = length(intersect(result.BH, nonzero)); TP
Power = TP/k; Power
FDR = (length(result.BH)-TP)/length(result.BH); FDR

# D. Refaeli