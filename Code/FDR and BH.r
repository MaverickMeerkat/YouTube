# P.Values
n = 10000
X = rnorm(n)
# x_i \sim N(mu, 1)
# H0: mu = 0
# H1: mu > 0 
pval1 = pnorm(X, lower.tail = F)
hist(pval1, breaks=20, prob=T, main="1-way p. value")
lines(seq(0,1,1), rep(1,2), col=2)

# H0: mu = 0
# H1: mu != 0 
pval2 = 2*pmin(pnorm(X, lower.tail = F), pnorm(X))
hist(pval2, prob=T, main="2-way p. value")
lines(seq(0,1,1), rep(1,2), col=2)

# When mu is actually 2
X1 = rnorm(n, 2)
pval1.1 = pnorm(X1, lower.tail = F)
hist(pval1.1, breaks=20, prob=T, main="1-way p. value")
lines(seq(0,1,1), rep(1,2), col=2)

Xhalf = c(X[1:(n/2)], X1[1:(n/2)])
pval.half = pnorm(Xhalf, lower.tail = F)

# Bonferonni
rej = sum(pval.half<0.05/n) # rejected p.values
rej.true = sum(pval.half[(n/2):n]<0.05/n) # rejected p.values from actual H1
rej/(n/2)  # % true discoveries = power
(rej-rej.true)/rej # false positive

# BH
pBH = p.adjust(pval.half,method="BH")
rej = sum(pBH<0.05) # rejected p.values
rej.true = sum(pBH[(n/2):n]<0.05) # rejected p.values from actual H1
rej/(n/2)  # % true discoveries = power
(rej-rej.true)/rej # false positive
# FDR <= m0/m * q

# Draw
plot(1:n, sort(pval.half))
lines(1:n, (1:n)*0.05/n, col='red')

# Reduced graph
t = sort(c(pval1[1:5], pval1.1[1:5]))
plot(1:10, t)
t<(1:10)*0.05/length(t)
p.adjust(t,method="BH")
lines(1:10, (1:10)*0.05/10, col='red')

t2 = c(0.006, 0.007, 0.008, 0.01, 0.012, 0.02, 0.03, 0.032, 0.04, 0.041, 0.05, 0.06)
p.adjust(t2, method="BH") 
plot(1:12, t2)
lines(1:12, (1:12)*0.05/12, col='red')

t3 = c(0.007, 0.008, 0.01, 0.012, 0.02, 0.03, 0.032, 0.04, 0.041, 0.05, 0.06)
p.adjust(t3, method="BH") 
plot(1:11, t3)
lines(1:11, (1:11)*0.05/11, col='red')

t4 = c(0.006, 0.007, 0.008, 0.01, 0.012, 0.02, 0.03, 0.032, 0.04, 0.041, 0.05)
p.adjust(t4, method="BH") 
plot(1:11, t4)
lines(1:11, (1:11)*0.05/11, col='red')


# D. Refaeli