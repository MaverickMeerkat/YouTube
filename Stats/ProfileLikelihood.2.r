# Profile Likelihood GLM example
# We are testing the confidence-interval on the slope (b)

# Generate the data
rpt = 4
x = rep(seq(1, 10, 0.5), times=rpt)
mu = 2.3*x # mu = a + b*x, a=intercept, b=slope
y = rpois(length(mu), mu)
plot(x, y)

fit = glm(y ~ x, family=poisson(link="identity"))
fit$coefficients

# Profile likelihood Function
loglik = function(b) {
  fit1 = glm(y ~ 1, offset=b*x, family=poisson(link="identity"))
  return(logLik(fit1))
}

logLik(fit) # built-in R function that computes the log-likelihood
loglik(fit$coefficients[2]) # our profile-likelihood should be equal for the b_MLE

beta = seq(fit$coefficients[2]-0.4, fit$coefficients[2]+0.4, 0.0001)
ll = rep(0, length(beta))
for (i in 1:length(beta)){
  ll[i] = loglik(beta[i])
}
plot(beta, ll)

# Now that we have the log-likelihood as a function of our parameter of interest, we can 
# invert the Likelihood-Ratio test to calculate a Confidence Interval for it.
wch = which(ll > logLik(fit)-qchisq(0.95, 1)/2)
(ind.lo = wch[1])
(beta.lo = beta[ind.lo])
(ind.hi = wch[length(wch)])
(beta.hi = beta[ind.hi])

confint(fit) # built in R function, that calculates Profile Likelihood CI for a glm fit
c(beta.lo, beta.hi) # our manually calculated  Profile Likelihood CI
# Add to the plot
points(c(beta.lo, beta.hi), c(ll[ind.lo], ll[ind.hi]), col=2, pch=19)
lines(c(beta.lo, beta.hi), c(ll[ind.lo], ll[ind.hi]), col=3)
text(fit$coefficients[2], loglik(beta.lo)+.2, "95% CI")

# Sanity check - see if the Likelihood Ratio test give 95% confidence for our values
(beta.MLE = fit$coefficients[2])
pchisq(2*(loglik(beta.MLE)-loglik(beta.lo)), 1)
pchisq(2*(loglik(beta.MLE)-loglik(beta.hi)), 1)

# D. Refaeli