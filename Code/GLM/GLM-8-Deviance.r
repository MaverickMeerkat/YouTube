# Generate the data
rpt = 4
x = rep(seq(1, 10, 0.5), times=rpt)
mu = 2.3*x # + rnorm(length(x), sd=0.5)
y = rpois(length(mu), mu)
plot(x, y)

fit = glm(y ~ x, family=poisson(link="identity"))
summary(fit)