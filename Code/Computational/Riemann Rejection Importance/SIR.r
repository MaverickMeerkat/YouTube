# Sampling Importance Resampling - SIR

# f(x) - desired (unnormalized) distribution - 5*chi-square(df=5)
# g(x) - proposed distribution - exponential
n = 1000000
x.exp.sample = rexp(n)
par(mfrow=c(1,1))
hist(x.exp.sample)

f.x.unnormalized.pdf = 5*dchisq(x.exp.sample, df=5)
g.x.pdf = dexp(x.exp.sample)
weights = f.x.unnormalized.pdf/g.x.pdf
weights.norm = weights/sum(weights)

f.x.sample = sample(x.exp.sample, size = n, replace = TRUE, prob = weights.norm)

par(mfrow=c(1,2))
x = rchisq(n, df=5)
hist(x[x<15], breaks = seq(0,15,1), main="Chi-Square")
hist(f.x.sample, breaks = seq(0,15,1), main="SIR")

# moving from f1(x) to f2(x)
f.1 = dchisq(f.x.sample, df=5)
f.2 = df(f.x.sample, df1=5, df2=25)
new.weights = f.2/f.1
mean(new.weights) # should be ~ 1
new.weights.norm = new.weights/sum(new.weights)

f2.x.sample = sample(f.x.sample, size = n, replace = TRUE, prob = new.weights.norm)

par(mfrow=c(1,2))
x = rf(n, 5, 25)
hist(x[x<15], breaks = seq(0,15,1), main="F(5,25)")
hist(f2.x.sample, breaks = seq(0,15,1), main="SIR")

