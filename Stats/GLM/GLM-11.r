library(GLMsData); data(nminer)
nm.m1 <- glm(Minerab ~ Eucs, data=nminer, family=poisson)
summary(nm.m1)  # all summary 
printCoefmat(coef(summary(nm.m1))) # only coefficients
confint(nm.m1) # Wald - confidence 

data(trees)
cherry.m1 <- glm(Volume ~ log(Height) + log(Girth), data=trees,
                  family=Gamma(link="log"))
summary(cherry.m1)$dispersion  # default
summary(cherry.m1)
