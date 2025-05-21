#3.5
Crabs <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Crabs.dat",
                     + header=TRUE)
Crabs

#3.6
y <- c(5,18,19,25,7,7,2); n <- c(6,21,20,36,17,18,3)
x <- c(1,2,3,4,5,6,7)
fit <- glm(y/n ~ x, family=binomial(link=logit), weights=n)
summary(fit)

#3.7