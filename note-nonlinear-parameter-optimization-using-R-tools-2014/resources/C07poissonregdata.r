#  C07poissonregdata.R

# Poisson estimating equation with "offset"
U.eqn <- function(beta, Y, X, obs.period) {
   Xb <- c(X %*% beta)
   as.vector(crossprod(X, Y - (obs.period * exp(Xb)))) # changed 130129
}
poisson.sim <- function(beta, X, obs.period) {
  Xb <- c(X %*% beta)
  mean <- exp(Xb) * obs.period
  rpois(nrow(X), lambda=mean)
}
require(BB)
require(setRNG)
# this RNG setting can be used to reproduce results
test.rng <- list(kind="Mersenne-Twister", normal.kind="Inversion", seed=1234)
old.seed <- setRNG(test.rng)
n <- 500
X <- matrix(NA, n, 8)
X[,1] <- rep(1, n)
X[,3] <- rbinom(n, 1, prob=0.5)
X[,5] <- rbinom(n, 1, prob=0.4)
X[,7] <- rbinom(n, 1, prob=0.4)
X[,8] <- rbinom(n, 1, prob=0.2)
X[,2] <- rexp(n, rate=1/10)
X[,4] <- rexp(n, rate=1/10)
X[,6] <- rnorm(n, mean=10, sd=2)
obs.p <- rnorm(n, mean=100, sd=30) # observation period
beta <- c(-5, 0.04, 0.3, 0.05, 0.3, -0.005, 0.1, -0.4)
Y <- poisson.sim(beta, X, obs.p)
