# C07poissonreg1.R

source("C07poissonregdata.R", echo=FALSE) # set up data

## Using dfsane from BB
aBB <- dfsane(par=rep(0,8), fn=U.eqn, control=list(NM=TRUE, M=100, trace=FALSE), Y=Y, X=X, obs.period=obs.p)
aBB
# "glm" gives same results as solving the estimating equations
ans.glm <- glm(Y ~ X[,-1], offset=log(obs.p), family=poisson(link="log"))
ans.glm
