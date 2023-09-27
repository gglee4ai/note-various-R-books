# C10aderiv1.R

mod <- ~100*b1/(1+10*b2*exp(-0.1*b3*t))
mod
namev<-c("b1", "b2", "b3")
try1<-deriv(mod, namev)
try1
