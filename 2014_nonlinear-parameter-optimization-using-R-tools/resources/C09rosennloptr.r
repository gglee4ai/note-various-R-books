## ----label=C09nlow1, echo=TRUE-------------------------------------------
require(nloptwrap)
nowp <- function(answer){
# nloptwrap answer summary
cat("Fn =",answer$value," after ",answer$iter," iterations, parameters:\n")
print(answer$par)
cat(answer$message,"\n")
invisible(0)
}

albfgs <- lbfgs(sstart, fr, gr=frg)
nowp(albfgs)
atnewton <- tnewton(sstart, fr, gr=frg)
nowp(atnewton)
avarmetric <- varmetric(sstart, fr, gr=frg)
nowp(avarmetric)
anelmead <- neldermead(sstart, fr)
nowp(anelmead)
anewuoa <- newuoa(sstart, fr)
nowp(anewuoa)
# remove the packages in case of confusion
detach(package:nloptwrap)
detach(package:nloptr)
