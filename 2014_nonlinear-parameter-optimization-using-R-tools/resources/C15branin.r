## ----label=C15branin1, echo=TRUE-----------------------------------------
## branmin.R
myseed<-123456L # The user can use any seed. BUT ... some don't work so well.
branin<-function(x){ ## Branin's test function
     if (length(x) != 2) stop("Wrong dimensionality of parameter vector")
     x1<-x[1] # limited to [-5, 10]
     x2<-x[2] # limited to [0, 15]
     a <- 1
     b <- 5.1 / (4 * pi * pi)
     c <- 5 / pi
     d <- 6
     e <- 10 
     f <- 1/(8*pi)
     ## Global optima of 0.397887
     ## at (-pi, 12.275), (pi, 2.275), (9.42478, 2.475)
     val <- a*(x2 - b*(x1^2) + c*x1 - d)^2 + e*(1 - f)*cos(x1) + e
}
branin.g<-function(x){ ## Branin's test function
     if (length(x) != 2) stop("Wrong dimensionality of parameter vector")
     x1<-x[1]
     x2<-x[2]
     a <- 1
     b <- 5.1 / (4 * pi * pi)
     c <- 5 / pi
     d <- 6
     e <- 10 
     f <- 1/(8*pi)
     ## Global optima of 0.397887
     ## at (-pi, 12.275), (pi, 2.275), (9.42478, 2.475)
#     val <- a*(x2 - b*(x1^2) + c*x1 - d)^2 + e*(1 - f)*cos(x1) + e
     g1 <- 2*a*(x2 - b*(x1^2) + c*x1 - d)*(-2*b*x1+c)-e*(1-f)*sin(x1)
     g2 <- 2*a*(x2 - b*(x1^2) + c*x1 - d)
     gg<-c(g1, g2)
}


## ----label=C15branin2, echo=TRUE-----------------------------------------
dist2<-function(va, vb){
    n1<-length(va)
    n2<-length(vb)
    if (n1 != n2) stop("Mismatched vectors")
    dd<-0
    for (i in 1:n1){ dd<-dd+(va[i]-vb[i])^2 }
    dd
}
y1 <- c(-pi, 12.275)
y2 <- c(pi, 2.275)
y3 <- c(9.42478, 2.475)
lo<-c(-5, 0)
up<-c(10, 15)
npt<-40
grid1<-((1:npt)-1)*(up[1]-lo[1])/(npt-1)+lo[1]
grid2<-((1:npt)-1)*(up[2]-lo[2])/(npt-1)+lo[2]
pnts<-expand.grid(grid1,grid2)
names(pnts)=c("x1", "x2")
nrun<-dim(pnts)[1]


## ----label=C15branin3, echo=FALSE----------------------------------------
suppressPackageStartupMessages(require(Rvmmin))
reslt<-matrix(NA, nrow=nrun, ncol=7)
names(reslt)<-c("vmin","bx1","bx2", "sx1", "sx2", "sval")
ctrl<-list(maxit=20000)

cmat<-matrix(" ",nrow=npt, ncol=npt)
kk<-0 # for possible use as a progress counter
for (ii1 in 1:npt) { # row index is ii1 for x2 from grid2, plotted at ii=npt+1-ii1
    for (jj in 1:npt) { # col index is for x1 on graph
       ii<-npt+1-ii1
       strt<-c(grid1[jj], grid2[ii1]) 
       sval<-branin(strt)
       ans<-suppressWarnings(
            Rvmmin(strt, branin, branin.g, lower=lo, upper=up, control=ctrl)
       )
       kk<-kk+1
       reslt[kk,4]<-strt[1]
       reslt[kk,5]<-strt[2]
       reslt[kk,6]<-sval
       reslt[kk,1]<-ans$value
       reslt[kk,2]<-ans$par[1]
       reslt[kk,3]<-ans$par[2]
       reslt[kk,7]<-0
       if (ans$convergence !=0) { 
##     cat("Nonconvergence from start c(",strt[1],", ", strt[2],")\n")
           reslt[kk,1]<- .Machine$double.xmax 
       } else {
          if (dist2(ans$par, y1) < 1e-6) reslt[kk,7]=1
          if (dist2(ans$par, y2) < 1e-6) reslt[kk,7]=2
          if (dist2(ans$par, y3) < 1e-6) reslt[kk,7]=3
       }
       cmat[ii, jj]<-as.character(reslt[kk,7])
   } # end jj
} # end ii1
mpts<-kk
cat("Non convergence from the following starts:\n")
linetrip<-0
for (kk in 1:mpts){
   if (reslt[kk,7]==0) {
   cat("(",reslt[kk,4],", ",reslt[kk,5],")  ")
   linetrip<-linetrip+1
   if (4*floor(linetrip/4)==linetrip) cat("\n")
   }
}


## ----label=C15branin5, echo=TRUE-----------------------------------------
zz<-branin(pnts)
zz<-as.numeric(as.matrix(zz))
zy<-matrix(zz, nrow=40, ncol=40)
contour(grid1, grid2, zy, nlevels=25)
points(y1[1], y1[2], pch=19, cex=2)
points(y2[1], y2[2], pch=19, cex=2)
points(y3[1], y3[2], pch=19, cex=2)
title(main="Branin function contour plot")
title(sub="The solid circles are the three global minima")


## ----label=C15branin4, echo=TRUE, fig.height=7, fig.width=5--------------
for (ii in 1:npt){
    vrow<-paste(cmat[ii,],sep=" ", collapse=" ")
    cat(vrow,"\n")
}
