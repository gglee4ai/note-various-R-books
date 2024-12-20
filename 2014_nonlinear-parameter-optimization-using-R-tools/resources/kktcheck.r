require(numDeriv)
kktcheck <- function(ans, ufn, ugr=NULL, ...) {
## This version for unconstrained problems and minimization -- need to adjust for others.

#  A wrapper function to integrate major optimization packages in R
#  For now, we only consider algorithms for unconstrained and box-constrained optimization
#  This function can implement "multiple" algorithms
#
# Input:
#  ans = a single vector of starting values
#  ufn = user objective function (assumed to be sufficeintly differentiable) for MINIMIZATION ??
#  ugr = name of a function to compute the (analytic) gradient of the objective function
#
#  Author:  John Nash
#  Date:  February 22, 2010
#
#################################################################

## For this routine put in values for controls -- may want this elsewhere ??
trace <- TRUE # ?? change later poss
kkttol <- 10*.Machine$double.eps^(1/4)
kkt2tol<- 100* (.Machine$double.eps^(1/4))
if (trace) cat("KKT test tolerances: for g = ",kkttol,"   for H = ",kkt2tol,"\n")
if (trace) {
	cat("Parameters: ")
	print(ans$par)
}

opt2vec <- function(method, optans, time) {
## Displays optim() answers nicely
	outvec<-c(meth=method, fval=optans$value,
		     par=optans$par, c=optans$counts,
		     conv=optans$convergence, time=time)
	return(outvec)
}

## Post-processing -- Kuhn Karush Tucker conditions
#  Ref. pg 77, Gill, Murray and Wright (1981) Practical Optimization, Academic Press
##	if (trace) { 
##		cat("KKT test for answer set \n") 
##		print(ans) # ?? later do opt2vec etc.		
##	}
	if (trace & ans$convergence==0) {
		cat("Successful convergence! \n")  ## inform user we've succeeded
	} else {
		cat("Not nec. converged -- KKT may be suspect\n")
	}
	# Testing final solution. Use numDeriv to get gradient and Hessian; compute Hessian eigenvalues
	kkt1<-NA
	kkt2<-NA
	if (trace) cat("Compute gradient approximation at finish\n")
	gradOK<-FALSE
	if (is.null(ugr)) {
	  	ngatend<-try(grad(ufn, ans$par, ...))
	} else {
		ngatend<-try(ugr(ans$par, ...)) # Gradient at solution
	}
	if (class(ngatend) != "try-error") gradOK<-TRUE # 100215 had == rather than != here
	if ( (! gradOK) & trace) cat("Gradient computations failure!\n") # ???? remove
	if (gradOK) {
		# test gradient
		if (trace) {
			cat("Gradient:")
			print(ngatend)
		}
		kkt1<-(max(abs(ngatend)) <= kkttol*(1.0+abs(ans$value)) ) # ?? Is this sensible?
		cat("kkt1 = ",kkt1,"\n") 
		if (trace) cat("Compute Hessian approximation \n")
		if (is.null(ugr)) {
			nhatend<-try(hessian(ufn, ans$par, ...))
		} else {
			nhatend<-try(jacobian(ugr,ans$par, ...))
		} # numerical hessian at "solution"
		print(nhatend)
		if (class(nhatend) != "try-error") {
		# For bounds constraints, we need to "project" the gradient and Hessian
		##	    bset<-sort(unique(c(which(ans$par<=lower), which(ans$par>=upper))))
		##	    nbds<-length(bset) # number of elements nulled by bounds
		##	    # Note: we assume that we are ON, not over boundary, but use <= and >=. No tolerance is used.
		##	    ngatend[bset]<-0 # "project" the gradient
		##	    nhatend[bset,] <-0
		##	    nhatend[, bset] <-0 # and the Hessian
			hev<- try(eigen(nhatend)$values) # 091215 use try in case of trouble
			if (class(hev) != "try-error") {
				cat("Eigenvalues found\n")
				print(hev)
				evnhatend <- hev # answers are OK
				# now look at Hessian
				npar<-length(ans$par)
				nbds <- 0 ## ?? for now only -- later need bounds
				negeig<-(hev[npar] <= (-1)*kkttol*(1.0+abs(ans$value)))
				cat("negeig = ",negeig,"\n")
				evratio<-hev[npar-nbds]/hev[1]
				cat("evratio =",evratio,"\n")
				# If non-positive definite, then there will be zero eigenvalues (from the projection)
				# in the place of the "last" eigenvalue and we'll have singularity.
				# WARNING: Could have a weak minimum if semi-definite.
				kkt2<- (evratio >kkt2tol) && (! negeig)
				cat("kkt2 = ",kkt2,"\n") 
			} else {
				## warnstr<-paste("Eigenvalue failure after method ",method[i],sep='')
				##	if (ctrl$dowarn) warning(warnstr)
				if (trace) {
					cat("Eigenvalue failure!\n")
					print(nhatend)
				}
			}
		} else { # computing Hessian has failed
			warnstr<-paste("Hessian not computable")
			## if (ctrl$dowarn) warning(warnstr)
			if (trace) cat(warnstr,"\n") 
		}
	} else { # gradient failure
		warnstr<-paste("Gradient not computable")
		## if (ctrl$dowarn) warning(warnstr)
		if (trace) cat(warnstr,"\n") 
	}
	kktout<-list(kktg=kkt1,kktH=kkt2,ngr=ngatend,nH=nhatend,Hev=hev)
	return(kktout)
} ## end of kktcheck

