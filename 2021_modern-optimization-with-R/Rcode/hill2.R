### hill2.R file ###

# steepest ascent hill climbing:
#    par - initial solution
#    fn - evaluation function
#    change - function to generate the next candidate 
#    lower - vector with lowest values for each dimension
#    upper - vector with highest values for each dimension
#    control - list with stopping and monitoring method:
#       $N - number of change searches (steepest ascent)
#       $maxit - maximum number of iterations
#       $REPORT - frequency of monitoring information
#       $digits - (optional) round digits for reporting
#    type - "min" or "max"
#    ... - extra parameters for fn
sa_hclimbing=function(par,fn,change,lower,upper,control,
                   type="min",...)
{ fpar=fn(par,...)
  for(i in 1:control$maxit) 
     { 
      # first change
      par1=change(par,lower,upper) 
      fpar1=fn(par1,...)
      if(control$N>1) # steepest ascent cycle 
      { for(j in 1:(control$N-1)) 
         { # random search for better par1 solutions:
          par2=change(par,lower,upper) 
          fpar2=fn(par2,...)
          b=best(par1,fpar1,par2,fpar2,type)
          par1=b$par;fpar1=b$fpar # update change
         }
      }
      if(control$REPORT>0 &&(i==1||i%%control$REPORT==0)) 
         report_iter(i,par,fpar,par1,fpar1,control)
      b=best(par,fpar,par1,fpar1,type) # update best solution
      par=b$par;fpar=b$fpar
     }
  if(control$REPORT>=1) 
     report_iter("best:",par,fpar,control=control)
  return(list(sol=par,eval=fpar))
}

# stochastic hill climbing:
#    par - initial solution
#    fn - evaluation function
#    change - function to generate the next candidate 
#    lower - vector with lowest values for each dimension
#    upper - vector with highest values for each dimension
#    control - list with stopping and monitoring method:
#       $P - Probability (in [0,1]) for accepting solutions
#       $maxit - maximum number of iterations
#       $REPORT - frequency of monitoring information
#       $digits - (optional) round digits for reporting
#    type - "min" or "max"
#    ... - extra parameters for fn
st_hclimbing=function(par,fn,change,lower,upper,control,
                   type="min",...)
{ fpar=fn(par,...)
  b=list(par=par,fpar=fpar) # set initial best
  for(i in 1:control$maxit) 
     { 
      par1=change(par,lower,upper) 
      fpar1=fn(par1,...)
      if(control$REPORT>0 &&(i==1||i%%control$REPORT==0)) 
         report_iter(i,par,fpar,par1,fpar1,control)

      b=best(b$par,b$fpar,par1,fpar1,type) # memorize best

      x=runif(1) # random between [0,1]
      if(x<control$P) # accept new solution
       { par=par1; fpar=fpar1 }
      else # select best between par and par1
       { 
        b1=best(par,fpar,par1,fpar1,type) 
        par=b1$par;fpar=b1$fpar # update par
       }
     }
  par=b$par;fpar=b$fpar # set par to best
  if(control$REPORT>=1)
     report_iter("best:",par,fpar,control=control)
  return(list(sol=par,eval=fpar))
}
