## ----label=C03gr1, echo=TRUE---------------------------------------------
gr <- function(par, ...) {
   fbase <- myfn(par, ...)  # ensure we have right value, may not be necessary
   df <- rep(NA, length(par))
   teps <- eps * (abs(par) + eps)
   for (i in 1:length(par)) {
      dx <- par
      dx[i] <- dx[i] + teps[i] # Dangerous step if a constraint is in the way!
      tdf <- (myfn(dx, ...) - fbase)/teps[i]
      if (!is.finite(tdf) || is.nan(tdf)) tdf <- 0  # Is this a good choice?
      df[i] <- tdf
   }
   df
}
