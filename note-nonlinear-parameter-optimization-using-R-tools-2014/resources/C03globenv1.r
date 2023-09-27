## ----label=C03globenv1, echo=TRUE, eval=TRUE-----------------------------
mystart <- function() {
# JN: Define globals here.
   gtn<-list(x=0, y=1, vec = rep(0, 9))
   envjn<<-list2env(gtn)
}
y<-4
myrun <- function(){
  cat("y:",y,"  envjn$y: ")
  print(envjn$y)
  envjn$y <- 9876
  return(0)
}
mystart()
myrun()
cat("envjn$y:",envjn$y,"\n")


