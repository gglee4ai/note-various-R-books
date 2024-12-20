## ----label=C04mytanchunk1, echo=TRUE, size="scriptsize", cache=TRUE------
mytan<-function(xdeg){ # tangent in degrees
    xrad<-xdeg*pi/180.0 # conversion to radians
    tt<-tan(xrad)
}
gmytan<-function(xdeg){ # tangent in degrees
    xrad<-xdeg*pi/180.0 # conversion to radians
    gg<-pi/(180*cos(xrad)^2)
}
