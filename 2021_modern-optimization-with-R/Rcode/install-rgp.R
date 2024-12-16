### install-rgp.R file ###

install.packages("devtools") # install first devtools
library(devtools) # load devtools
install.packages("emoa") # install emoa rgp dependency
install_version("rgp",version="0.4-1") # install archived rgp
