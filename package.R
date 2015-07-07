# Package Creation script
library(devtools)
rm(list=ls())
current.code <- as.package(".")
load_all(current.code)
document(current.code)
check(current.code)
check_doc(current.code)
#install(current.code)
