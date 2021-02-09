.onAttach <- function(libname, pkgname) {
  packageStartupMessage("To get started, see the demos with \n", 
                        "vignette(package='maskRangeR')")
}

#if(getRversion() >= "3.4.0")  utils::globalVariables(c("cost"))

