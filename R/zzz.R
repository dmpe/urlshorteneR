.onAttach <- function(libname, pkgname) {
  packageStartupMessage("In order to use bitly functions, you first need to authenticate.
                        For that execute 'bitly_auth()' in R console.")
}
