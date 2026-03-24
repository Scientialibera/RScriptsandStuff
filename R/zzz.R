.onAttach <- function(libname, pkgname) {
  ver <- utils::packageVersion(pkgname)
  packageStartupMessage(
    pkgname, " v", ver,
    " | ML \u00b7 Finance \u00b7 NLP toolkit",
    "\n\u2139 Use help(package = '", pkgname, "') for function reference"
  )
}
