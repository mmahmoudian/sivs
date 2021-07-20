# If you are reading this, perhaps you are wondering what the zzz.R is. This
# code just produces the startup message when user attaches the package. Nothing
# fancy :) . This has been perfectly explained in the book R Packages by
# Hadley Wickham and Jenny Bryan and the specific section that explain this file
# can be found in the following URL:
#  https://r-pkgs.org/r.html?q=zzz.R#when-you-do-need-side-effects

.onAttach <- function(libname, pkgname){
    pkgVersion <- packageDescription(pkgname, fields = "Version")
    msg <- paste0(pkgname, " v", pkgVersion,
                  "\nPlease consider citing this method using either of the following:",
                  "\n\t- citation(sivs)",
                  "\n\t- https://doi.org/10.1093/bioinformatics/btab501",
                  "\nFor help: https://github.com/mmahmoudian/sivs/")

    packageStartupMessage(msg)
}
