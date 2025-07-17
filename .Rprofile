# detect if you are using OSX
# if (Sys.info()["sysname"] == "Darwin") {
#   # Set the proxy for OSX
#   Sys.setenv(
#     HTTP_PROXY = "http://proxy.eu.novartis.net:2010",
#     HTTPS_PROXY = "http://proxy.eu.novartis.net:2010",
#     http_proxy = "http://proxy.eu.novartis.net:2010",
#     https_proxy = "http://proxy.eu.novartis.net:2010"
#   )
# }

source("renv/activate.R")

# options(vsc.browser = FALSE)
# if (!identical(Sys.getenv("CI", unset = ""), "")) {
#   message("Running on CI/CD")
#   Sys.setenv(
#     "RENV_PATHS_CACHE" = "~/.renv/cache",
#     "RENV_PATHS_LIBRARY_ROOT" = "~/.renv/library"
#   )

#   # Configure better repo choice with binaries for CI/CD
#   repos <- c(
#     CRAN = "https://rspm.apps.dit-prdocp.novartis.net/validated-R-4.1.0/latest",
#     RSPM = "https://rspm.apps.dit-prdocp.novartis.net/exploratory/__linux__/focal/latest"
#   )
#   options(renv.config.repos.override = repos)
# } else {
#   # renv cache
#   if (Sys.getenv("RSTUDIO_PROGRAM_MODE") == "server") {
#     message("Running on RSW")
#     # On RSW
#     Sys.setenv("RENV_PATHS_ROOT" = "/funstorage/renv/")
#   } else {
#     message("Running locally")
#     # Local
#     Sys.setenv(
#       "RENV_PATHS_CACHE" = "~/.renv/cache",
#       "RENV_PATHS_LIBRARY_ROOT" = "~/.renv/library"
#     )
#   }

#   options(
#     repos = c(
#       CRAN = "https://rspm.apps.dit-prdocp.novartis.net/validated-R-4.1.0/latest",
#       RSPM = "https://rspm.apps.dit-prdocp.novartis.net/exploratory/latest"
#     )
#   )
# }

# if (Sys.getenv("LOAD_MONITOS_PKG") != "") {
#   message("Loading monitOS package locally")
#   devtools::load_all(".")
# }
