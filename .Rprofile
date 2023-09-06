# source("renv/activate.R")
# source("renv/activate.R")
options(vsc.browser = FALSE)
if (!identical(Sys.getenv("CI", unset = ""), "")) {
  message("Running on CI/CD")
  Sys.setenv(
    "RENV_PATHS_CACHE" = "~/.renv/cache",
    "RENV_PATHS_LIBRARY_ROOT" =  "~/.renv/library"
  )

  # Configure better repo choice with binaries for CI/CD
  repos <-  c(
    CRAN = "https://rspm.apps.dit-prdocp.novartis.net/validated-R-4.1.0/latest",
    RSPM = "https://rspm.apps.dit-prdocp.novartis.net/exploratory/__linux__/focal/latest"
  )
  options(renv.config.repos.override = repos)
} else {
  # renv cache
  if (Sys.getenv("RSTUDIO_PROGRAM_MODE") == "server") {
    message("Running on RSW")
    # On RSW
    Sys.setenv("RENV_PATHS_ROOT" = "/funstorage/renv/")
  } else {
    message("Running locally")
    # Local
    Sys.setenv(
      "RENV_PATHS_CACHE" = "~/.renv/cache",
      "RENV_PATHS_LIBRARY_ROOT" =  "~/.renv/library"
    )
  }
  # proxy
  #Sys.setenv(
  #  http_proxy = "http://chbs-proxy.eu.novartis.net:2010",
  #  https_proxy = "http://chbs-proxy.eu.novartis.net:2010",
  #  no_proxy = "localhost,127.0.0.1,novartis.net,novartis.intra",
  #  noproxy = "localhost,127.0.0.1,novartis.net,novartis.intra"
  #)

  options(
    repos = c(
      CRAN = "https://rspm.apps.dit-prdocp.novartis.net/validated-R-4.1.0/latest",
      RSPM = "https://rspm.apps.dit-prdocp.novartis.net/exploratory/latest"
    )
  )
}

