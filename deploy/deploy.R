add_connect_server <- function() {
  if (!(Sys.getenv("CONNECT_SERVER") %in% rsconnect::servers()$url)) {
    message("Adding server")
    rsconnect::addConnectServer(
      url = Sys.getenv("CONNECT_SERVER"),
      name = "rsc-prod"
    )
  }
}

cleanup_connect_user <- function() {
  if (Sys.getenv("CONNECT_USER") %in% rsconnect::accounts(server = "rsc-prod")$name) {
    message("Cleanup previous session ...")
    rsconnect::removeAccount(
      Sys.getenv("CONNECT_USER"),
      "rsc-prod"
    )
    message(sprintf("Removed user: %s from server: %s", Sys.getenv("CONNECT_USER"), "rsc-prod"))
  }
}

register_connect_user <- function() {
  message("Adding user")
  rsconnect::connectApiUser(
    account = Sys.getenv("CONNECT_USER"),
    server = "rsc-prod",
    apiKey = Sys.getenv("CONNECT_API_KEY")
  )
}

build_app_bundle <- function() {
  main_files <- list.files(
    pattern = "(app.R)|(renv.lock)|(NAMESPACE)|(DESCRIPTION)",
    "."
  )
  inst_folder <- list.files("inst", recursive = TRUE, full.names = TRUE)
  R_folder <- list.files("R", recursive = TRUE, full.names = TRUE)
  c(main_files, inst_folder, R_folder)
}


# Required by CICD to deploy on dev, prod or a specific branch
deploy_app_rsc <- function() {
  message("Preparing to deploy")

  # Required for R4.1.0 runner to avoid SSL issues
  Sys.setenv(
    http_proxy = "",
    https_proxy = "",
    no_proxy = "",
    noproxy = ""
  )

  # Ensure proper repositories
  # options(
  #   repos = c(
  #     CRAN = "https://rspm.apps.dit-prdocp.novartis.net/validated-R-4.1.0/latest",
  #     RSPM = "https://rspm.apps.dit-prdocp.novartis.net/exploratory/latest"
  #   )
  # )
  # print(options("repos"))

  # For SSL debugging
  # options(rsconnect.http.verbose = TRUE)

  # Before going further you'll have to create some
  # ENV variables from Gitlab: Settings -> CI/CD -> variables
  # CONNECT_API_KEY -> API key obtained from RStudio Connect UI.
  # CONNECT_SERVER -> https://rsconnect-prod.dit.eu.novartis.net
  # CONNECT_USER -> your novartis ID, lower characters.
  # Also a GITHUB_PAT just in case you need github.

  # Add server and connect to the account if necessary
  # This MUST be done only once!!!
  add_connect_server()

  # Cleanup previous sessions
  cleanup_connect_user()

  # Register the user
  register_connect_user()

  # Deploy!
  deploy_error <- NULL
  deploy_res <- tryCatch(
    {
      rsconnect::deployApp(
        appDir = ".",
        appFiles = build_app_bundle(),
        appPrimaryDoc = NULL,
        appName = 'monitOS',
        appTitle = 'Monitoring OS',
        appId = 2946,
        forceUpdate = TRUE,
        logLevel = "verbose",
        account = Sys.getenv("CONNECT_USER"),
        server = "rsc-prod",
        launch.browser = FALSE
      )

      message("App successfully deployed ...")

    },
    error = function(e) {
      message("---- Error deploying %s. Please review below  ----\n")
      stop(deploy_error$message)
    }
  )
}
