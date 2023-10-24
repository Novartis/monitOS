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
        appTitle = 'Monitoring overall survival in pivotal trials in indolent cancers',
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
