# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
options(shiny.port = httpuv::randomPort())

# Set base_dir for development (required)
# You can change this path or set it via environment variable
base_dir <- Sys.getenv("CLIMATE_RISK_BASE_DIR", unset = "tests/tests_data")
if (base_dir == "" || !dir.exists(base_dir)) {
  stop("Base directory not found. Please set CLIMATE_RISK_BASE_DIR environment variable or modify base_dir in this script.")
}

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application with base_dir
cat("Running app with base_dir:", base_dir, "\n")
run_app(base_dir = base_dir)
