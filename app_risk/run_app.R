# run_app.R - Launch the Gulf of America shipping app

# Set working directory to app folder if not already there
if (!endsWith(getwd(), "app_company")) {
  if (file.exists("app_company")) {
    setwd("app_company")
  } else {
    stop("Cannot find app_company directory")
  }
}

# Check for required environment variables
if (Sys.getenv("MAPBOX_ACCESS_TOKEN") == "") {
  message("WARNING: MAPBOX_ACCESS_TOKEN environment variable not set.")
  message("The map may not display properly without a Mapbox token.")
  message("Get a free token at: https://account.mapbox.com/access-tokens/")
  message("")
  message("Set it with: Sys.setenv(MAPBOX_ACCESS_TOKEN = 'your-token-here')")
  message("")
}

# Run the app
shiny::runApp(launch.browser = TRUE)