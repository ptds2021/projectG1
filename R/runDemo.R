#' @title Covid-19 tracker
#'
#' @author L. Hsieh, A. Monney, R. Tombola
#' @export
run_tracker <- function() {
  appDir <- system.file("shiny-examples", "tracker", package = "projectG1")
  if (appDir == "") {
    stop(
      "Could not find example directory. Try re-installing `projectG1`.",
      call. = FALSE
    )
  }
  shiny::runApp(appDir, display.mode = "normal")

}

