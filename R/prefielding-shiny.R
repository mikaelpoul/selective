#' Launch experiment mockup used for piloting
#'
#' Launch experiment mockup used for piloting.
#'
#' @keywords piloting prepublication rshiny
#'
#' @param ... Arguments passed to \code{shiny::runApp}.
#'
#' @importFrom shiny runApp
#'
#' @examples
#'\dontrun{
#'  launch_mockup()
#' }
#' @export
launch_mockup <- function(...) {
  shiny::runApp(appDir = system.file("prefielding-mockup", package = "selective"), ...)
}
