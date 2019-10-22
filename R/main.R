#' Function to run Shiny from R
#' @import shiny
#' @examples
#' shiny_mofa(port = 8400)
shiny_mofa <- function(...) {
    shiny::runApp(system.file('app', package = "mofaplusshiny"), ...)
}
