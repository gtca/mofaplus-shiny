#' Function to run Shiny from R
#' @import shiny
#' @examples
#' shiny_mofa2(port = 8400)
shiny_mofa2 <- function(...) {
    shiny::runApp(system.file('app', package = "mofaplusshiny"), ...)
}
