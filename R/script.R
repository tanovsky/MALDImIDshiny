#' MALDImID Shinyapp
#' MALDImID from \code{\link{MALDImID}}
#'
#' @author Cristiano Oliveira, \email{cristiano.oliveira@iph.med-heidelberg.de}
#' @seealso \code{\link{MALDImID}}
#' @keywords MALDImID

#' @importFrom shiny shinyApp
#' @importFrom shiny addResourcePath

# related to cutoffF.R file
#' @importFrom grDevices jpeg
#' @importFrom graphics abline
#' @importFrom graphics axTicks
#' @importFrom graphics axis
#' @importFrom graphics barplot
#' @importFrom graphics hist
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics points
#' @importFrom utils read.csv
#' @importFrom utils zip
#' @importFrom grDevices dev.off

#' @importFrom openxlsx read.xlsx
#' @importFrom openxlsx write.xlsx

# TODO improve this when possible.
.onAttach <- function(...) {
  # Create link to javascript and css files for package
  # Hack to make possible to deploy with shinyBS
  # https://github.com/ebailey78/shinyBS/issues/100
  shiny::addResourcePath("sbs", system.file("www", package="shinyBS"))
  # To make the resource files available for the shiny app
  shiny::addResourcePath("www", system.file("www", package="CutoffFinder"))
}

#' runMALDImID
#'
#' Starts the shiny app
#'
#' @param port The TCP port that the application should listen on. Defaults to choosing a random port.
#' @param launch.browser If true, the system's default web browser will be launched automatically after the app is started. Defaults to true in interactive sessions only.
#' @param host The IPv4 address that the application should listen on. Defaults to the shiny.host option, if set, or "127.0.0.1" if not.
#' @return None
#' @examples
#' \dontrun{
#' runMALDImID()
#' runMALDImID(port = 9000)
#' runMALDImID(9000)
#' }
#' @export
runMALDImID <- function(port = NULL,
                            launch.browser = getOption("shiny.launch.browser",
                                                       interactive()),
                            host = getOption("shiny.host",
                                             "127.0.0.1")) {
  options(shiny.maxRequestSize=1024^3) #allows uploading 1Gb file.
  environment(MALDImIDServer) <- environment()
  shiny::shinyApp(ui = MALDImIDUI(),
                  server = function(input, output, session) {
                    MALDImIDServer(input, output, session)},
                    options = list(port = port,
                                   launch.browser = launch.browser,
                                   host = host))
}
