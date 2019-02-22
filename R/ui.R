#' MALDImID Shinyapp
#' ShinyApp from \code{\link{MALDImID}}
#'
#' @author Cristiano Oliveira, \email{oliveirc@dkfz.de}
#' @seealso \code{\link{MALDImID}}
#' @keywords MALDImID
#'

#' @importFrom shiny renderDataTable
#' @importFrom shiny insertUI
#' @importFrom shiny observeEvent
#' @importFrom shiny updateNumericInput
#' @importFrom shiny renderUI
#' @importFrom shiny showNotification
#' @importFrom shiny onSessionEnded
#' @importFrom shiny updateTabsetPanel
#' @importFrom shiny withProgress
#' @importFrom shiny removeUI
#' @importFrom shiny uiOutput
#' @importFrom shiny tagList
#' @importFrom shiny renderImage
#' @importFrom shiny validate
#' @importFrom shiny selectInput
#' @importFrom shiny plotOutput
#' @importFrom shiny HTML
#' @importFrom shiny div
#' @importFrom shiny withTags
#' @importFrom shinyBS addTooltip
#' @importFrom shinyjs html
#' @importFrom stringr str_match
#' @importFrom tools file_ext
#' @importFrom utils read.csv
#' @importFrom shiny hideTab
#' @importFrom shiny showTab
#' @importFrom shiny includeHTML
#' @importFrom shiny shinyApp
#' @importFrom shiny shinyUI
#' @importFrom shiny downloadButton
#' @importFrom shiny fluidPage
#' @importFrom shiny textAreaInput
#' @importFrom shiny headerPanel
#' @importFrom shiny mainPanel
#' @importFrom shiny titlePanel
#' @importFrom shiny numericInput
#' @importFrom shiny h3
#' @importFrom shiny sidebarPanel
#' @importFrom shiny htmlOutput
#' @importFrom shiny tabPanel
#' @importFrom shiny fileInput
#' @importFrom shiny sidebarLayout
#' @importFrom shiny dataTableOutput
#' @importFrom shiny tags
#' @importFrom shiny div
#' @importFrom shiny tabsetPanel
#' @importFrom shiny withTags
#' @importFrom shiny icon
#' @importFrom shiny p
#' @importFrom shiny actionButton
#' @importFrom shiny conditionalPanel
#' @importFrom shinyBS bsTooltip
#' @importFrom shinyBS tipify
#' @importFrom shinyjs useShinyjs
#' @importFrom shiny textAreaInput
#' @importFrom shiny textInput
#'@importFrom shiny sliderInput



# #' @noRd
# MALDImIDUI <- function() {
# shinyUI(fluidPage(
#
#   # Application title
#   titlePanel("Old Faithful Geyser Data"),
#
#   # Sidebar with a slider input for number of bins
#   sidebarLayout(
#     sidebarPanel(
#       sliderInput("bins",
#                   "Number of bins:",
#                   min = 1,
#                   max = 50,
#                   value = 30)
#     ),
#
#     # Show a plot of the generated distribution
#     mainPanel(
#       plotOutput("distPlot")
#     )
#   )
# ))
# }


################################################################################
#shinyUI
################################################################################
#' @noRd
MALDImIDUI <- function() {
  shinyUI(
    fluidPage(
      tags$head(
        tags$link(rel = "shortcut icon",
                  type = "image/x-icon",
                  href = "www/favicon.ico")),
#      useShinyjs(),
      headerPanel("MALDImID"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          div(id = "conditionHelper",
              textInput(inputId = "condition",
                        label="Condition")),
          div(id = "peptideFilepathHelper",
              fileInput(inputId = "peptideFilepath",
                        label="Peptide file",
                        multiple = FALSE,
                        accept = NULL)),
          div(id = "hsilFilepathHelper",
              fileInput(inputId = "hsilFilepath",
                        label="Condition file",
                        multiple = FALSE,
                        accept = NULL)),
          div(id = "mzValueHelper",
            textAreaInput(inputId = "mzValue",
                          label = "m/z values",
                          value = "",
                          height = "200px")),
          div(actionButton('Run', label = "OK",
                           icon = icon("ok", lib = "glyphicon")),
              style="float:right;"),
          withTags( # hack to fix margin issue..
            div(style = "margin-top: 80px")
          )
        ),
        mainPanel(
          width = 8,
          tabsetPanel(id = "inTabset",
                      # tabPanel('Home',
                      #          div(style = "margin-top: 30px"),
                      #           includeHTML(system.file("www/home.html", package="CutoffFinder"))),
                      tabPanel('Peptides', value = "peptideFilepath",
                               div(style = "margin-top: 30px"),
                               dataTableOutput("importedPeptideFilepath")),
                      tabPanel('Condition', value = "hsilFilepath",
#                      tabPanel('HSIL', value = "hsilFilepath",
                               div(style = "margin-top: 30px"),
                               dataTableOutput("importedHsilFilepath")),
                      tabPanel('Analysis', value = "analysis",
                               withTags(
                                 div(class = "someSpace",
                                     h3("  "))
                               ),
                               div(downloadButton('downloadData', 'Download'), style="float:right;"),
                               dataTableOutput("results"),
                               # HAVE TO KEEP AT LEAST ONE TOOLTIP AT THE CLIENT FOR THIS LIBRARY TOWORK..
                               bsTooltip(id = "downloadData",
                                         title = "Download all data and images",
                                         placement = "bottom",
                                         trigger = "hover",
                                         options = NULL),
                               # Place holder for HTML code outputted
                               tags$div(id = 'htmlPlaceholder'),
                               tags$div(id = 'plots'))
          )
        )
      )
    )
  )
}
