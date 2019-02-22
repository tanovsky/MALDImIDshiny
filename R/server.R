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
#' @importFrom shiny h2
#' @importFrom shiny withTags
#' @importFrom shinyBS addTooltip
#' @importFrom shinyjs html
#' @importFrom stringr str_match
#' @importFrom tools file_ext
#' @importFrom utils read.csv
#' @importFrom shiny hideTab
#' @importFrom shiny showTab

#' @noRd
addTooltipHelper <- function(session,
                             id,
                             title,
                             placement = "bottom",
                             trigger = "hover",
                             options = list(delay = list(show=1000, hide=1000))) {
  shinyBS::addTooltip(session = session,
                      id = id,
                      title = title,
                      placement = placement,
                      trigger = trigger
                      #                      ,
                      #                      options = options
  )
}

mzValuesText_numeric <- "12\n32\n34\n45"
mzValuesText_nonNumeric <- "12\n32\n34\n45a"


ValidateAndGetMZValues <- function(textValues) {
  # textValues <- mzValuesText_nonNumeric
  # textValues <- mzValuesText_numeric
  mzValuesVector <- as.numeric(strsplit(textValues, split = "\n")[[1]])
  if ((length(mzValuesVector) == 0) | (any(is.na(mzValuesVector)))) {
    return(NULL)
  } else {
    return(mzValuesVector)
  }
}




################################################################################
# shinyServer
################################################################################
#' @noRd
MALDImIDServer <- function(input, output, session) {
  shortNotificationDuration <- 3
  longNotificationDuration <- 5
  LABEL_NO_DATA <- "(no data)"
  inputDirectory <- CreateSessionInputDirectory(session)
  myData <- NULL

  # notificationsTootip <<- c()

  hideTab(inputId = "inTabset", target = "peptideFilepath", session = session)
  hideTab(inputId = "inTabset", target = "hsilFilepath", session = session)
  hideTab(inputId = "inTabset", target = "analysis", session = session)

  # shinyBS::addTooltip(session = session, id = "tsvFilepathHelper",
  #                     #          bsTooltip(id = "tsvFilepathHelper",
  #                     title = "Input a tab separated value file. It can have any extension as long as it is a tab separated",
  #                     #                    placement = "right",
  #                     placement = "bottom",
  #                     trigger = "hover",
  #                     #                    options = NULL,
  #                     options = list(delay = list(show=1000, hide=3000)))

  addTooltipHelper(session = session,
                   id = "conditionHelper",
                   title = "Input the condition to be compared")

  addTooltipHelper(session = session,
                   id = "peptideFilepathHelper",
                   title = "Input a tab separated value file. It can have any extension as long as it is a tab separated.")
  addTooltipHelper(session = session,
                   id = "hsilFilepathHelper",
                   title = "Input a xlsx file.")
  addTooltipHelper(session = session,
                   id = "mzValue",
                   title = "Input each m/z values in a new line.\n Use '.' as decimal separator")
  addTooltipHelper(session = session,
                   id = "Run",
                   title = "Run Analysis")


  observeEvent(input$peptideFilepath, {
    if (!is.null(input$peptideFilepath)) {
#      print(inputDirectory)
      file.remove(list.files(path = inputDirectory, full.names = TRUE, pattern = ".txt"))
      fixUploadedFilesNames(input$peptideFilepath, inputDirectory)
      myData <- read.csv(file = UploadedFilepaths(input$peptideFilepath, inputDirectory),
                         check.names=FALSE,
                         sep = "\t")

      # output$importedPeptideFilepath <- renderDataTable(FixDataTable(input$peptideFilepath,
      #                                                            inputDirectory,
      #                                                            type = file_ext(input$peptideFilepath$datapath)),
      #                                               options = list(dom = "", searching = FALSE))
      # To edit number of options just use this vector
      # the 'All' option is added by default
      # and the first options is selected by default
      availableRowsInComboBoxOption <- c(10, 25, 50, 100)
      availableRowsInComboBoxOptionsExtendedWithAll <- c(availableRowsInComboBoxOption, -1)
      labelsForAvailableRowsInComboBoxOptionsExtendedWithAll <- c(
        sapply(availableRowsInComboBoxOption, toString), 'All'
      )
      output$importedPeptideFilepath <- renderDataTable(myData,
                                        options = list(
                                          lengthMenu = list(availableRowsInComboBoxOptionsExtendedWithAll,
                                                            labelsForAvailableRowsInComboBoxOptionsExtendedWithAll),
                                          pageLength = availableRowsInComboBoxOptionsExtendedWithAll[1],
                                          autoWidth = TRUE,
                                          scrollX = TRUE,
                                          fillContainer = TRUE,
                                          searching = TRUE))
    }
    showTab(inputId = "inTabset", target = "peptideFilepath", session = session)
    hideTab(inputId = "inTabset", target = "analysis", session = session)
  })



  observeEvent(input$hsilFilepath, {
    if (!is.null(input$hsilFilepath)) {
      file.remove(list.files(path = inputDirectory, full.names = TRUE, pattern = "*.xlsx"))
      fixUploadedFilesNames(input$hsilFilepath, inputDirectory)
      # myData <- read.csv(file = UploadedFilepaths(input$hsilFilepath, inputDirectory),
      #                    check.names=FALSE,
      #                    sep = "\t")

      myData <- read.xlsx(UploadedFilepaths(input$hsilFilepath, inputDirectory))



      # output$importedPeptideFilepath <- renderDataTable(FixDataTable(input$hsilFilepath,
      #                                                            inputDirectory,
      #                                                            type = file_ext(input$hsilFilepath$datapath)),
      #                                               options = list(dom = "", searching = FALSE))
      # To edit number of options just use this vector
      # the 'All' option is added by default
      # and the first options is selected by default
      availableRowsInComboBoxOption <- c(10, 25, 50, 100)
      availableRowsInComboBoxOptionsExtendedWithAll <- c(availableRowsInComboBoxOption, -1)
      labelsForAvailableRowsInComboBoxOptionsExtendedWithAll <- c(
        sapply(availableRowsInComboBoxOption, toString), 'All'
      )
      output$importedHsilFilepath <- renderDataTable(myData,
                                                        options = list(
                                                          lengthMenu = list(availableRowsInComboBoxOptionsExtendedWithAll,
                                                                            labelsForAvailableRowsInComboBoxOptionsExtendedWithAll),
                                                          pageLength = availableRowsInComboBoxOptionsExtendedWithAll[1],
                                                          autoWidth = TRUE,
                                                          scrollX = TRUE,
                                                          fillContainer = TRUE,
                                                          searching = TRUE))
    }
    showTab(inputId = "inTabset", target = "hsilFilepath", session = session)
    hideTab(inputId = "inTabset", target = "analysis", session = session)
    updateTabsetPanel(session, "inTabset", selected = "hsilFilepath")
  })

  onSessionEnded(function() {
    cat("Session Ended\n")
    success <- unlink(GetSessionDirectory(session),
                      recursive = T)
    if (success) {
      print("Delete temporary folder sucessfully")
    } else {
# TODO why is this not working..
#      print("Failed to delete temporary folder")
#      print(GetSessionDirectory(session))
    }
  })

  observeEvent(input$Run, {

#     #print("before")
#     #print(notificationsTootip)
#     # lapply(notificationsTootip, removeNotification)
#     # notificationsTootip <<- c()
#     # print("after")
#     # print(notificationsTootip)
#
#     if (is.null(input$tsvFilepath)) {
#       notificationId <- showNotification(paste("Please upload required file"), duration = shortNotificationDuration)
#       # print(notificationId)
#       # print("and now notificationId!?!")
#       # print(notificationId)
#       # notificationsTootip <<- c(notificationsTootip, notificationId)
#       # print("and now upload!?!")
#       # print(notificationsTootip)
#
#       return()
#     }
#     filepath <- UploadedFilepaths(input$tsvFilepath, inputDirectory)
#     myData <- read.csv(file = filepath,
#                        check.names=FALSE,
#                        sep = "\t")
#     # if ((input$outcome == LABEL_NO_DATA) & grepl("outcome", input$method)) {
#     #   showNotification(paste("If you select a method that to operate on the outcome, you need to select an outcome too."), duration = longNotificationDuration)
#     #   return()
#     # }
#     updateTabsetPanel(session, "inTabset", selected = "analysis")
#     withProgress(message = 'Analysis in progress', {
#
#     })
# #    showTab(inputId = "inTabset", target = "tsvFile", session = session)
#     showTab(inputId = "inTabset", target = "analysis", session = session)



# print(
#     ValidateAndGetMZValues(input$mzValue)
# )

    mzValues <- ValidateAndGetMZValues(input$mzValue)
    if (is.null(mzValues)) {
      showNotification(paste("Please insert numeric m/z values"), duration = 10)
    }

    # peptides <- read.csv(file = UploadedFilepaths(input$peptideFilepath, inputDirectory),
    #                      check.names=FALSE,
    #                      sep = "\t")
    #
    # hsilData <- read.csv(file = UploadedFilepaths(input$hsilFilepath, inputDirectory),
    #                      check.names=FALSE,
    #                      sep = "\t")

#    showNotification(paste("Calculating.."), duration = 30)

    results <- NULL
    withProgress(message = 'Calculating..', value = 0, {
    results <- GetResult(input$condition,
                         UploadedFilepaths(input$peptideFilepath, inputDirectory),
                         UploadedFilepaths(input$hsilFilepath, inputDirectory),
                         mzValues,
                         file.path(inputDirectory,'resultsMaldimyid.xlsx'))
    })

# for full query
# 865.39
# 987.57
# 1905.95

# for light query
#1485.5864

    # results <- GetResult('/Users/tanovsky/wip/Remy/MASSturbo/resources/peptides.txt',
    #           '/Users/tanovsky/wip/Remy/MASSturbo/resources/members HSIL.xlsx',
    #           c(865.39, 987.57, 1905, 95),
    #           'resultsMaldimyid.xlsx')




  availableRowsInComboBoxOption <- c(10, 25, 50, 100)
  availableRowsInComboBoxOptionsExtendedWithAll <- c(availableRowsInComboBoxOption, -1)
  labelsForAvailableRowsInComboBoxOptionsExtendedWithAll <- c(
    sapply(availableRowsInComboBoxOption, toString), 'All'
  )
  output$results <- renderDataTable(results,
                                   options = list(
                                     lengthMenu = list(availableRowsInComboBoxOptionsExtendedWithAll,
                                                       labelsForAvailableRowsInComboBoxOptionsExtendedWithAll),
                                     pageLength = availableRowsInComboBoxOptionsExtendedWithAll[1],
                                     autoWidth = TRUE,
                                     scrollX = TRUE,
                                     fillContainer = TRUE,
                                     searching = TRUE))

  showTab(inputId = "inTabset", target = "analysis", session = session)
  updateTabsetPanel(session, "inTabset", selected = "analysis")

#    ./MALDImID.R peptides='/Users/tanovsky/wip/Remy/MASSturbo/resources/peptides.txt' hsilMembers='/Users/tanovsky/wip/Remy/MASSturbo/resources/members HSIL.xlsx' mzvalues='865.39, 987.57, 1905,95' output='resultsMaldimyid.xlsx'


  })

  output$downloadData <- myZipDownloadHandler(generatedFilename = "MALDImID",
                                              basedir = inputDirectory,
                                              pattern = "*.(xlsx|txt)$")
}
