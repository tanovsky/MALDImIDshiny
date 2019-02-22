#!/usr/bin/env Rscript
# library(optparse)
# library(stringr)
# library(openxlsx)

# @importFrom openxlsx read.xlsx
# @importFrom openxlsx write.xlsx

#' @noRd
IsMzWithinMargin <- function(value, mzValues, mzMaldiAdjustement = 1, mzMargin = 0.2) {
  mzValuesAdjusted <- (mzValues - mzMaldiAdjustement)
  mzValuesAdjustedLowerEnd <- mzValuesAdjusted - mzMargin
  mzValuesAdjustedUpperEnd <- mzValuesAdjusted + mzMargin
  isWithin <- "No"
  originalmzValue <- ""
  for (i in seq_along(mzValues)) {
    if ((value < mzValuesAdjustedUpperEnd[i]) & (value > mzValuesAdjustedLowerEnd[i])) {
      isWithin <- "Yes"
      originalmzValue <- as.character(mzValues[i])
      break
    }
  }
  return(c(isWithin, originalmzValue))
}

# option_list <- list(
#     make_option(c("-v", "--verbose"),
#                 help = "Be verbose",
#                 action = "store_true",
#                 default = FALSE),
#     make_option(c("-o", "--output"),
#                 help = "Final results filepath",
#                 action = "outputfilepath_custom",
#                 type = "character"
#                 ))
#
# someExampleDirectory <- "c:/myResources"
# myEpilogue = paste0("Example: %prog peptides='", file.path(someExampleDirectory, "peptides.xlsx"), "' hsilMembers='", file.path(someExampleDirectory, "hsilMembers.xlsx"), "' mzValues='865.391, 987.573, 1905.95' output='results.xlsx'")
#   parser <- OptionParser(
#     usage = paste("%prog peptides hsilMembers mzValues \n",
#                   "This is Maldimyid. it calculates.....................................",
#                   ".....................................................................\n",
#                   "Required parameters:",
#                   "peptides = peptides filepath",
#                   "hsilMembers = HSIL Filepath",
#                   "mzValues = comma separated list of mz values",
#                   "output = generated results filepath",
#                   sep="\n"),
#     epilogue = myEpilogue,
#     option_list=option_list)
#
# arguments <- parse_args(parser, positional_arguments = TRUE)
# options <- arguments$options
#
# # TODO validate all mandatory parameters
# if (length(arguments$args) < 4) {
#   cat("Wrong number of required positional arguments\n\n")
#   print_help(parser)
#   stop()
# } else {
#   inputFilepath <- arguments$args[1]
#   inputMembersHSIL_Filepath <- arguments$args[2]
#   mzValues <- arguments$args[3]
#   outputFilepath <- arguments$args[4]
#
#   inputFilepath <- strsplit(inputFilepath, split = "=")[[1]][2]
#   inputMembersHSIL_Filepath <- strsplit(inputMembersHSIL_Filepath, split = "=")[[1]][2]
#   mzValues <- strsplit(mzValues, split = "=")[[1]][2]
#   mzValues <- trimws(strsplit(mzValues, split = ",")[[1]])
#   mzValues <- as.numeric(mzValues)
#   outputFilepath <- strsplit(outputFilepath, split = "=")[[1]][2]
# }



#' @noRd
GetResult <- function(conditionChosen, inputFilepath, inputMembersHSIL_Filepath, mzValues, outputFilepath) {

  # inputFilepath <-
  #
  #   inputFilepath <-'/Users/tanovsky/wip/Remy/MASSturbo/resources/peptides.txt',
  # inputMembersHSIL_Filepath <- '/Users/tanovsky/wip/Remy/MASSturbo/resources/members HSIL.xlsx',
  # mzValues <- '865.39\n987.57\n1905.95',
  #             'resultsMaldimyid.xlsx'

#condition <- "HSIL"
#condition <- "ADE"

myData <- read.csv(file = inputFilepath, check.names = FALSE, sep = "\t")
membersHSILrawData <- read.xlsx(inputMembersHSIL_Filepath)
geneNameHSIL <- membersHSILrawData$Gene.name
columnNames <- colnames(myData)

pattern = "Intensity \\w"
conditionColumnAndCondition <- str_match(columnNames[grepl(pattern, columnNames)], "Intensity (?<condition>[a-z,A-Z]*)")
conditionColumn <- names(table(conditionColumnAndCondition[,1]))
numberOfSamplesPerCondition <- table(conditionColumnAndCondition[,2])
conditions <- names(numberOfSamplesPerCondition)

conditionColumnsHeaders <- columnNames[grepl(paste0("Intensity ", conditionChosen, "[0-9]+"), columnNames)]
selectedTableHeaders <- c("Mass", "Proteins", "Gene names", "Score", "Unique (Proteins)")
myFilteredData <- myData[, selectedTableHeaders]
for (conditionColumnHeader in conditionColumn) {
  myFilteredData <- cbind(myFilteredData, apply(myData[, grepl(conditionColumnHeader, columnNames)], 1, mean))
}

averageHeader <- "Average"
colnames(myFilteredData) <- c(selectedTableHeaders, paste(averageHeader, conditionColumn))


positiveReplicatesTable <- NULL
for (condition in conditions) {
  conditionColumnsHeaders <- columnNames[grepl(paste0("Intensity ", condition, "[0-9]+"), columnNames)]
  positiveReplicatesTable <- cbind(positiveReplicatesTable,  "#PositiveReplicates" = apply(myData[, conditionColumnsHeaders], 1, function(x) {sum(x > 0)}))
}
colnames(positiveReplicatesTable) <- paste0("#PositiveReplicates_", conditions)

myFilteredData <- cbind(myFilteredData, positiveReplicatesTable)
isWithinRange <- lapply(myFilteredData$Mass, IsMzWithinMargin,  mzValues = mzValues, mzMaldiAdjustement = 1, mzMargin = 0.2)
isWithinRange <- matrix(unlist(isWithinRange), ncol = 2, byrow = TRUE)
myFilteredData <- cbind(myFilteredData,
                        "mzWithinRange" = isWithinRange[, 1],
                        "MZ original" = isWithinRange[, 2],
                        "MaldiMass" = as.numeric(isWithinRange[, 2]) - 1,
                        "MaldiDelta" = (myFilteredData$Mass - (as.numeric(isWithinRange[, 2]) - 1)))

truncatedMasses <- trunc(myFilteredData$Mass)
uniqueTruncatedMasses <- sort(unique(truncatedMasses))

myFilteredData <- cbind(tmpTruncatedMass = truncatedMasses, myFilteredData)

filteredAndSortedTable <- NULL
for (i in uniqueTruncatedMasses) {
  tmpTable <- myFilteredData[myFilteredData$tmpTruncatedMass == i, -c(1)]
#  filteredAndSortedTable <- rbind(filteredAndSortedTable, tmpTable[order(tmpTable$"Average Intensity HSIL"),])

  averageColumnName <- paste("Average Intensity", conditionChosen)
  filteredAndSortedTable <- rbind(filteredAndSortedTable, tmpTable[order(tmpTable[, averageColumnName]),])


}

filteredAndSortedTable <- cbind(filteredAndSortedTable,
                                genePresentAtHSIL = filteredAndSortedTable$"Gene names" %in% geneNameHSIL)

genePresentAtConditionChosenColumnName <- paste0("genePresentAt", conditionChosen)
names(filteredAndSortedTable)[length(names(filteredAndSortedTable))] <- genePresentAtConditionChosenColumnName

# To visualize only the interesting results..
# interestingResults <- filteredAndSortedTable[(filteredAndSortedTable$mzWithinRange != "No") &
#                          filteredAndSortedTable$genePresentAtHSIL,]
interestingResults <- filteredAndSortedTable[(filteredAndSortedTable$mzWithinRange != "No") &
                                               filteredAndSortedTable[, genePresentAtConditionChosenColumnName],]

# To output the filtered results as an xlsx file
outputDir <- dirname(outputFilepath)
dir.create(outputDir, showWarnings = FALSE)
write.xlsx(filteredAndSortedTable, file = outputFilepath)
print("FINTIUOO!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
return(interestingResults)
}