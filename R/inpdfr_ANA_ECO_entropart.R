#' Performs an analysis of ecological diversity and structure.
#'
#' Uses the \code{\link[entropart]{entropart-package}} to analyse the word-occurrence data.frame,
#'    considering words as species and documents as communities.
#'
#' @param wordF The data.frame containing word occurrences.
#' @param getPlot A vector with four logical values. If \code{getPlot[1]==TRUE},
#'    the \code{MetaCommunity} object is plotted and saved in the RESULTS directory.
#'    If \code{getPlot[2]==TRUE}, the \code{DivPart} analisis is plotted and
#'    saved in the RESULTS directory. If \code{getPlot[3]==TRUE}, the \code{DivEst}
#'    analisis is plotted and saved in the RESULTS directory. If \code{getPlot[4]==TRUE},
#'    the \code{DivProfile} analisis is plotted and saved in the RESULTS directory.
#' @param getTextSink A vector with four logical values. If \code{getTextSink[1]==TRUE},
#'    the \code{MetaCommunity} object is saved in the RESULTS directory.
#'    If \code{getTextSink[2]==TRUE}, the \code{DivPart} analisis is saved in the
#'    RESULTS directory. If \code{getTextSink[3]==TRUE}, the \code{DivEst}
#'    analisis is saved in the RESULTS directory. If \code{getTextSink[4]==TRUE},
#'    the \code{DivProfile} analisis is saved in the RESULTS directory.
#' @param mwidth The width of the plot in pixels.
#' @param mheight The height of the plot in pixels.
#' @param formatType The format for the output file ("eps", "pdf", "png", "svg", "tiff", "jpeg", "bmp").
#' @return A \code{MetaCommunity} object (see \code{\link[entropart]{entropart-package}}).
#' @examples
#' \dontrun{
#' data("wordOccuDF")
#' doMetacomEntropart(wordF = wordOccuDF)
#' }
#' @export
#' @importFrom entropart DivPart
#' @importFrom entropart DivEst
#' @importFrom entropart DivProfile
doMetacomEntropart <- function(wordF, getPlot = c(TRUE, TRUE, TRUE, TRUE), 
  getTextSink = c(TRUE, TRUE, TRUE, TRUE), mwidth = 800, mheight = 800, formatType = "png"){
  ## create RESULTS folder
  if(sum(getPlot) > 0 | sum(getTextSink) > 0){
    subDir <- "RESULTS"
    dir.create(file.path(getwd(), subDir), showWarnings = FALSE)
  }
  ## make entropart analysis
  metacomDB <- wordF
  names(metacomDB)[1] <- "Species"
  dfAbundances <- metacomDB[,2:length(wordF[1,])]
  rownames(dfAbundances) <- as.character(metacomDB[,1])
  metaCom <- entropart::MetaCommunity(Abundances = dfAbundances)

  if(getPlot[1] == TRUE){
    R.devices::devEval(type = formatType, name = "entropart_MetaCom",
      aspectRatio = mheight / mwidth,
      scale = do.call(function(){if((mheight / mwidth) <= 1) {
        x <- max(mheight / 480, mwidth / 480)} else {
          x <- min(mheight / 480, mwidth / 480)}
        return(x)}, list())
      , path = file.path(getwd(), subDir), {
        try(graphics::plot(metaCom), silent = TRUE)
      }
    )
  }

  if(getTextSink[1] == TRUE){
    sink('RESULTS/entropart_MetaCom.txt')
      cat('\n#######################\n### SUMMARY         ###\n#######################\n')
      try(summary(metaCom), silent = TRUE)
      cat('\n#######################\n### RESULTS         ###\n#######################\n')
      try(print(metaCom), silent = TRUE)
    sink()
  }

  mDP <- DivPart(MC = metaCom) # Diversity partition

  if(getPlot[2] == TRUE){
    R.devices::devEval(type = formatType, name = "entropart_DivPart",
     aspectRatio = mheight / mwidth,
     scale = do.call(function(){if((mheight / mwidth) <= 1) {
       x <- max(mheight / 480, mwidth / 480)} else {
         x <- min(mheight / 480, mwidth / 480)}
       return(x)}, list())
     , path = file.path(getwd(), subDir), {
      try(graphics::plot(mDP), silent = TRUE)
     }
    )
  }

  if(getTextSink[2] == TRUE){
    sink('RESULTS/entropart_DivPart.txt')
      cat('\n#######################\n### SUMMARY         ###\n#######################\n')
      try(summary(mDP), silent = TRUE)
      cat('\n#######################\n### RESULTS         ###\n#######################\n')
      try(print(mDP), silent = TRUE)
    sink()
  }

  mDE <- DivEst(MC = metaCom) # Diversity estimation

  if(getPlot[3] == TRUE){
    R.devices::devEval(type = formatType, name = "entropart_DivEst",
     aspectRatio = mheight / mwidth,
     scale = do.call(function(){if((mheight / mwidth) <= 1) {
       x <- max(mheight / 480, mwidth / 480)} else {
         x <- min(mheight / 480, mwidth / 480)}
       return(x)}, list())
     , path = file.path(getwd(), subDir), {
      try(graphics::plot(mDE), silent = TRUE)
     }
    )
  }

  if(getTextSink[3] == TRUE){
    sink('RESULTS/entropart_DivEst.txt')
      cat('\n#######################\n### SUMMARY         ###\n#######################\n')
      try(summary(mDE), silent = TRUE)
      cat('\n#######################\n### RESULTS         ###\n#######################\n')
      try(print(mDE), silent = TRUE)
    sink()
  }

  mDProf <- DivProfile(MC = metaCom) # Diversity profiles

  if(getPlot[4] == TRUE){
    R.devices::devEval(type = formatType, name = "entropart_DivProf",
     aspectRatio = mheight / mwidth,
     scale = do.call(function(){if((mheight / mwidth) <= 1) {
       x <- max(mheight / 480, mwidth / 480)} else {
         x <- min(mheight / 480, mwidth / 480)}
       return(x)}, list())
     , path = file.path(getwd(), subDir), {
      try(graphics::plot(mDProf), silent = TRUE)
     }
    )
  }

  if(getTextSink[4] == TRUE){
    sink('RESULTS/entropart_DivProf.txt')
      cat('\n#######################\n### SUMMARY         ###\n#######################\n')
      try(summary(mDProf), silent = TRUE)
      cat('\n#######################\n### RESULTS         ###\n#######################\n')
      try(print(mDProf), silent = TRUE)
    sink()
  }

  return(metaCom)
}
