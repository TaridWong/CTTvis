#' @title point_biserial_visualization
#'
#' @description plotting results of item discrimination analysis from the Classical test theory framework
#'
#' @param responses A dichotompus item response object (a dataframe or a matrix)
#' @param title Title of the plot
#' @param pBis_threshold Threshold of the point-biserial correlation (pBis)
#'
#' @return A data frame sorted by item discrimination in ascending order. A plot of item discrimination in relation to the specified threshold.
#' @examples
#'
#' data(dichotomous_response)
#'
#' # To plot item discrimination with pBis threshol of .20.
#'
#' point_biserial_plot(responses = dichotomous_response,
#'       title = "Item Discrimination Plot", pBis_threshold = 0.20)
#'
#' @export
#' @importFrom CTT "itemAnalysis"
#' @importFrom graphics abline text

point_biserial_plot <-
  function(responses, title = "Item Discrimination", pBis_threshold = 0.20)
  {
    # CRITICAL VALUES
    cvpb = pBis_threshold

    ctt.analysis <- CTT::itemAnalysis(responses, itemReport=TRUE,
                                      NA.Delete=TRUE, pBisFlag = .20,
                                      bisFlag = .20, flagStyle = c("X",""))

    test_item.total <- data.frame(item = 1:ctt.analysis$nItem ,
                                  point_biserial = ctt.analysis$itemReport$pBis)

    plot(test_item.total,
         main = title,  # Use the title parameter here
         type = "p",
         pch = 1,
         cex = 2.8,
         col = "purple",
         ylab = "Point-Biserial correlation",
         xlab = "Item Number",
         ylim = c(-0.2, 1),
         xlim = c(0, ctt.analysis$nItem))

    abline(h = cvpb, col = "red")

    outlier <- data.matrix(subset(test_item.total,
                                  subset = test_item.total[,2] < cvpb))

    text(outlier, paste("i", outlier[,1], sep = ""), col = "red", cex = .7)

    return(test_item.total[order(test_item.total$point_biserial),])
  }
