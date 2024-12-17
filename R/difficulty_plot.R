#' @title item_difficulty_visualization
#'
#' @description plotting results of item difficulty analysis from the Classical test theory framework
#'
#' @param responses A dichotomous item response object (a dataframe or a matrix)
#' @param title Title of the plot
#' @param easyFlag threshold of the easy item
#' @param hardFlag threshold of the hard item
#'
#' @return A data frame sorted by item difficulty in ascending order. A plot of item difficulty in relation to the specified threshold.
#' @examples
#'
#' data(dichotomous_response)
#'
#' # To plot item difficulty with easyFlag of .9 and hardFlag of .5
#'
#' difficulty_plot(responses = dichotomous_response,
#'       title = "Item Difficulty Plot", easyFlag = .90, hardFlag = .50)
#'
#' @export
#' @importFrom CTT "itemAnalysis"
#' @importFrom graphics abline text

difficulty_plot <-
  function(responses, title = "Item Difficulty", easyFlag = .90, hardFlag = .50)
  {
    # CRITICAL VALUES
    cveasy = easyFlag
    cvhard = hardFlag

    ctt.analysis <- CTT::itemAnalysis(responses, itemReport=TRUE,
                                      NA.Delete=TRUE, pBisFlag = .20,
                                      bisFlag = .20, flagStyle = c("X",""))

    difficulty_value <- data.frame(item = 1:ctt.analysis$nItem ,
                                   difficulty = ctt.analysis$itemReport$itemMean)

    plot(difficulty_value,
         main = title,  # Use the title parameter here,
         type = "p",
         pch = 4,
         cex = 1.8,
         col = "purple",
         ylab = "Item Difficulty",
         xlab = "Item Number",
         ylim = c(0, 1),
         xlim = c(0, ctt.analysis$nItem))

    abline(h = cveasy, col = "red")
    abline(h = cvhard, col = "red")

    item_names <- colnames(responses)  # Extract the item names

    outlier <- data.matrix(subset(difficulty_value,
                                  subset = difficulty_value[,2] > cveasy | difficulty_value[,2] < cvhard))

    text(outlier, labels = item_names[outlier[,1]], col = "red", cex = 0.7)

    return(difficulty_value[order(difficulty_value$difficulty),])
  }

