#' @title coefficient_alpha_visualization
#'
#' @description plotting results of coefficient alpha analysis from the Classical test theory framework.
#' Items that increase the overall coefficient alpha when dropped will be shown above the overall alpha line.
#' This helps identifying items that could be revised or removed based on its influence to unidimensional coefficient alpha reliability of the test.
#'
#' This function can also be used with polytomous item responses.
#' However, it is recommended for users to perform reverse coding as necessary before implementing this function.
#'
#' @param responses A dichotompus item response object (a dataframe or a matrix)
#' @param title Title of the plot
#' @param alpha_round Rounding option for coefficient alpha. default to 4 decimal points.
#'
#' @return A data frame sorted by coefficient alpha if dropped in ascending order. A plot of coefficient alpha of each item in relation to the overall coefficient alpha.
#' @examples
#'
#' data(reliability_df)
#'
#' # To plot coefficient alpha with the overall alpha rounding of 3 decimal places
#'
#' coefficient_alpha_plot(responses = reliability_df,
#'       title = "Coefficient Alpha Plot", alpha_round = 3)
#'
#' @export
#' @importFrom CTT "itemAnalysis"
#' @importFrom graphics abline text axis

coefficient_alpha_plot <-
  function(responses, title = "Coefficient Alpha", alpha_round = 3)
  {

    # Perform CTT analysis
    ctt.analysis <- CTT::itemAnalysis(responses, itemReport=TRUE,
                                      NA.Delete=TRUE, pBisFlag = .20,
                                      bisFlag = .20, flagStyle = c("X",""))

    # Extract overall alpha and alpha if dropped
    overall_alpha = ctt.analysis$alpha

    test_item.total <- data.frame(item = 1:ctt.analysis$nItem ,
                                  alpha_if_dropped = ctt.analysis$itemReport$alphaIfDeleted)

    plot(test_item.total,
         main = title,  # Use the title parameter here
         type = "p",
         pch = 4,
         cex = 1.8,
         col = "purple",
         ylab = "Coefficient Alpha if Dropped",
         xlab = "Item Number",
         ylim = c(-0.2, 1),
         xlim = c(0, ctt.analysis$nItem))

    abline(h = overall_alpha, col = "red")

    # Add the overall alpha value as text in the top left corner
    text(x = 0.5,  # Near the left edge
         y = max(test_item.total$alpha_if_dropped, overall_alpha) + 0.1,  # Slightly above the plot range
         labels = paste("Overall Alpha =", round(overall_alpha, alpha_round)),
         col = "red",
         cex = 0.9,
         pos = 4)  # Position to the right of the x-coordinate

    item_names <- colnames(responses)  # Extract the item names

    # Identify and label items where dropping improves overall alpha
    outlier <- data.matrix(subset(test_item.total,
                                  subset = test_item.total[,2] > overall_alpha))

    if (nrow(outlier) > 0) {  # Only add text if there are outliers
      text(outlier, labels = item_names[outlier[,1]], col = "red", cex = 0.7)
    }

    return(test_item.total[order(test_item.total$alpha_if_dropped),])
  }
