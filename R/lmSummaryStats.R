

#' Linear Model Summary Stats
#'
#' Get the TSS, MSS, and RSS for a linear model.
#'
#' @param lm
#'
#' @returns a dataframe with
#' \itemize{
#'  \item \code{TSS}
#'  \item \code{MSS}
#'  \item \code{RSS}
#' }
#' @export
#'
#' @examples
#' df <- data.frame(
#'   x = rnorm(100, mean = 50, sd = 10),
#'   y = 5 + 2 * rnorm(100, mean = 50, sd = 10) + rnorm(100, mean = 0, sd = 5)
#' )
#'
#' model <- lm(y ~ x, data = df)
#'
#' lmSummaryStats(model)
lmSummaryStats <- function(lm) {
  y_data = lm$model[[1]]
  y_pred = fitted(lm)

  TSS = sum((y_data - mean(y_data)) ^ 2)
  MSS = sum((y_pred - mean(y_data)) ^ 2)
  RSS = sum((y_data - y_pred) ^ 2)

  return(data.frame(
    TSS = TSS,
    MSS = MSS,
    RSS = RSS
  ))
}
