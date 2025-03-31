#' myboot2
#'
#' Use bootstrapping to find a confidence interval for a statistic using a random sample.
#'
#' @param iter number of iterations of bootstrapping
#' @param x random sample
#' @param fun the function describing the statistic to measure
#' @param alpha the significance level
#' @param ... extra parameters for the histogram
#'
#' @returns A list containing:
#' \describe{
#'   \item{ci}{Numeric vector of length 2 with the lower and upper bounds of the confidence interval.}
#'   \item{fun}{The function used to compute the statistic (e.g., `"mean"`).}
#'   \item{x}{The original data sample used for bootstrapping.}
#'   \item{xstat}{A numeric vector of bootstrap sample statistics, one for each iteration.}
#' }
#'
#' @importFrom graphics segments
#' @importFrom stats quantile
#'
#' @export
#'
#' @examples
#' myboot2(iter=1000, x=runif(20, 1, 10), fun="mean", alpha=0.05)
myboot2<-function(iter=10000,x, fun = "mean", alpha = 0.05, ...) {
  #Notice where the ... is repeated in the code
  n = length(x)   #sample size

  #Now sample with replacement
  y = sample(x, n * iter, replace = TRUE) #A

  # Make a matrix with all the resampled values
  rs.mat = matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  xstat = apply(rs.mat, 2, fun)
  # xstat is a vector and will have iter values in it
  ci = quantile(xstat, c(alpha / 2, 1 - alpha / 2)) #B
  # Nice way to form a confidence interval
  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para = hist(xstat,
              freq = FALSE,
              las = 1,
              main = "Histogram of Bootstrap sample statistics",
              ...)

  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat = matrix(x,
               nrow = length(x),
               ncol = 1,
               byrow = TRUE)

  #pte is the point estimate
  #This uses whatever fun is
  pte = apply(mat, 2, fun)
  abline(v = pte, lwd = 3, col = "Black")# Vertical line
  segments(ci[1], 0, ci[2], 0, lwd = 4)      #Make the segment for the ci
  text(ci[1],
       0,
       paste("(", round(ci[1], 2), sep = ""),
       col = "Red",
       cex = 3)
  text(ci[2],
       0,
       paste(round(ci[2], 2), ")", sep = ""),
       col = "Red",
       cex = 3)

  # plot the point estimate 1/2 way up the density
  text(pte, max(para$density) / 2, round(pte, 2), cex = 3)

  return(list(ci = ci, fun = fun, x = x, xstat = xstat))# Some output to use if necessary
}
