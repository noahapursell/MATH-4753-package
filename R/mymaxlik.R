#' mymaxlik
#'
#' Use the grid approximation method to estimate the parameter that maximizes the log-likelihood function "lfun"
#'
#' @param lfun a function that takes in a single sample and a parameter that describes the log-likelihood function.
#' @param x a vector of numbers that are the results of sampling some distribution.
#' @param param a vector of numbers that represents the different parameter values you want to try in the grid approximation method
#' @param ... extra parameters for plotting
#'
#' @return A list with the following components:
#' \describe{
#'   \item{i}{The index corresponding to the maximum likelihood (integer).}
#'   \item{parami}{The parameter value corresponding to the maximum likelihood (numeric).}
#'   \item{yi}{The maximum log-likelihood value (numeric).}
#'   \item{slope}{A numeric vector giving slope estimates around the maximum likelihood.}
#' }
#'
#' @importFrom graphics axis points
#'
#' @export
#'
#' @examples
#' # Define a simple log-likelihood function
#' lfun <- function(x, p) {
#'   dbinom(x, size = 20, prob = p, log = TRUE)
#' }
#'
#' # Example data
#' x <- c(3, 3, 4, 3, 4, 5, 5, 4)
#'
#' # Find the maximum likelihood
#' mymaxlik(lfun, x, seq(from = 0, to = 1, length.out = 100))
mymaxlik = function(lfun, x, param, ...) {
  # how many param values are there?
  np = length(param)
  # outer -- notice the order, x then param
  # this produces a matrix – try outer(1:4,5:10,function(x,y) paste(x,y,sep=" "))   to understand
  z = outer(x, param, lfun) # A
  # z is a matrix where each x,param is replaced with the function evaluated at those values
  y = apply(z, 2, sum)

  # y is a vector made up of the column sums
  # Each y is the log lik for a new parameter value
  plot(param,
       y,
       col = "Blue",
       type = "l",
       lwd = 2,
       ...)
  # which gives the index for the value of y >= max.
  # there could be a max between two values of the parameter, therefore 2 indices
  # the first max will take the larger indice
  i = max(which(y == max(y))) # B
  abline(v = param[i], lwd = 2, col = "Red")

  # plots a nice point where the max lik is
  points(param[i],
         y[i],
         pch = 19,
         cex = 1.5,
         col = "Black")
  axis(3, param[i], round(param[i], 2))
  #check slopes. If it is a max the slope should change sign from + to
  # We should get three + and two -vs
  ifelse(i - 3 >= 1 &
           i + 2 <= np,
         slope <- (y[(i - 2):(i + 2)] - y[(i - 3):(i + 1)]) / (param[(i - 2):(i +
                                                                                2)] - param[(i - 3):(i + 1)]),
         slope <- "NA")
  return(list(
    i = i,
    parami = param[i],
    yi = y[i],
    slope = slope
  ))
}
