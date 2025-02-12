#' mybin
#'
#' Create a plot and return a table of the probabilities of getting i successes
#'  out of n trials with probability p of success, for all values of i from 0 to n.
#'
#' @param iter Number of iterations to run the sampling.
#' @param n Number of trials per sample.
#' @param p Probability that a single event is successful.
#'
#' @returns A numeric vector with the probabilities of having i successes, for all values
#' of i from 0 to n.
#' @export
#'
#' @examples
#' mybin(100, 10, 0.8)
mybin = function(iter = 100,
                 n = 10,
                 p = 0.5) {
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat = matrix(NA,
                   nr = n,
                   nc = iter,
                   byrow = TRUE)
  #Make a vector to hold the number of successes in each trial
  succ = c()
  for (i in 1:iter) {
    #Fill each column with a new sample
    sam.mat[, i] = sample(c(1, 0), n, replace = TRUE, prob = c(p, 1 - p))
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i] = sum(sam.mat[, i])
  }
  #Make a table of successes
  succ.tab = table(factor(succ, levels = 0:n))
  #Make a barplot of the proportions
  barplot(
    succ.tab / (iter),
    col = rainbow(n + 1),
    main = "Binomial simulation",
    xlab = "Number of successes"
  )
  succ.tab / iter
}
