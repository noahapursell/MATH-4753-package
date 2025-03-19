#' My Normal Curve
#'
#' plot a normal curve and calculate the probability of a sample being less than
#' the given value `a`
#'
#' @param mu normal distribution mean
#' @param sigma normal distribution standard deviation
#' @param a threshold
#'
#' @return list containing these elements
#' \describe{
#'   \item{\code{mu}}{The normal distribution mean.}
#'   \item{\code{sigma}}{The normal distribution standard deviation.}
#'   \item{\code{prob}}{The probability of a sample being less than `a`.}
#' }
#'
#' @importFrom stats dnorm
#' @importFrom graphics polygon text
#' @export
#'
#' @examples
#' myncurve(10, 4, 6)
myncurve = function(mu, sigma, a){
  f = function(x) dnorm(x,mean=mu,sd=sigma)
  curve(f, xlim = c(mu-4*sigma, mu + 3*sigma), xlab="x", ylab="P(x)")
  xSeq = seq(mu-3*sigma, a, length=100 * (a - (mu-3*sigma)))
  polyXs = c(xSeq[1], xSeq, xSeq[length(xSeq)])
  polyYs = c(0, dnorm(xSeq, mean=mu, sd=sigma), 0)
  polygon(x=polyXs, y=polyYs, col="cyan")
  prob = pnorm(a, mean=mu, sd=sigma)
  dProb = round(prob, 4)
  text(x=mean(c(a, mu-3*sigma)), y=0.5 * dnorm(x=mean(c(a, mu-3*sigma)), mean=mu, sd=sigma), paste0("Probability: ", dProb))
  list(mu = mu, sigma = sigma, prob=prob)
}
