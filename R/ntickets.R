
#' ntickets
#'
#' Determine the number of tickets to sell with a specified success rate, total
#' number of seats, and overbooking chance tolerance.
#'
#' @param N numeric: the total number of seats available
#' @param gamma numeric: the acceptable probability of overbooking
#' @param p numeric: the probability that someone who buys a ticket will actually show up
#'
#' @return A named list with the following components:
#' \describe{
#'   \item{nd}{numeric; the optimal number of tickets determined using the discrete distribution.}
#'   \item{nc}{numeric; the optimal number of tickets determined using the normal approximation.}
#'   \item{N}{numeric; the number of seats available.}
#'   \item{p}{numeric; the probability of a ticket buyer showing up.}
#'   \item{gamma}{numeric; the acceptable probability of overbooking.}
#' }
#' @importFrom stats pbinom pnorm spline uniroot
#' @importFrom graphics plot lines abline curve title
#' @export
#'
#' @examples
#' ntickets(N=100, gamma=0.1, p=0.9)
ntickets = function(N, gamma, p) {
  ns = seq(N, N / p * 1.5)
  f = function(n)
    1 - gamma - pbinom(N, n, prob = p)
  f_of_ns = f(ns)
  min_index = which.min(abs(f_of_ns))
  nd = ns[min_index]
  plot(ns, f_of_ns, main=paste0("Objective Vs n to find optimal tickets sold (", nd, ") gamma=", gamma, " N=", N, " discrete"), xlab="n", ylab="objective")
  curve_data = spline(ns, f_of_ns, n=100)
  lines(curve_data, col="blue", lwd=2)
  abline(v=ns[min_index], lty=2)
  abline(h=0, lty=2)
  fc = function(n) {
    1 - gamma - pnorm(N+0.5, mean=n*p, sd=sqrt(n*p*(1-p)))
  }
  fc_of_ns = fc(ns)
  nc = uniroot(f = fc, interval = c(N, N / p * 2))$root
  curve(fc, from=N, to=N / p * 2, xlab="n", ylab="Objective")
  title(paste0("Objective Vs n to find optimal tickets sold (", nc, ") gamma=", gamma, " N=", N, " continuous"))
  abline(h=0, lty=2)
  abline(v=nc, lty=2)


  list(nd=nd, nc=nc, N=N, p=p, gamma=gamma)
}
