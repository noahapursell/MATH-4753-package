test_that("myncurve works (mu)", {
  output = myncurve(5, 3, 1)
  mu = output$mu
  expect_equal(mu, 5)
})

test_that("myncurve works (sigma)", {
  output = myncurve(5, 3, 1)
  sigma = output$sigma
  expect_equal(sigma, 3)
})

test_that("myncurve works (prob)", {
 output = myncurve(5, 3, 1)
 prob = output$prob
 expect_equal(prob, pnorm(1, 5, 3))
})
